library(jsonlite)
library(RCurl)
dataset <- getURL("https://s3.us-east-1.amazonaws.com/blackboard.learn.xythos.prod/5956621d575cd/8614406?response-content-disposition=inline%3B%20filename%2A%3DUTF-8%27%27fall2019-survey-M02%25281%2529.json&response-content-type=application%2Fjson&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20191210T213442Z&X-Amz-SignedHeaders=host&X-Amz-Expires=21600&X-Amz-Credential=AKIAIL7WQYDOOHAZJGWQ%2F20191210%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=daed6d684bdc240eb7c8dbef69de8eceffa76659d08e7458b06f87352e2fabf8")
df <- jsonlite :: fromJSON(dataset)
View(df)
install.packages("tidyverse")
##########################
library(tidyverse)
library(RCurl)
library(jsonlite)
library(imputeTS)
library(ggplot2)
library(ggmap)
library(arules)
library(arulesViz)
library(tm)
library(tidyverse) 
library(scales) 
library(stringr)
library(ggthemes)



df_complete <- df[complete.cases(df[,-32]),]
View(df_complete)

na_values <- apply(apply(df_nps, 2, is.na), 2, which)
na_values
View(na_values)

df_complete$type_passenger <- cut(df_complete$Likelihood.to.recommend,
                                  breaks = c(-1,7,9,Inf),
                                  labels = c("detractor","passive","promoter"))

df_complete$type_passenger <- factor(df_complete$type_passenger, ordered = TRUE)
df_complete$type_passenger[which(is.na(df_complete$type_passenger))] = "passive"

# Create Arrival delay > 5 boolean
df_complete$Arrival_Delay_greater_than_5 <- (df_complete$Arrival.Delay.in.Minutes > 5)
str(df_complete$Arrival_Delay_greater_than_5)

# Create Departure delay > 5 boolean
df_complete$Departure_Delay_greater_than_5 <- (df_complete$Departure.Delay.in.Minutes > 5)
str(df_complete$Departure_Delay_greater_than_5)

# Create Long_flight_time
quantile(df_complete$Flight.time.in.minutes)
df_complete$Long_flight_time <- df_complete$Flight.time.in.minutes > 144


# Create Long trip > 1100 boolean
quantile(df_complete$Flight.Distance)
df_complete$Long_Trip <- df_complete$Flight.Distance > 1100

# Create Flight_time
df_complete$flight_day_part <- cut(df_complete$Scheduled.Departure.Hour,
                                       breaks = c(-1,6,12,17,19,Inf),
                                       labels = c("late night","morning","afternoon","evening","night"))


df_complete$Gender <- factor(df_complete$Gender, labels = c("Female","Male"))
df_complete$Airline.Status <- factor(df_complete$Airline.Status, labels = c("Blue", "Gold", "Platinum", "Silver"))
df_complete$Type.of.Travel <- factor(df_complete$Type.of.Travel, labels = c("Business travel", "Mileage tickets", "Personal Travel"))
df_complete$Class <- factor(df_complete$Class, labels = c("Business", "Eco", "Eco Plus"))
df_complete$Partner.Code <- factor(df_complete$Partner.Code, labels = unique(df_complete$Partner.Code)[order(unique(df_complete$Partner.Code))])
df_complete$Partner.Name<- factor(df_complete$Partner.Name, labels = unique(df_complete$Partner.Name)[order(unique(df_complete$Partner.Name))])
df_complete$Origin.City<- factor(df_complete$Origin.City, labels = unique(df_complete$Origin.City)[order(unique(df_complete$Origin.City))])
df_complete$Origin.State<- factor(df_complete$Origin.State, labels = unique(df_complete$Origin.State)[order(unique(df_complete$Origin.State))])
df_complete$Destination.City<- factor(df_complete$Destination.City, labels = unique(df_complete$Destination.City)[order(unique(df_complete$Destination.City))])
df_complete$Destination.State<- factor(df_complete$Destination.State, labels = unique(df_complete$Destination.State)[order(unique(df_complete$Destination.State))])

#1.Class distribution

findiff.tb <- table(df_complete$Class)
prop.table(findiff.tb)

# Save the proportion table in an object 
findiff.ptb <- prop.table(findiff.tb)

# Convert the proportion table to a data frame
findiff.df <- as.data.frame(findiff.ptb)

# Converting to a data frame lost the names
names(findiff.df) <- c("Class", "Frequency")
findiff.df

ggplot(data=findiff.df, mapping=aes(x=Class, y=Frequency)) + 
  geom_col(fill="darkgrey", alpha=0.5) +
  scale_y_continuous(label=percent) +
  theme_classic()+
  labs(title="Distribution of classes by per cent",x="", y="") + 
  theme(title=element_text(size=14), axis.text=element_text(size=12)) +
  
  geom_text(mapping=aes(x=Class, y=Frequency,
                        label=percent(Frequency)), size=3.5, nudge_y=0.03) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#
#2. Detractors proportion
findiff2.tb <- table(df_complete$type_passenger)
prop.table(findiff2.tb)

# Save the proportion table in an object 
findiff2.ptb <- prop.table(findiff2.tb)

# Convert the proportion table to a data frame
findiff2.df <- as.data.frame(findiff2.ptb)

# Converting to a data frame lost the names
names(findiff2.df) <- c("Type", "Frequency")
findiff2.df

ggplot(data=findiff2.df, mapping=aes(x=Type, y=Frequency)) + 
  geom_col(fill="darkgrey", alpha=0.5) +
  scale_y_continuous(label=percent) +
  theme_classic()+
  labs(title="Distribution of types of passengers by per cent",x="", y="") + 
  theme(title=element_text(size=14), axis.text=element_text(size=12)) +
  
  geom_text(mapping=aes(x=Type, y=Frequency,
                        label=percent(Frequency)), size=3.5, nudge_y=0.03) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#



#3. Type of passenger distribution by age


boxplot(df_complete$Age ~ df_complete$type_passenger)+labs(title="Distribution of type of passenge with age",x="", y="")

#4. Jitter plot for price.sensitivity vs type of passenger 
df_complete %>%
  select(Price.Sensitivity, type_passenger) %>%
  group_by(type_passenger) %>%
  ggplot(aes(x = Price.Sensitivity, y = type_passenger))+
  geom_jitter(alpha = 0.4, color = 'blue')+
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"))

#5. Type of passenger distribution by gender

df_complete %>%
  select(Gender, type_passenger) %>%
  group_by(type_passenger) %>%
  ggplot(aes(x = Gender, y = type_passenger))+
  geom_jitter(alpha = 0.4, color = 'orange')+
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid")
  )

#6.  type_passenger vs Type.of.Travel

findiff3.tb <- table(df_complete$Type.of.Travel)
prop.table(findiff3.tb)

# Save the proportion table in an object 
findiff3.ptb <- prop.table(findiff3.tb)

# Convert the proportion table to a data frame
findiff3.df <- as.data.frame(findiff3.ptb)

# Converting to a data frame lost the names
names(findiff3.df) <- c("Travel", "Frequency")
findiff3.df

ggplot(data=findiff3.df, mapping=aes(x=Travel, y=Frequency)) + 
  geom_col(fill="darkgrey", alpha=0.5) +
  scale_y_continuous(label=percent) +
  theme_classic()+
  labs(title="Distribution of types of travel by per cent",x="", y="") + 
  theme(title=element_text(size=14), axis.text=element_text(size=12)) +
  
  geom_text(mapping=aes(x=Travel, y=Frequency,
                        label=percent(Frequency)), size=3.5, nudge_y=0.03) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#
#7. proportion of detractors in type of travel

df_complete %>%
  select(Type.of.Travel, type_passenger) %>%
  group_by(type_passenger) %>%
  ggplot(aes(x = Type.of.Travel, y = type_passenger))+
  geom_jitter(alpha = 0.4, color = 'pink')+
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid")
  )


#8.  type_passenger vs Status

findiff4.tb <- table(df_complete$Airline.Status)
prop.table(findiff4.tb)

# Save the proportion table in an object 
findiff4.ptb <- prop.table(findiff4.tb)

# Convert the proportion table to a data frame
findiff4.df <- as.data.frame(findiff4.ptb)

# Converting to a data frame lost the names
names(findiff4.df) <- c("Status", "Frequency")
findiff4.df

ggplot(data=findiff4.df, mapping=aes(x=Status, y=Frequency)) + 
  geom_col(fill="darkgrey", alpha=0.5) +
  scale_y_continuous(label=percent) +
  theme_classic()+
  labs(title="Distribution of Airline statuses by per cent",x="", y="") + 
  theme(title=element_text(size=14), axis.text=element_text(size=12)) +
  
  geom_text(mapping=aes(x=Status, y=Frequency,
                        label=percent(Frequency)), size=3.5, nudge_y=0.03) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))










#9. type_passenger vs Airline.Status

df_complete %>%
  select(Airline.Status, type_passenger) %>%
  group_by(type_passenger) %>%
  ggplot(aes(x = Airline.Status, y = type_passenger))+
  geom_jitter(alpha = 0.4, color = 'green')+
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"))


#10. airline partners frequency
findiff5.tb <- table(df_complete$Partner.Name)
prop.table(findiff5.tb)

# Save the proportion table in an object 
findiff5.ptb <- prop.table(findiff5.tb)

# Convert the proportion table to a data frame
findiff5.df <- as.data.frame(findiff5.ptb)

# Converting to a data frame lost the names
names(findiff5.df) <- c("Airline.Partner", "Frequency")
findiff5.df
  ggplot(data=findiff5.df, mapping=aes(x=Airline.Partner, y=Frequency)) + 
  geom_col(fill="darkgrey", alpha=0.5) +
  scale_y_continuous(label=percent) +
  theme_classic()+
  labs(title="Distribution of types of travel by per cent",x="", y="") + 
  theme(title=element_text(size=14), axis.text=element_text(size=12)) +
  
  geom_text(mapping=aes(x=Airline.Partner, y=Frequency,
                        label=percent(Frequency)), size=3, nudge_y=0.03) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


#11. type_passenger vs Class

df_complete %>%
  select(Class, type_passenger) %>%
  group_by(type_passenger) %>%
  ggplot(aes(x = Class, y = type_passenger))+
  geom_jitter(alpha = 0.4, color = 'purple')+
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid")
  )


df_complete %>%
  select(Class, type_passenger) %>%
  group_by(type_passenger) %>%
  ggplot(aes(x = Class, y = type_passenger))+
  geom_jitter(alpha = 0.4, color = 'purple')+
  theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid")
  )

#12. Type of passenger distribution by airline

ggplot(data=df_complete, mapping=aes(x=df_complete$Partner.Name, y=count(df_complete$Partner.Name))) + 
  geom_col(fill="darkgrey", alpha=0.5) +
  scale_y_continuous(label=percent) +
  theme_classic()+
  labs(title="Distribution of airline partners by count of flights",x="", y="") + 
  theme(title=element_text(size=14), axis.text=element_text(size=12)) +
  
  geom_text(mapping=aes(x=Airline.Partner, y=Frequency,
                        label=percent(Frequency)), size=3, nudge_y=0.03) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

ggplot(df_complete, aes(count(Partner.Name))+
  geom_histogram(aes(fill = type_passenger), bins = 10, color = 'Black')
