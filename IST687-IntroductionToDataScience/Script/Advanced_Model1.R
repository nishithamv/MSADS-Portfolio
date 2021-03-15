# Run these three functions to get a clean test of homework code
dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!


setwd("~/Desktop/Intro to Data Science")

#Importing data
library(jsonlite)
data <- fromJSON('fall2019-survey-M02(1).json')
View(data)

#Librarying the necessary packages 
library(arules)
library(arulesViz)
library(ggplot2)
library(DT)

#Excluding columns that seem intuitively irrelevent to the model approach 

data$freeText <- NULL
data[,c(7,16,17,23,28,29,30,31)] <- NULL

#Taking a subset of three airline partners who have the highest number of instances 
data_top3 <- subset(data, data$Partner.Name == 'Cheapseats Airlines Inc.' | data$Partner.Name == 'Sigma Airlines Inc.' | data$Partner.Name == 'Northwest Business Airlines Inc.')
View(data_top3)

#Understanding the distribution of the target column in the data subset 
barplot(prop.table(table(data_top3$Likelihood.to.recommend)), col = 'blue')

#replacing the na values

summary(data_top3)

#There are NA values in Departure delay and flight time in minutes columns. After looking closely, the values are missing becasue the flight is cancelled. Hence replacing NAs with zeroes.
data_top3[is.na(data_top3$Departure.Delay.in.Minutes),19] <- 0
data_top3[is.na(data_top3$Flight.time.in.minutes),21] <- 0

#There is 1 NA  in the liklihood to recommend. Replacing it with the mean value of liklihood to recommend. 
round(mean(data_top3$Likelihood.to.recommend, na.rm = T))
data_top3[is.na(data_top3$Likelihood.to.recommend),23] <- round(mean(data_top3$Likelihood.to.recommend, na.rm = T))

#Chekcing to see if the NA value is gone 
summary(data_top3)

#binning the liklihood to recommend variable to <8 and >=8 bins where 0(detractors/passive) : <8, 1(Promoters) : >=8

data_top3[data_top3$Likelihood.to.recommend < 8,23] <- 0
data_top3[data_top3$Likelihood.to.recommend >= 8,23] <- 1

#DIstribution of Liklihhod to recommend after binning
barplot(prop.table(table(data_top3$Likelihood.to.recommend)),col = 'blue')

#46% of the customers have given rating less than 8 which is a cause of concern 

#What are the common characteristics of these customers who have given a bad rating?

#Let us first understand if there is any correlation between the numeric variables and the target variable 

View(data_top3)

#State wise counts of detractors and promoters
table(data_top3$Destination.State, data_top3$Likelihood.to.recommend)

#Deleting some columns 
data_top3$Destination.City <- NULL
data_top3$Origin.City <- NULL

#Airline status wise counts on detractors and promoters 
table(data_top3$Airline.Status, data_top3$Likelihood.to.recommend)

#Blue airline status people giving bad ratings most 

table(data_top3$Age, data_top3$Likelihood.to.recommend)
#Binning the age variable 
data_top3[data_top3$Age >= 15 & data_top3$Age <35,2] <- 'young'
data_top3[data_top3$Age >= 35 & data_top3$Age < 60,2] <- 'middle aged'
data_top3[data_top3$Age >= 60 & data_top3$Age <= 85,2] <- 'old'
data_top3$Age <- as.factor(data_top3$Age)

#Age wise counts of detractors and promoters 
prop.table(table(data_top3$Age, data_top3$Likelihood.to.recommend))

#people wit age >60  are giving bad ratings 

#Gender wise counts of detractors and promoters
table(data_top3$Gender, data_top3$Likelihood.to.recommend)
#Females give more bad ratings 

#Updating the data types of some of the columns 
data_top3$Likelihood.to.recommend <- as.factor(data_top3$Likelihood.to.recommend)
data_top3$Gender<- as.factor(data_top3$Gender)
data_top3$Price.Sensitivity<- as.factor(data_top3$Price.Sensitivity)


#Binning the flights per year column into 3 categories viz. '0-30','30-60' and '60-100'
data_top3[data_top3$Flights.Per.Year >=0 & data_top3$Flights.Per.Year<=30,5] <- '0-30'
data_top3[data_top3$Flights.Per.Year >30 & data_top3$Flights.Per.Year<=60,5] <- '30-60'
data_top3[data_top3$Flights.Per.Year >60 & data_top3$Flights.Per.Year<=95,5] <- '60-100'

#FLights per year wise counts of detractors and promoters
table(data_top3$Flights.Per.Year, data_top3$Likelihood.to.recommend)

data_top3$Flights.Per.Year <- as.factor(data_top3$Flights.Per.Year)

#Binning the loyalty column with 0.2 interval
data_top3$Loyalty1<-cut(data_top3$Loyalty, c(-1,-.8,-.6,-.4,-.2,0,0.2,0.4,0.6,0.8,1))
data_top3$Type.of.Travel <- as.factor(data_top3$Type.of.Travel)
data_top3$Total.Freq.Flyer.Accts <- as.factor(data_top3$Total.Freq.Flyer.Accts)


#Binning the SHopping amount and Eating and drinking amounts at airport variables with 10 and 20 intervals respectively.
data_top3$shopping<-cut(data_top3$Shopping.Amount.at.Airport, seq(-1,600,10))
data_top3$eating<-cut(data_top3$Eating.and.Drinking.at.Airport, seq(-1,650,20))

#Updating data types to factor of columns 
data_top3$Class <- as.factor(data_top3$Class)
data_top3$Day.of.Month <- as.factor(data_top3$Day.of.Month)
data_top3$Partner.Name <- as.factor(data_top3$Partner.Name)
data_top3$Origin.State<- as.factor(data_top3$Origin.State)
data_top3$Destination.State <- as.factor(data_top3$Destination.State)
data_top3$Scheduled.Departure.Hour <- as.factor(data_top3$Scheduled.Departure.Hour)
data_top3$Flight.cancelled <- as.factor(data_top3$Flight.cancelled)

#Binning and factorising the variable departure delay in minutes with 20 minutes interval 
data_top3$departure_delay_min<-cut(data_top3$Departure.Delay.in.Minutes, seq(-1,600,20))
data_top3$Flight.cancelled <- as.factor(data_top3$Flight.cancelled)

#binning FLight time in minutes and and flight distance columns 
data_top3$flight_time<-cut(data_top3$Flight.time.in.minutes, seq(-1,400,100))
data_top3$flight_dist<-cut(data_top3$Flight.Distance, seq(-1,3000,500))

str(data_top3)

data_top3$Airline.Status <- as.factor(data_top3$Airline.Status)

#Filtering out duplicate columns from the data set 
data_top3v2 <- data_top3[,-c(6,9,10,16,17,19,20)]

str(data_top3v2)

View(data_top3v2)

data_top3v2$Flight.cancelled <- as.factor(data_top3v2$Flight.cancelled)

#creating transaction matrix from the final cleaned data set 
my_data <- as(data_top3v2,'transactions')

#Exploring the transaction matrix 
head(inspect(my_data))

item_freq <- itemFrequency(my_data)

#Visualising the item frequencies 
itemFrequencyPlot(my_data, topN = 15, popCol = 'red')

#Building rules using apriori algorithm on my_data with support 0.1, confidence 0.5 and maxlength of rules as 20
#Here, RHS is detractors. meaning the rules will specify what characteristics define detractors 
ruleset <- apriori(my_data, parameter = list(support = 0.1,confidence = 0.5, maxlen = 20), appearance = list(default = 'lhs', rhs = ("Likelihood.to.recommend=0")))

inspect(ruleset)

#After inspecting the ruleset, we get 422 rules. which are too many. 
plot(ruleset)

#Lets focus only on the good rules with lift > 1.8
goodrules <- ruleset[quality(ruleset)$lift > 1.8]
inspect(goodrules)

rules_dt <- data.table( lhs = labels( lhs(goodrules) ), 
                        rhs = labels( rhs(goodrules) ), 
                        quality(goodrules) )[ order(-lift), ]

datatable(rules_dt)
#There are 86 good rules which are ready to be analysed 

#Building rules using apriori algorithm on my_data with support 0.1, confidence 0.5 and maxlength of rules as 20
#Here, RHS is PROMOTERS. meaning the rules will specify what characteristics define PROMOTERS
ruleset1 <- apriori(my_data, parameter = list(support = 0.1,confidence = 0.5, maxlen = 20), appearance = list(default = 'lhs', rhs = ("Likelihood.to.recommend=1")))
ruleset1
inspect(ruleset1)

plot(ruleset1)

#Using lift > 1.4 to get good rules for Promoters 
goodrules1 <- ruleset1[quality(ruleset1)$lift > 1.4]
inspect(goodrules1)

inspectDT(goodrules1)
#188 good rules are created to analyse for promoters 


#########################

#Focusing on cheapseats airline and using the variables that occur most in the rules generated above, let us create a subset 
#of the data and try to build a classification model using SVM algorithm

#The variables included in this data set are carefully selected from the rules generated for both promoters and detractors.
#These variables occur most in the rules and hence must contribute to the data model. Other variables are eliminated to reduce the dimensions of the dataset.

#1. Type of travel 
# 2. Airline Status
# 3. Gender 
# 4. Class
# 5. Flight Cancelled
# 6. Departure Delay
# 7. Flight Time 
# 8. Total Freq Flyeer Acc
# 9. Age
# 10. Flight Time 
# 11. Flight Dist 
# 12. Shopping
# 13. Flights per year 

View(data_top3v2)
summary(data_top3v2)
cheapseats <- subset(data_top3v2, Partner.Name == 'Cheapseats Airlines Inc.')

View(cheapseats)
summary(cheapseats)

#Excluding irrelevent columns 
cheapseats <- cheapseats[,-c(9,10,11,12,15,17)]
View(cheapseats)

#Using one hot encoding to convert the categorical data set into numerical 
target <- cheapseats[,10]
cheapseats <- cheapseats[,-10]

#Librarying Mltools package which contains one_hot function 
library(mltools)
library(data.table)

cheapseatsv1 <- one_hot(as.data.table(cheapseats))

View(cheapseatsv1)

#Binding the target column again with the data set 
cheapseatsv1 <- cbind(cheapseatsv1,target)

colnames(cheapseatsv1)

#Removing some binning categories that do not appear in the rules 
cheapseatsv1 <- cheapseatsv1[,-c(39:93)]

str(cheapseatsv1)
sum(is.na(cheapseatsv1))
#Train test split 
#Training - 70%
#testing 30%

library(caret)
index <- createDataPartition(cheapseatsv1$target, p= 0.7, list = F)
training <- cheapseatsv1[index,]
testing <- cheapseatsv1[-index,]


#Tuning the model to get the optimum cost level 
library(e1071)
library(kernlab)
tune <- tune.svm(target~., data=training, cost=c(0.001, 0.01, 0.1, 1,5,10))
summary(tune)

#Input the parameters to the model 
svmOutput <- ksvm(target~., data = training, kernel = 'rbfdot', kpar= 'automatic', C=0.1, cross = 3, prob.model= TRUE)

#Output of the model 
svmOutput

#Predicting on test data 
svmPred <- predict(svmOutput,testing)

#confusion matrix to see accracy 
confusionMatrix(svmPred,testing$target)




