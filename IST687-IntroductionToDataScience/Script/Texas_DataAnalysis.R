#
dev.off()
cat('\014')
rm(list=ls())

#setting working directory
setwd("~/Desktop/IST687/Project")

library(RCurl)
library(jsonlite)
library(tidyverse)
library(ggplot2)
library(ggmap)  
library(maps)
library(mapproj)
library(arules)
library(arulesViz)

#copying the json data into airlines dataframe
airlines <- jsonlite::fromJSON("fall2019-survey-M02(1).json")
View(airlines)

#grouping the dataframe by origin state
originStateProp <- airlines %>%
  group_by(Origin.State) %>%
  summarise(Detractors=length(Likelihood.to.recommend[Likelihood.to.recommend<7])/length(Likelihood.to.recommend), 
            Passive=length(Likelihood.to.recommend[Likelihood.to.recommend==7 | Likelihood.to.recommend==8])/length(Likelihood.to.recommend),
            Promoters=length(Likelihood.to.recommend[Likelihood.to.recommend>8])/length(Likelihood.to.recommend),
            Total=length(Likelihood.to.recommend)) %>%
  arrange(-Detractors)

originStateProp$Origin.State <- tolower(originStateProp$Origin.State)
View(originStateProp)

#creating a map for proportion of detractors by origin state
us <- map_data("state")

map1 <- originStateProp %>%
  ggplot()+
  aes(map_id = Origin.State) +
  geom_map(map = us) +
  aes(fill = Detractors) +
  expand_limits(x = us$long, y = us$lat) +
  coord_map() +
  ggtitle("Origin states with proportion of detractors")
map1

#subsetting texas state data based on origin state
texas <- airlines %>%
  filter(Origin.State == "Texas")
View(texas)

col <- c("Destination.City", "Origin.City", "Age", "Year.of.First.Flight", "Loyalty", "Total.Freq.Flyer.Accts",
         "Flights.Per.Year", "Shopping.Amount.at.Airport", "Eating.and.Drinking.at.Airport", "Day.of.Month", 
         "Flight.date", "Partner.Name", "Origin.State", "Destination.State", "Scheduled.Departure.Hour", 
         "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes", "Flight.time.in.minutes", "Flight.Distance", 
         "olong", "olat", "dlong", "dlat", "freeText")

texas <- texas[,(!colnames(texas) %in% col), drop =FALSE]

#binning likelihood to recommend and price sensitivity
texas$Likelihood.to.recommend[texas$Likelihood.to.recommend>8] <- "promoter"
texas$Likelihood.to.recommend[texas$Likelihood.to.recommend<7] <- "detractor"
texas$Likelihood.to.recommend[texas$Likelihood.to.recommend==7 | texas$Likelihood.to.recommend==8] <- "passive"

texas$Price.Sensitivity[texas$Price.Sensitivity==0 | texas$Price.Sensitivity==1] <- "low"
texas$Price.Sensitivity[texas$Price.Sensitivity==2 | texas$Price.Sensitivity==3] <- "moderate"
texas$Price.Sensitivity[texas$Price.Sensitivity==4 | texas$Price.Sensitivity==5] <- "high"

#creating transactions for texas dataset
tx <- as(texas, "transactions")

inspect(tx)

itemFrequency(tx)

#generating association rules for detractors based on 0.5% support and 90% confidence
ruleset <- apriori(tx, parameter = list(support = 0.005, confidence = 0.90), 
                   appearance = list(default = "lhs", rhs = ("Likelihood.to.recommend=detractor")))

#inspecting the ruleset 
inspectDT(ruleset)
