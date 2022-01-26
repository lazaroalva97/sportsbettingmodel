#Building Model 

library(readr)
library(dplyr)
library(tidyverse)
#Load Data Needed 
Stats <- read_csv("data/Cleaned Data/Final Standings Table/Final_Standing_2021.csv", 
                  col_types = cols(...1 = col_skip()))
Boxscores <- read_csv("data/Cleaned Data/Boxscores/Boxscores_2021.csv", 
                      col_types = cols(...1 = col_skip()))

#Create Margin
Boxscores <- Boxscores %>% 
  mutate(Margin = Vis_Score - Home_Score)


#Join and ASRS & HSRS 
Away_SRS <- Stats %>% select(Tm, SRS) 
names(Away_SRS)[1] <- paste("Visitor")
names(Away_SRS)[2] <- paste("ASRS")

Home_SRS <- Stats %>% select(Tm, SRS) 
names(Home_SRS)[1] <- paste("Home")
names(Home_SRS)[2] <- paste("HSRS")


Boxscores <- left_join(Boxscores, Away_SRS, by = "Visitor")
Boxscores <- left_join(Boxscores, Home_SRS, by = "Home")

#Margin here is the dependent variable - what we are trying to measure based off our stats
#How is the final score margin is influenced by our stats - 
#Stats are  the independent variables - not influenced by everything 

#Basic Linear Regression 
model <- lm(Margin ~ ASRS + HSRS, data=Boxscores)
summary(model)

plot(model)
#About 34% of the margin could be explained by SRS 
#intercept is where the line starts 
#In sports gamble modeling the intercept is always the home advantage.
#In this case the away team had a -1.75 (typically football is -3 or a field goal)

#lets do an example - a matchup between the Dal Cowboys @ TB Buccanears
# Cowboys ASRS is 9.9 and the Chargers HSRS is 1.1

#Equation should be INT + ASRS * Cowboys SRS 
-1.7456 + (9.9*1.1165) + (8.6*-0.8838) 
#The sum of this equation is the predicted margin of this matchup
#1.70707 Cowboys would be favored by 1.7 points 

#Now if the oddsboards have the Cowboys as -3 (favored by -3), 
#but our model says the cowboys should be favored by -1.7
#According to our model - the Buccs have an edge of 1.3 points
#There is 1.3 points of value on the Bucs at +3
#Assuming our model is right - we would be the Bucs to cover that spread.

#Now what if want to add more stats
#Now we have these additional stats - SOS, OSRS and DSRS


#Join and Away SOS, OSRS, & DSRS & Home SOS, OSRS, & DSRS
Away_Stats2 <- Stats %>% select(Tm, SoS, OSRS, DSRS) 
names(Away_Stats2)[1] <- paste("Visitor")
names(Away_Stats2)[2] <- paste("V_SOS")
names(Away_Stats2)[3] <- paste("V_OSRS")
names(Away_Stats2)[4] <- paste("V_DSRS")

Home_Stats2 <- Stats %>% select(Tm, SoS, OSRS, DSRS) 
names(Home_Stats2)[1] <- paste("Home")
names(Home_Stats2)[2] <- paste("H_SOS")
names(Home_Stats2)[3] <- paste("H_OSRS")
names(Home_Stats2)[4] <- paste("H_DSRS")

Boxscores <- left_join(Boxscores, Away_Stats2, by = "Visitor")
Boxscores <- left_join(Boxscores, Home_Stats2, by = "Home")

#Basic Linear Regression 
Boxscores <- as.data.frame(Boxscores)

model2 <- lm(Margin ~ 
              ASRS + HSRS + 
              V_SOS + H_SOS + 
              V_OSRS + H_OSRS + 
              V_DSRS + H_DSRS,
            data=Boxscores)
summary(model2)
#This model has a lower R Squared that worse - stats that we picked are not predictive

#Use the same example 
