
####*0.Data preparation*####
#This process is used to import data and establish basic assumptions

#Required package
library(dplyr);library(openxlsx);library(ggplot2);library(tidyverse)


###(1)imported data####
#bicycle data
biked <- read.csv("./data/bikedata1.csv",stringsAsFactors = F)
names(biked)

#pollution data
#We have pre-processed the air pollutant concentration data for weekly
podata <- read.csv("./data/poldata.csv",stringsAsFactors = F)
bikepo <- left_join(biked,podata,by=c("area","week"))



###(2)Basic assumptions####

###--Distance--###
#According to the condition that the commuting distance is constant,
#the user's original commuting mode that before dockless bikes appeared was assumed:

##A.We classify users who ride less than 1km in a single ride into walking mode(milew)
##B.We classify users who ride more than 3km in a single ride into driving mode(milec)
##C.Users with other distances in a single ride were classified into riding mode(mileb)
bikepo <- mutate(bikepo,
                 milew=(wts*onemilr0.5*0.25)+
                   (wts*onemilr1*0.75),
                 milec=(wts*onemilr5.*6)+
                   (wts*onemilr5*4.75)+(wts*onemilr4.5*4.25)+
                   (wts*onemilr4*3.75)+(wts*onemilr3.5*3.25),
                 mileb=mil-milew-milec)

#We assume that the average distance of 0.25km for those proportion of ride distance less than 0.5km,
#and 0.75km for 0.5-1km, and so on
#Since we don’t have a record of every ride
#Unit: 10 thousand due to the unit in wts is 10 thousand

###--User number--###
##The assumption here is the number of users walking, biking, and driving
#before the dockless bike per week

#Combined with the results of the sample survey, the assumption were based on the average daily cycling mileage,
#Users in 0-3km/day was from walking, 3-5km/day biking and driving above 5km/day
#Formula：Total user * proportion of daily cycling mileage in 3 level 
bikepo$acw <- (bikepo$dmilr1+bikepo$dmilr13)*bikepo$wau*10000  #walking user
bikepo$acb <- bikepo$dmilr35*bikepo$wau*10000  #biking user
bikepo$acc <- (bikepo$dmilr57+bikepo$dmilr710+bikepo$dmilr10)*bikepo$wau*10000  #driving user
