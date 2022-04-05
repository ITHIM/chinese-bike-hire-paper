rm(list=ls()) 
#The change of physical activity in travel mode change
####***********************Calculation of mean travel MET-hours**************************####
####*0.Data preparation*####
#This process is used to import data and establish basic assumptions
# getwd()
# setwd("E:/Rwork/code to James/changed/no bus")

#Required package
library(dplyr);library(openxlsx);library(ggplot2);library(tidyverse);library(rio);library(dbplyr)
###(1)imported data####
#bicycle data
biked <- read.csv("./data/bikedata210415.csv",stringsAsFactors = F)
names(biked)
#pollution data
#We have pre-processed the air pollutant concentration data for weekly
podata <- read.csv("./data/poldata.csv",stringsAsFactors = F)
bikepo <- left_join(biked,podata,by=c("area","week"))

###(2)Basic assumptions####

###--Distance--###
#According to the condition that the commuting distance is constant,
#the user's original commuting mode that before dockless bikes appeared was assumed:

##A.We classify ride miles that ride less than 1km in a single ride into walking mode(milew)
##B.We classify ride miles who ride more than 3km in a single ride into driving mode(milec)36.2% and public transportation(milep)63.8%
##C.ride miles with other distances in a single ride were classified into riding mode(mileb)
bikepo <- mutate(bikepo,
                 milew=(wts*onemilr0.5*0.25)+
                   (wts*onemilr1*0.75),
                 milec=((wts*onemilr5.*6)+
                          (wts*onemilr5*4.75)+(wts*onemilr4.5*4.25)+
                          (wts*onemilr4*3.75)+(wts*onemilr3.5*3.25))*0.362,
                 milep=((wts*onemilr5.*6)+
                          (wts*onemilr5*4.75)+(wts*onemilr4.5*4.25)+
                          (wts*onemilr4*3.75)+(wts*onemilr3.5*3.25))*0.638,
                 mileb=mil-milew-milec-milep)
mean(bikepo$mil)
mean(bikepo$milew)
mean(bikepo$milec)
mean(bikepo$milep)
mean(bikepo$mileb)

mean(bikepo$milew)+mean(bikepo$mileb)+mean(bikepo$milec)+mean(bikepo$milep)
###--Users--###

###--Active number of different mode by age and gender--###
#active users of each modes are ontained based on the online survey
bikepo$acw <- 0.632*bikepo$wau*10000  #users walking before 
bikepo$acb <- 0.111*bikepo$wau*10000 #users biking before
bikepo$acc <- 0.093*bikepo$wau*10000  #users driving before
bikepo$acp <- 0.164*bikepo$wau*10000  #users taking public transport before


mean(bikepo$acw)
mean(bikepo$acb)
mean(bikepo$acc)
mean(bikepo$acp)

mean(bikepo$acw)+mean(bikepo$acb)+mean(bikepo$acc)+mean(bikepo$acp)
mean(bikepo$wau)

####*1.Data calculation*####
###(1)Total mileage divided by gender and age ratio####
bikepo$milmr <- bikepo$milm/bikepo$mil
bikepo$milfr <- bikepo$milf/bikepo$mil

##gender-age distribution(10 type)
#male
bikepo$mil25mr <- bikepo$mil25m/bikepo$mil
bikepo$mil2530mr <- bikepo$mil2530m/bikepo$mil
bikepo$mil3135mr <- bikepo$mil3135m/bikepo$mil
bikepo$mil3640mr <- bikepo$mil3640m/bikepo$mil
bikepo$mil40mr <- bikepo$mil40m/bikepo$mil

#female
bikepo$mil25fr <- bikepo$mil25f/bikepo$mil
bikepo$mil2530fr <- bikepo$mil2530f/bikepo$mil
bikepo$mil3135fr <- bikepo$mil3135f/bikepo$mil
bikepo$mil3640fr <- bikepo$mil3640f/bikepo$mil
bikepo$mil40fr <- bikepo$mil40f/bikepo$mil
mean(bikepo$mil25mr+bikepo$mil2530mr+bikepo$mil3135mr+bikepo$mil3640mr+bikepo$mil40mr+bikepo$mil25fr+bikepo$mil2530fr+bikepo$mil3135fr+bikepo$mil3640fr+bikepo$mil40fr,na.rm = T)

###(2)Active user who used to walk, drive, cycling by gender and age ratio####

##Walking
#gender-age distribution(10 type)
bikepo$acw25m<-bikepo$acw*bikepo$ager25m
bikepo$acw2630m<-bikepo$acw*bikepo$ager2630m
bikepo$acw3135m<-bikepo$acw*bikepo$ager3135m
bikepo$acw3640m<-bikepo$acw*bikepo$ager3640m
bikepo$acw41m<-bikepo$acw*bikepo$ager41m
bikepo$acw25f<-bikepo$acw*bikepo$ager25f
bikepo$acw2630f<-bikepo$acw*bikepo$ager2630f
bikepo$acw3135f<-bikepo$acw*bikepo$ager3135f
bikepo$acw3640f<-bikepo$acw*bikepo$ager3640f
bikepo$acw41f<-bikepo$acw*bikepo$ager41f
mean(bikepo$acw25m)+mean(bikepo$acw2630m)+mean(bikepo$acw3135m)+mean(bikepo$acw3640m)+mean(bikepo$acw41m)+mean(bikepo$acw25f)+mean(bikepo$acw2630f)+mean(bikepo$acw3135f)+mean(bikepo$acw3640f)+mean(bikepo$acw41f)
##biking
#gender-age distribution(10 type)
bikepo$acb25m<-bikepo$acb*bikepo$ager25m
bikepo$acb2630m<-bikepo$acb*bikepo$ager2630m
bikepo$acb3135m<-bikepo$acb*bikepo$ager3135m
bikepo$acb3640m<-bikepo$acb*bikepo$ager3640m
bikepo$acb41m<-bikepo$acb*bikepo$ager41m
bikepo$acb25f<-bikepo$acb*bikepo$ager25f
bikepo$acb2630f<-bikepo$acb*bikepo$ager2630f
bikepo$acb3135f<-bikepo$acb*bikepo$ager3135f
bikepo$acb3640f<-bikepo$acb*bikepo$ager3640f
bikepo$acb41f<-bikepo$acb*bikepo$ager41f
mean(bikepo$acb25m)+mean(bikepo$acb2630m)+mean(bikepo$acb3135m)+mean(bikepo$acb3640m)+mean(bikepo$acb41m)+mean(bikepo$acb25f)+mean(bikepo$acb2630f)+mean(bikepo$acb3135f)+mean(bikepo$acb3640f)+mean(bikepo$acb41f)
##Drving
#gender-age distribution(10 type)
bikepo$acc25m<-bikepo$acc*bikepo$ager25m
bikepo$acc2630m<-bikepo$acc*bikepo$ager2630m
bikepo$acc3135m<-bikepo$acc*bikepo$ager3135m
bikepo$acc3640m<-bikepo$acc*bikepo$ager3640m
bikepo$acc41m<-bikepo$acc*bikepo$ager41m
bikepo$acc25f<-bikepo$acc*bikepo$ager25f
bikepo$acc2630f<-bikepo$acc*bikepo$ager2630f
bikepo$acc3135f<-bikepo$acc*bikepo$ager3135f
bikepo$acc3640f<-bikepo$acc*bikepo$ager3640f
bikepo$acc41f<-bikepo$acc*bikepo$ager41f
mean(bikepo$acc25m)+mean(bikepo$acc2630m)+mean(bikepo$acc3135m)+mean(bikepo$acc3640m)+mean(bikepo$acc41m)+mean(bikepo$acc25f)+mean(bikepo$acc2630f)+mean(bikepo$acc3135f)+mean(bikepo$acc3640f)+mean(bikepo$acc41f)
#Taking public transportation
#gender-age distribution(10 type)
bikepo$acp25m<-bikepo$acp*bikepo$ager25m
bikepo$acp2630m<-bikepo$acp*bikepo$ager2630m
bikepo$acp3135m<-bikepo$acp*bikepo$ager3135m
bikepo$acp3640m<-bikepo$acp*bikepo$ager3640m
bikepo$acp41m<-bikepo$acp*bikepo$ager41m
bikepo$acp25f<-bikepo$acp*bikepo$ager25f
bikepo$acp2630f<-bikepo$acp*bikepo$ager2630f
bikepo$acp3135f<-bikepo$acp*bikepo$ager3135f
bikepo$acp3640f<-bikepo$acp*bikepo$ager3640f
bikepo$acp41f<-bikepo$acp*bikepo$ager41f
mean(bikepo$acp25m)+mean(bikepo$acp2630m)+mean(bikepo$acp3135m)+mean(bikepo$acp3640m)+mean(bikepo$acp41m)+mean(bikepo$acp25f)+mean(bikepo$acp2630f)+mean(bikepo$acp3135f)+mean(bikepo$acp3640f)+mean(bikepo$acp41f)


###(3)Time spent in different travel modes before modal shift####

##Total
bikepo$walkt <- bikepo$milew*10000/(5/60)

#The walking speed was set as 5km/h,
#10000 is the distance unit(10 thousand km), 60 is the conversion of hour and minute in speed
bikepo$biket <- bikepo$mileb*10000/(bikepo$mil/bikepo$wtt)
#The riding speed was extract from actual data
bikepo$cart <- bikepo$milec*10000/(30/60)
#The driving speed was set as 30km/h
bikepo$ptt <- bikepo$milep*10000/(30/60)
#The public bus speed was set as 30 km/h
mean(bikepo$walkt,na.rm = T)
mean(bikepo$biket,na.rm = T)
mean(bikepo$cart,na.rm = T)
mean(bikepo$ptt,na.rm = T)

##Walking time gender-age distribution
#walk time before
bikepo$walkt25m <- bikepo$milew*bikepo$mil25mr*10000/(5/60)
bikepo$walkt2530m <- bikepo$milew*bikepo$mil2530mr*10000/(5/60)
bikepo$walkt3135m <- bikepo$milew*bikepo$mil3135mr*10000/(5/60)
bikepo$walkt3640m <- bikepo$milew*bikepo$mil3640mr*10000/(5/60)
bikepo$walkt41m <- bikepo$milew*bikepo$mil40mr*10000/(5/60)
bikepo$walkt25f <- bikepo$milew*bikepo$mil25fr*10000/(5/60)
bikepo$walkt2530f <- bikepo$milew*bikepo$mil2530fr*10000/(5/60)
bikepo$walkt3135f <- bikepo$milew*bikepo$mil3135fr*10000/(5/60)
bikepo$walkt3640f <- bikepo$milew*bikepo$mil3640fr*10000/(5/60)
bikepo$walkt41f <- bikepo$milew*bikepo$mil40fr*10000/(5/60)
mean(bikepo$walkt25m+bikepo$walkt2530m+bikepo$walkt3135m+bikepo$walkt3640m+bikepo$walkt41m+bikepo$walkt25f+bikepo$walkt2530f+bikepo$walkt3135f+bikepo$walkt3640f+bikepo$walkt41f,
     na.rm = T)
#walking time after bike shared
bikepo$walktshared25m<-bikepo$milew*bikepo$mil25mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$walktshared2530m <- bikepo$milew*bikepo$mil2530mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$walktshared3135m <- bikepo$milew*bikepo$mil3135mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$walktshared3640m <- bikepo$milew*bikepo$mil3640mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$walktshared41m <- bikepo$milew*bikepo$mil40mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$walktshared25f <- bikepo$milew*bikepo$mil25fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$walktshared2530f <- bikepo$milew*bikepo$mil2530fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$walktshared3135f <- bikepo$milew*bikepo$mil3135fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$walktshared3640f <- bikepo$milew*bikepo$mil3640fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$walktshared41f <- bikepo$milew*bikepo$mil40fr*10000/(bikepo$mil/bikepo$wtt)
mean(bikepo$mil/bikepo$wtt,
     na.rm = T)
mean(bikepo$walktshared25m+bikepo$walktshared2530m+bikepo$walktshared3135m+bikepo$walktshared3640m+bikepo$walktshared41m+bikepo$walktshared25f+bikepo$walktshared2530f+bikepo$walktshared3135f+bikepo$walktshared3640f+bikepo$walktshared41f,
     na.rm = T)

##driving time gender-age distribution
#driving time before
bikepo$cart25m <- bikepo$milec*bikepo$mil25mr*10000/(30/60)
bikepo$cart2530m <- bikepo$milec*bikepo$mil2530mr*10000/(30/60)
bikepo$cart3135m <- bikepo$milec*bikepo$mil3135mr*10000/(30/60)
bikepo$cart3640m <- bikepo$milec*bikepo$mil3640mr*10000/(30/60)
bikepo$cart41m <- bikepo$milec*bikepo$mil40mr*10000/(30/60)
bikepo$cart25f <- bikepo$milec*bikepo$mil25fr*10000/(30/60)
bikepo$cart2530f <- bikepo$milec*bikepo$mil2530fr*10000/(30/60)
bikepo$cart3135f <- bikepo$milec*bikepo$mil3135fr*10000/(30/60)
bikepo$cart3640f <- bikepo$milec*bikepo$mil3640fr*10000/(30/60)
bikepo$cart41f <- bikepo$milec*bikepo$mil40fr*10000/(30/60)
mean(bikepo$cart25m+bikepo$cart2530m+bikepo$cart3135m+bikepo$cart3640m+bikepo$cart41m+bikepo$cart25f+bikepo$cart2530f+bikepo$cart3135f+bikepo$cart3640f+bikepo$cart41f,
     na.rm = T)

#driving time after bike shared
bikepo$cartshared25m <- bikepo$milec*bikepo$mil25mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$cartshared2530m <- bikepo$milec*bikepo$mil2530mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$cartshared3135m <- bikepo$milec*bikepo$mil3135mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$cartshared3640m <- bikepo$milec*bikepo$mil3640mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$cartshared41m <- bikepo$milec*bikepo$mil40mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$cartshared25f <- bikepo$milec*bikepo$mil25fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$cartshared2530f <- bikepo$milec*bikepo$mil2530fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$cartshared3135f <- bikepo$milec*bikepo$mil3135fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$cartshared3640f <- bikepo$milec*bikepo$mil3640fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$cartshared41f <- bikepo$milec*bikepo$mil40fr*10000/(bikepo$mil/bikepo$wtt)
mean(bikepo$cartshared25m+bikepo$cartshared2530m+bikepo$cartshared3135m+bikepo$cartshared3640m+bikepo$cartshared41m+bikepo$cartshared25f+bikepo$cartshared2530f+bikepo$cartshared3135f+bikepo$cartshared3640f+bikepo$cartshared41f,
     na.rm = T)

##biking time gender-age distribution
#biking time before
bikepo$biket25m <- bikepo$mileb*bikepo$mil25mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket2530m <- bikepo$mileb*bikepo$mil2530mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket3135m <- bikepo$mileb*bikepo$mil3135mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket3640m <- bikepo$mileb*bikepo$mil3640mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket41m <- bikepo$mileb*bikepo$mil40mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket25f <- bikepo$mileb*bikepo$mil25fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket2530f <- bikepo$mileb*bikepo$mil2530fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket3135f <- bikepo$mileb*bikepo$mil3135fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket3640f <- bikepo$mileb*bikepo$mil3640fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket41f <- bikepo$mileb*bikepo$mil40fr*10000/(bikepo$mil/bikepo$wtt)
mean(bikepo$biket25m+bikepo$biket2530m+bikepo$biket3135m+bikepo$biket3640m+bikepo$biket41m+bikepo$biket25f+bikepo$biket2530f+bikepo$biket3135f+bikepo$biket3640f+bikepo$biket41f,
     na.rm = T)

#biking time after bike shared
bikepo$biketshared25m <- bikepo$mileb*bikepo$mil25mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biketshared2530m <- bikepo$mileb*bikepo$mil2530mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biketshared3135m <- bikepo$mileb*bikepo$mil3135mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biketshared3640m <- bikepo$mileb*bikepo$mil3640mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biketshared41m <- bikepo$mileb*bikepo$mil40mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biketshared25f <- bikepo$mileb*bikepo$mil25fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biketshared2530f <- bikepo$mileb*bikepo$mil2530fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biketshared3135f <- bikepo$mileb*bikepo$mil3135fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biketshared3640f <- bikepo$mileb*bikepo$mil3640fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biketshared41f <- bikepo$mileb*bikepo$mil40fr*10000/(bikepo$mil/bikepo$wtt)
mean(bikepo$biketshared25m+bikepo$biketshared2530m+bikepo$biketshared3135m+bikepo$biketshared3640m+bikepo$biketshared41m+bikepo$biketshared25f+bikepo$biketshared2530f+bikepo$biketshared3135f+bikepo$biketshared3640f+bikepo$biketshared41f,
     na.rm = T)

##PT time gender-age distribution
#PT time before
bikepo$ptt25m <- bikepo$milep*bikepo$mil25mr*10000/(30/60)
bikepo$ptt2530m <- bikepo$milep*bikepo$mil2530mr*10000/(30/60)
bikepo$ptt3135m <- bikepo$milep*bikepo$mil3135mr*10000/(30/60)
bikepo$ptt3640m <- bikepo$milep*bikepo$mil3640mr*10000/(30/60)
bikepo$ptt41m <- bikepo$milep*bikepo$mil40mr*10000/(30/60)
bikepo$ptt25f <- bikepo$milep*bikepo$mil25fr*10000/(30/60)
bikepo$ptt2530f <- bikepo$milep*bikepo$mil2530fr*10000/(30/60)
bikepo$ptt3135f <- bikepo$milep*bikepo$mil3135fr*10000/(30/60)
bikepo$ptt3640f <- bikepo$milep*bikepo$mil3640fr*10000/(30/60)
bikepo$ptt41f <- bikepo$milep*bikepo$mil40fr*10000/(30/60)
mean(bikepo$ptt25m+bikepo$ptt2530m+bikepo$ptt3135m+bikepo$ptt3640m+bikepo$ptt41m+bikepo$ptt25f+bikepo$ptt2530f+bikepo$ptt3135f+bikepo$ptt3640f+bikepo$ptt41f,
     na.rm = T)
#walking time after bike shared
bikepo$pttshared25m<-bikepo$milep*bikepo$mil25mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$pttshared2530m <- bikepo$milep*bikepo$mil2530mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$pttshared3135m <- bikepo$milep*bikepo$mil3135mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$pttshared3640m <- bikepo$milep*bikepo$mil3640mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$pttshared41m <- bikepo$milep*bikepo$mil40mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$pttshared25f <- bikepo$milep*bikepo$mil25fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$pttshared2530f <- bikepo$milep*bikepo$mil2530fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$pttshared3135f <- bikepo$milep*bikepo$mil3135fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$pttshared3640f <- bikepo$milep*bikepo$mil3640fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$pttshared41f <- bikepo$milep*bikepo$mil40fr*10000/(bikepo$mil/bikepo$wtt)
mean(bikepo$mil/bikepo$wtt,
     na.rm = T)
mean(bikepo$pttshared25m+bikepo$pttshared2530m+bikepo$pttshared3135m+bikepo$pttshared3640m+bikepo$pttshared41m+bikepo$pttshared25f+bikepo$pttshared2530f+bikepo$pttshared3135f+bikepo$pttshared3640f+bikepo$pttshared41f,
     na.rm = T)

#下一步分组计算各组的MMET并输出表格，需要看bus的MMET是多少，不管为0还是2.3，都不太对
###(3)Travel MET-hour of each group before and after travel mode shift per week(changed)####
##Total
#Travel MET-hour before travel mode shift per week
bikepo$tmetnoshared25m <- (bikepo$walkt25m*(3.6-1)+bikepo$biket25m*(5.4-1)+bikepo$ptt25m*(1-1))/(bikepo$wau*10000*bikepo$ager25m)/60
bikepo$tmetnoshared2630m <- (bikepo$walkt2530m*(3.6-1)+bikepo$biket2530m*(5.4-1)+bikepo$ptt2530m*(1-1))/(bikepo$wau*10000*bikepo$ager2630m)/60
bikepo$tmetnoshared3135m <- (bikepo$walkt3135m*(3.6-1)+bikepo$biket3135m*(5.4-1)+bikepo$ptt3135m*(1-1))/(bikepo$wau*10000*bikepo$ager3135m)/60
bikepo$tmetnoshared3640m <- (bikepo$walkt3640m*(3.6-1)+bikepo$biket3640m*(5.4-1)+bikepo$ptt3640m*(1-1))/(bikepo$wau*10000*bikepo$ager3640m)/60
bikepo$tmetnoshared41m <- (bikepo$walkt41m*(3.6-1)+bikepo$biket41m*(5.4-1)+bikepo$ptt41m*(1-1))/(bikepo$wau*10000*bikepo$ager41m)/60
bikepo$tmetnoshared25f <- (bikepo$walkt25f*(3.6-1)+bikepo$biket25f*(5.4-1)+bikepo$ptt25f*(1-1))/(bikepo$wau*10000*bikepo$ager25f)/60
bikepo$tmetnoshared2630f <- (bikepo$walkt2530f*(3.6-1)+bikepo$biket2530f*(5.4-1)+bikepo$ptt2530f*(1-1))/(bikepo$wau*10000*bikepo$ager2630f)/60
bikepo$tmetnoshared3135f <- (bikepo$walkt3135f*(3.6-1)+bikepo$biket3135f*(5.4-1)+bikepo$ptt3135f*(1-1))/(bikepo$wau*10000*bikepo$ager3135f)/60
bikepo$tmetnoshared3640f <- (bikepo$walkt3640f*(3.6-1)+bikepo$biket3640f*(5.4-1)+bikepo$ptt3640f*(1-1))/(bikepo$wau*10000*bikepo$ager3640f)/60
bikepo$tmetnoshared41f <- (bikepo$walkt41f*(3.6-1)+bikepo$biket41f*(5.4-1)+bikepo$ptt41f*(1-1))/(bikepo$wau*10000*bikepo$ager41f)/60
##Travel MET-hour after travel mode shift per week
bikepo$tmetshared25m <-(bikepo$mil25m*10000/(bikepo$mil/bikepo$wtt)*(5.4-1))/(bikepo$wau*10000*bikepo$ager25m)/60
bikepo$tmetshared2630m <- (bikepo$mil2530m*10000/(bikepo$mil/bikepo$wtt)*(5.4-1))/(bikepo$wau*10000*bikepo$ager2630m)/60
bikepo$tmetshared3135m <- (bikepo$mil3135m*10000/(bikepo$mil/bikepo$wtt)*(5.4-1))/(bikepo$wau*10000*bikepo$ager3135m)/60
bikepo$tmetshared3640m <- (bikepo$mil3640m*10000/(bikepo$mil/bikepo$wtt)*(5.4-1))/(bikepo$wau*10000*bikepo$ager3640m)/60
bikepo$tmetshared41m <- (bikepo$mil40m*10000/(bikepo$mil/bikepo$wtt)*(5.4-1))/(bikepo$wau*10000*bikepo$ager41m)/60
bikepo$tmetshared25f <- (bikepo$mil25f*10000/(bikepo$mil/bikepo$wtt)*(5.4-1))/(bikepo$wau*10000*bikepo$ager25f)/60
bikepo$tmetshared2630f <- (bikepo$mil2530f*10000/(bikepo$mil/bikepo$wtt)*(5.4-1))/(bikepo$wau*10000*bikepo$ager2630f)/60
bikepo$tmetshared3135f <- (bikepo$mil3135f*10000/(bikepo$mil/bikepo$wtt)*(5.4-1))/(bikepo$wau*10000*bikepo$ager3135f)/60
bikepo$tmetshared3640f <- (bikepo$mil3640f*10000/(bikepo$mil/bikepo$wtt)*(5.4-1))/(bikepo$wau*10000*bikepo$ager3640f)/60
bikepo$tmetshared41f <- (bikepo$mil40f*10000/(bikepo$mil/bikepo$wtt)*(5.4-1))/(bikepo$wau*10000*bikepo$ager41f)/60
mean(bikepo$tmetnoshared25m,na.rm = T)
mean(bikepo$tmetshared25m,na.rm = T)

##Walking
#Travel MET-hour before travel mode shift per week
bikepo$wtmetnoshared25m <- (bikepo$walkt25m*(3.6-1))/(bikepo$acw25m)/60
bikepo$wtmetnoshared2530m <- (bikepo$walkt2530m*(3.6-1))/(bikepo$acw2630m)/60
bikepo$wtmetnoshared3135m <- (bikepo$walkt3135m*(3.6-1))/(bikepo$acw3135m)/60
bikepo$wtmetnoshared3640m <- (bikepo$walkt3640m*(3.6-1))/(bikepo$acw3640m)/60
bikepo$wtmetnoshared41m <- (bikepo$walkt41m*(3.6-1))/(bikepo$acw41m)/60
bikepo$wtmetnoshared25f <- (bikepo$walkt25f*(3.6-1))/(bikepo$acw25f)/60
bikepo$wtmetnoshared2530f <- (bikepo$walkt2530f*(3.6-1))/(bikepo$acw2630f)/60
bikepo$wtmetnoshared3135f <- (bikepo$walkt3135f*(3.6-1))/(bikepo$acw3135f)/60
bikepo$wtmetnoshared3640f <- (bikepo$walkt3640f*(3.6-1))/(bikepo$acw3640f)/60
bikepo$wtmetnoshared41f <- (bikepo$walkt41f*(3.6-1))/(bikepo$acw41f)/60
#Travel MET-hour after travel mode shift per week
bikepo$wtmetshared25m <- (bikepo$walktshared25m*(5.4-1))/(bikepo$acw25m)/60
bikepo$wtmetshared2530m <- (bikepo$walktshared2530m*(5.4-1))/(bikepo$acw2630m)/60
bikepo$wtmetshared3135m <- (bikepo$walktshared3135m*(5.4-1))/(bikepo$acw3135m)/60
bikepo$wtmetshared3640m <- (bikepo$walktshared3640m*(5.4-1))/(bikepo$acw3640m)/60
bikepo$wtmetshared41m <- (bikepo$walktshared41m*(5.4-1))/(bikepo$acw41m)/60
bikepo$wtmetshared25f <- (bikepo$walktshared25f*(5.4-1))/(bikepo$acw25f)/60
bikepo$wtmetshared2530f <- (bikepo$walktshared2530f*(5.4-1))/(bikepo$acw2630f)/60
bikepo$wtmetshared3135f <- (bikepo$walktshared3135f*(5.4-1))/(bikepo$acw3135f)/60
bikepo$wtmetshared3640f <- (bikepo$walktshared3640f*(5.4-1))/(bikepo$acw3640f)/60
bikepo$wtmetshared41f <- (bikepo$walktshared41f*(5.4-1))/(bikepo$acw41f)/60

##driving
#Travel MET-hour before travel mode shift per week
bikepo$ctmetnoshared25m <- (bikepo$cart25m*(0))/(bikepo$acc25m)/60
bikepo$ctmetnoshared2530m <- (bikepo$cart2530m*(0))/(bikepo$acc2630m)/60
bikepo$ctmetnoshared3135m <- (bikepo$cart3135m*(0))/(bikepo$acc3135m)/60
bikepo$ctmetnoshared3640m <- (bikepo$cart3640m*(0))/(bikepo$acc3640m)/60
bikepo$ctmetnoshared41m <- (bikepo$cart41m*(0))/(bikepo$acc41m)/60
bikepo$ctmetnoshared25f <- (bikepo$cart25f*(0))/(bikepo$acc25f)/60
bikepo$ctmetnoshared2530f <- (bikepo$cart2530f*(0))/(bikepo$acc2630f)/60
bikepo$ctmetnoshared3135f <- (bikepo$cart3135f*(0))/(bikepo$acc3135f)/60
bikepo$ctmetnoshared3640f <- (bikepo$cart3640f*(0))/(bikepo$acc3640f)/60
bikepo$ctmetnoshared41f <- (bikepo$cart41f*(0))/(bikepo$acc41f)/60
#Travel MET-hour after travel mode shift per week
bikepo$ctmetshared25m <- (bikepo$walktshared25m*(5.4-1))/(bikepo$acc25m)/60
bikepo$ctmetshared2530m <- (bikepo$walktshared2530m*(5.4-1))/(bikepo$acc2630m)/60
bikepo$ctmetshared3135m <- (bikepo$walktshared3135m*(5.4-1))/(bikepo$acc3135m)/60
bikepo$ctmetshared3640m <- (bikepo$walktshared3640m*(5.4-1))/(bikepo$acc3640m)/60
bikepo$ctmetshared41m <- (bikepo$walktshared41m*(5.4-1))/(bikepo$acc41m)/60
bikepo$ctmetshared25f <- (bikepo$walktshared25f*(5.4-1))/(bikepo$acc25f)/60
bikepo$ctmetshared2530f <- (bikepo$walktshared2530f*(5.4-1))/(bikepo$acc2630f)/60
bikepo$ctmetshared3135f <- (bikepo$walktshared3135f*(5.4-1))/(bikepo$acc3135f)/60
bikepo$ctmetshared3640f <- (bikepo$walktshared3640f*(5.4-1))/(bikepo$acc3640f)/60
bikepo$ctmetshared41f <- (bikepo$walktshared41f*(5.4-1))/(bikepo$acc41f)/60

##biking
#Travel MET-hour before travel mode shift per week
bikepo$btmetnoshared25m <- (bikepo$biket25m*(5.4-1))/(bikepo$acb25m)/60
bikepo$btmetnoshared2530m <- (bikepo$biket2530m*(5.4-1))/(bikepo$acb2630m)/60
bikepo$btmetnoshared3135m <- (bikepo$biket3135m*(5.4-1))/(bikepo$acb3135m)/60
bikepo$btmetnoshared3640m <- (bikepo$biket3640m*(5.4-1))/(bikepo$acb3640m)/60
bikepo$btmetnoshared41m <- (bikepo$biket41m*(5.4-1))/(bikepo$acb41m)/60
bikepo$btmetnoshared25f <- (bikepo$biket25f*(5.4-1))/(bikepo$acb25f)/60
bikepo$btmetnoshared2530f <- (bikepo$biket2530f*(5.4-1))/(bikepo$acb2630f)/60
bikepo$btmetnoshared3135f <- (bikepo$biket3135f*(5.4-1))/(bikepo$acb3135f)/60
bikepo$btmetnoshared3640f <- (bikepo$biket3640f*(5.4-1))/(bikepo$acb3640f)/60
bikepo$btmetnoshared41f <- (bikepo$biket41f*(5.4-1))/(bikepo$acb41f)/60
#Travel MET-hour after travel mode shift per week
bikepo$btmetshared25m <- (bikepo$biketshared25m*(5.4-1))/(bikepo$acb25m)/60
bikepo$btmetshared2530m <- (bikepo$biketshared2530m*(5.4-1))/(bikepo$acb2630m)/60
bikepo$btmetshared3135m <- (bikepo$biketshared3135m*(5.4-1))/(bikepo$acb3135m)/60
bikepo$btmetshared3640m <- (bikepo$biketshared3640m*(5.4-1))/(bikepo$acb3640m)/60
bikepo$btmetshared41m <- (bikepo$biketshared41m*(5.4-1))/(bikepo$acb41m)/60
bikepo$btmetshared25f <- (bikepo$biketshared25f*(5.4-1))/(bikepo$acb25f)/60
bikepo$btmetshared2530f <- (bikepo$biketshared2530f*(5.4-1))/(bikepo$acb2630f)/60
bikepo$btmetshared3135f <- (bikepo$biketshared3135f*(5.4-1))/(bikepo$acb3135f)/60
bikepo$btmetshared3640f <- (bikepo$biketshared3640f*(5.4-1))/(bikepo$acb3640f)/60
bikepo$btmetshared41f <- (bikepo$biketshared41f*(5.4-1))/(bikepo$acb41f)/60

##PT(需要修改这个1，目前相当于假设BUS的MMET为0,即不考虑bus的PA)
#Travel MET-hour before travel mode shift per week
bikepo$ptmetnoshared25m <- (bikepo$ptt25m*(1-1))/(bikepo$acp25m)/60
bikepo$ptmetnoshared2530m <- (bikepo$ptt2530m*(1-1))/(bikepo$acp2630m)/60
bikepo$ptmetnoshared3135m <- (bikepo$ptt3135m*(1-1))/(bikepo$acp3135m)/60
bikepo$ptmetnoshared3640m <- (bikepo$ptt3640m*(1-1))/(bikepo$acp3640m)/60
bikepo$ptmetnoshared41m <- (bikepo$ptt41m*(1-1))/(bikepo$acp41m)/60
bikepo$ptmetnoshared25f <- (bikepo$ptt25f*(1-1))/(bikepo$acp25f)/60
bikepo$ptmetnoshared2530f <- (bikepo$ptt2530f*(1-1))/(bikepo$acp2630f)/60
bikepo$ptmetnoshared3135f <- (bikepo$ptt3135f*(1-1))/(bikepo$acp3135f)/60
bikepo$ptmetnoshared3640f <- (bikepo$ptt3640f*(1-1))/(bikepo$acp3640f)/60
bikepo$ptmetnoshared41f <- (bikepo$ptt41f*(1-1))/(bikepo$acp41f)/60
#Travel MET-hour after travel mode shift per week
bikepo$ptmetshared25m <- (bikepo$pttshared25m*(5.4-1))/(bikepo$acp25m)/60
bikepo$ptmetshared2530m <- (bikepo$pttshared2530m*(5.4-1))/(bikepo$acp2630m)/60
bikepo$ptmetshared3135m <- (bikepo$pttshared3135m*(5.4-1))/(bikepo$acp3135m)/60
bikepo$ptmetshared3640m <- (bikepo$pttshared3640m*(5.4-1))/(bikepo$acp3640m)/60
bikepo$ptmetshared41m <- (bikepo$pttshared41m*(5.4-1))/(bikepo$acp41m)/60
bikepo$ptmetshared25f <- (bikepo$pttshared25f*(5.4-1))/(bikepo$acp25f)/60
bikepo$ptmetshared2530f <- (bikepo$pttshared2530f*(5.4-1))/(bikepo$acp2630f)/60
bikepo$ptmetshared3135f <- (bikepo$pttshared3135f*(5.4-1))/(bikepo$acp3135f)/60
bikepo$ptmetshared3640f <- (bikepo$pttshared3640f*(5.4-1))/(bikepo$acp3640f)/60
bikepo$ptmetshared41f <- (bikepo$pttshared41f*(5.4-1))/(bikepo$acp41f)/60

mean(bikepo$tmetnoshared25m,na.rm = T)
mean(bikepo$tmetshared25m,na.rm = T)

mean(bikepo$wtmetnoshared25m,na.rm = T)
mean(bikepo$wtmetshared25m,na.rm = T)

mean(bikepo$ctmetnoshared25m,na.rm = T)
mean(bikepo$ctmetshared25m,na.rm = T)

mean(bikepo$btmetnoshared25m,na.rm = T)
mean(bikepo$btmetshared25m,na.rm = T)

mean(bikepo$ptmetnoshared25m,na.rm = T)
mean(bikepo$ptmetshared25m,na.rm = T)

###(4)Output travel MET*hour of each group per week before and after travel mode shift(changed)####
agegroup<-c("15-25","15-25","26-30","26-30","31-35","31-35","36-40","36-40","41-59","41-59")
GENDER<-c("1","2","1","2","1","2","1","2","1","2")
##Total output mean travel value by age and gender (1=m,2=f)
tmetnoshared<-c(mean(bikepo$tmetnoshared25m,na.rm = T),
                mean(bikepo$tmetnoshared25f,na.rm = T),
                mean(bikepo$tmetnoshared2630m,na.rm = T),
                mean(bikepo$tmetnoshared2630f,na.rm = T),
                mean(bikepo$tmetnoshared3135m,na.rm = T),
                mean(bikepo$tmetnoshared3135f,na.rm = T),
                mean(bikepo$tmetnoshared3640m,na.rm = T),
                mean(bikepo$tmetnoshared3640f,na.rm = T),
                mean(bikepo$tmetnoshared41m,na.rm = T),
                mean(bikepo$tmetnoshared41f,na.rm = T))
tmetshared<-c(mean(bikepo$tmetshared25m,na.rm = T),
              mean(bikepo$tmetshared25f,na.rm = T),
              mean(bikepo$tmetshared2630m,na.rm = T),
              mean(bikepo$tmetshared2630f,na.rm = T),
              mean(bikepo$tmetshared3135m,na.rm = T),
              mean(bikepo$tmetshared3135f,na.rm = T),
              mean(bikepo$tmetshared3640m,na.rm = T),
              mean(bikepo$tmetshared3640f,na.rm = T),
              mean(bikepo$tmetshared41m,na.rm = T),
              mean(bikepo$tmetshared41f,na.rm = T))
tpopsize<-c(mean(bikepo$wau*10000*bikepo$ager25m,na.rm = T),
            mean(bikepo$wau*10000*bikepo$ager25f,na.rm = T),
            mean(bikepo$wau*10000*bikepo$ager2630m,na.rm = T),
            mean(bikepo$wau*10000*bikepo$ager2630f,na.rm = T),
            mean(bikepo$wau*10000*bikepo$ager3135m,na.rm = T),
            mean(bikepo$wau*10000*bikepo$ager3135f,na.rm = T),
            mean(bikepo$wau*10000*bikepo$ager3640m,na.rm = T),
            mean(bikepo$wau*10000*bikepo$ager3640f,na.rm = T),
            mean(bikepo$wau*10000*bikepo$ager41m,na.rm = T),
            mean(bikepo$wau*10000*bikepo$ager41f,na.rm = T))
meanTTMET<-data.frame(agegroup,GENDER,tpopsize,tmetnoshared,tmetshared)

##Walking group
wtmetnoshared<-c(mean(bikepo$wtmetnoshared25m,na.rm = T),
                 mean(bikepo$wtmetnoshared25f,na.rm = T),
                 mean(bikepo$wtmetnoshared2530m,na.rm = T),
                 mean(bikepo$wtmetnoshared2530f,na.rm = T),
                 mean(bikepo$wtmetnoshared3135m,na.rm = T),
                 mean(bikepo$wtmetnoshared3135f,na.rm = T),
                 mean(bikepo$wtmetnoshared3640m,na.rm = T),
                 mean(bikepo$wtmetnoshared3640f,na.rm = T),
                 mean(bikepo$wtmetnoshared41m,na.rm = T),
                 mean(bikepo$wtmetnoshared41f,na.rm = T))
wtmetshared<-c(mean(bikepo$wtmetshared25m,na.rm = T),
               mean(bikepo$wtmetshared25f,na.rm = T),
               mean(bikepo$wtmetshared2530m,na.rm = T),
               mean(bikepo$wtmetshared2530f,na.rm = T),
               mean(bikepo$wtmetshared3135m,na.rm = T),
               mean(bikepo$wtmetshared3135f,na.rm = T),
               mean(bikepo$wtmetshared3640m,na.rm = T),
               mean(bikepo$wtmetshared3640f,na.rm = T),
               mean(bikepo$wtmetshared41m,na.rm = T),
               mean(bikepo$wtmetshared41f,na.rm = T))
wpopsize<-c(mean(bikepo$acw25m,na.rm = T),
            mean(bikepo$acw25f,na.rm = T),
            mean(bikepo$acw2630m,na.rm = T),
            mean(bikepo$acw2630f,na.rm = T),
            mean(bikepo$acw3135m,na.rm = T),
            mean(bikepo$acw3135f,na.rm = T),
            mean(bikepo$acw3640m,na.rm = T),
            mean(bikepo$acw3640f,na.rm = T),
            mean(bikepo$acw41m,na.rm = T),
            mean(bikepo$acw41f,na.rm = T))
meanWTMET<-data.frame(agegroup,GENDER,wpopsize,wtmetnoshared,wtmetshared)
##Driving group
ctmetnoshared<-c(mean(bikepo$ctmetnoshared25m,na.rm = T),
                 mean(bikepo$ctmetnoshared25f,na.rm = T),
                 mean(bikepo$ctmetnoshared2530m,na.rm = T),
                 mean(bikepo$ctmetnoshared2530f,na.rm = T),
                 mean(bikepo$ctmetnoshared3135m,na.rm = T),
                 mean(bikepo$ctmetnoshared3135f,na.rm = T),
                 mean(bikepo$ctmetnoshared3640m,na.rm = T),
                 mean(bikepo$ctmetnoshared3640f,na.rm = T),
                 mean(bikepo$ctmetnoshared41m,na.rm = T),
                 mean(bikepo$ctmetnoshared41f,na.rm = T))
ctmetshared<-c(mean(bikepo$ctmetshared25m,na.rm = T),
               mean(bikepo$ctmetshared25f,na.rm = T),
               mean(bikepo$ctmetshared2530m,na.rm = T),
               mean(bikepo$ctmetshared2530f,na.rm = T),
               mean(bikepo$ctmetshared3135m,na.rm = T),
               mean(bikepo$ctmetshared3135f,na.rm = T),
               mean(bikepo$ctmetshared3640m,na.rm = T),
               mean(bikepo$ctmetshared3640f,na.rm = T),
               mean(bikepo$ctmetshared41m,na.rm = T),
               mean(bikepo$ctmetshared41f,na.rm = T))
cpopsize<-c(mean(bikepo$acc25m,na.rm = T),
            mean(bikepo$acc25f,na.rm = T),
            mean(bikepo$acc2630m,na.rm = T),
            mean(bikepo$acc2630f,na.rm = T),
            mean(bikepo$acc3135m,na.rm = T),
            mean(bikepo$acc3135f,na.rm = T),
            mean(bikepo$acc3640m,na.rm = T),
            mean(bikepo$acc3640f,na.rm = T),
            mean(bikepo$acc41m,na.rm = T),
            mean(bikepo$acc41f,na.rm = T))
meanCTMET<-data.frame(agegroup,GENDER,cpopsize,ctmetnoshared,ctmetshared)
##Biking group
btmetnoshared<-c(mean(bikepo$btmetnoshared25m,na.rm = T),
                 mean(bikepo$btmetnoshared25f,na.rm = T),
                 mean(bikepo$btmetnoshared2530m,na.rm = T),
                 mean(bikepo$btmetnoshared2530f,na.rm = T),
                 mean(bikepo$btmetnoshared3135m,na.rm = T),
                 mean(bikepo$btmetnoshared3135f,na.rm = T),
                 mean(bikepo$btmetnoshared3640m,na.rm = T),
                 mean(bikepo$btmetnoshared3640f,na.rm = T),
                 mean(bikepo$btmetnoshared41m,na.rm = T),
                 mean(bikepo$btmetnoshared41f,na.rm = T))
btmetshared<-c(mean(bikepo$btmetshared25m,na.rm = T),
               mean(bikepo$btmetshared25f,na.rm = T),
               mean(bikepo$btmetshared2530m,na.rm = T),
               mean(bikepo$btmetshared2530f,na.rm = T),
               mean(bikepo$btmetshared3135m,na.rm = T),
               mean(bikepo$btmetshared3135f,na.rm = T),
               mean(bikepo$btmetshared3640m,na.rm = T),
               mean(bikepo$btmetshared3640f,na.rm = T),
               mean(bikepo$btmetshared41m,na.rm = T),
               mean(bikepo$btmetshared41f,na.rm = T))
bpopsize<-c(mean(bikepo$acb25m,na.rm = T),
            mean(bikepo$acb25f,na.rm = T),
            mean(bikepo$acb2630m,na.rm = T),
            mean(bikepo$acb2630f,na.rm = T),
            mean(bikepo$acb3135m,na.rm = T),
            mean(bikepo$acb3135f,na.rm = T),
            mean(bikepo$acb3640m,na.rm = T),
            mean(bikepo$acb3640f,na.rm = T),
            mean(bikepo$acb41m,na.rm = T),
            mean(bikepo$acb41f,na.rm = T))
meanBTMET<-data.frame(agegroup,GENDER,bpopsize,btmetnoshared,btmetshared)
##PT group
ptmetnoshared<-c(mean(bikepo$ptmetnoshared25m,na.rm = T),
                 mean(bikepo$ptmetnoshared25f,na.rm = T),
                 mean(bikepo$ptmetnoshared2530m,na.rm = T),
                 mean(bikepo$ptmetnoshared2530f,na.rm = T),
                 mean(bikepo$ptmetnoshared3135m,na.rm = T),
                 mean(bikepo$ptmetnoshared3135f,na.rm = T),
                 mean(bikepo$ptmetnoshared3640m,na.rm = T),
                 mean(bikepo$ptmetnoshared3640f,na.rm = T),
                 mean(bikepo$ptmetnoshared41m,na.rm = T),
                 mean(bikepo$ptmetnoshared41f,na.rm = T))
ptmetshared<-c(mean(bikepo$ptmetshared25m,na.rm = T),
               mean(bikepo$ptmetshared25f,na.rm = T),
               mean(bikepo$ptmetshared2530m,na.rm = T),
               mean(bikepo$ptmetshared2530f,na.rm = T),
               mean(bikepo$ptmetshared3135m,na.rm = T),
               mean(bikepo$ptmetshared3135f,na.rm = T),
               mean(bikepo$ptmetshared3640m,na.rm = T),
               mean(bikepo$ptmetshared3640f,na.rm = T),
               mean(bikepo$ptmetshared41m,na.rm = T),
               mean(bikepo$ptmetshared41f,na.rm = T))
ppopsize<-c(mean(bikepo$acp25m,na.rm = T),
            mean(bikepo$acp25f,na.rm = T),
            mean(bikepo$acp2630m,na.rm = T),
            mean(bikepo$acp2630f,na.rm = T),
            mean(bikepo$acp3135m,na.rm = T),
            mean(bikepo$acp3135f,na.rm = T),
            mean(bikepo$acp3640m,na.rm = T),
            mean(bikepo$acp3640f,na.rm = T),
            mean(bikepo$acp41m,na.rm = T),
            mean(bikepo$acp41f,na.rm = T))
meanPTMET<-data.frame(agegroup,GENDER,ppopsize,ptmetnoshared,ptmetshared)

sum(bpopsize)+sum(cpopsize)+sum(wpopsize)
sum(tpopsize)


SEPTMET1 <- merge(meanTTMET,meanPTMET,by=c("agegroup","GENDER"))
SEPTMET2 <- merge(SEPTMET1,meanCTMET,by=c("agegroup","GENDER"))
SEPTMET3 <- merge(SEPTMET2,meanBTMET,by=c("agegroup","GENDER"))
SEPTMET <- merge(SEPTMET3,meanWTMET,by=c("agegroup","GENDER"))
write.csv(SEPTMET,file="./data/Mean travel MET by wlaking driving biking per week.csv",
          row.names = F)



####***********************Calculation of individual non-travel MET-hours**************************####
####*0.Data preparation*####
###(1)Link age, gender, PA by ID####
##Age,gender,PA are scattered in three data sets, but they can be linked by ID

import("./data/PA surveys_pub_12.sas7bdat")->pub
import("./data/PA survey mast_pub_12.sas7bdat")->mast
import("./data/PA survey pact.sas7bdat")->pact
names(pub)
names(mast)
names(pact)
colnames(pact)[1]<-'Idind'

subset(pub,wave==2015 & age>14 & age<60,select=c("Idind","age"))->pubidage
subset(mast,select=c("Idind","GENDER"))->pubidgen
merge(pubidage,pubidgen,by="Idind")->pubid

subset(pact,WAVE==2015,select=c("Idind",
                                "U127A_MN","U129_MN",
                                "U327_MN","U328_MN","U329_MN","U330_MN","U331_MN","U332_MN","U331b_MN","U332b_MN","U333_MN","U334_MN","U335_MN","U336_MN","U337_MN","U338_MN",
                                "U340_MN","U341_MN","U343_MN","U344_MN","U509_MN","U510_MN","U346_MN","U347_MN","U411_MN","U412_MN","U414_MN","U415_MN","U417_MN","U418_MN","U352_MN","U353_MN","U352A_MN","U353A_MN"
))->pa_noccu1.2015
merge(pubid,pa_noccu1.2015,by="Idind")->PA_noccu2.2015

agegroup<-cut(PA_noccu2.2015$age,breaks=c(14,25,30,35,40,60),include.lowest=T,labels = c("15-25","26-30","31-35","36-40","41-59"))
PA<-cbind(PA_noccu2.2015[,1:2],agegroup,PA_noccu2.2015[,3:ncol(PA_noccu2.2015)])

####*1.Data calculation*####
###(1)calculate PA time per week####
##PA time (mins) per week, w=walking,c=cycling,TFS=Track,field and swimming; SBT=Soccer, basketball and tennis;BV=Badminton, volleyball;M=Martial arts;GDA=Gymnastics, dancing and acrobarics; s=walking and stroll; OMT=Others Moderate exercise (Pingpang, Tai Chi)
#walking and cycling
PA$TTw<-PA$U129_MN*5
PA$TTc<-PA$U127A_MN*5

#Vigorous exercise
PA$ETtfs<-PA$U331_MN*5+PA$U332_MN*2
PA$ETsbt<-PA$U333_MN*5+PA$U334_MN*2
#Moderate exercise
PA$ETbv<-PA$U335_MN*5+PA$U336_MN*2
PA$ETma<-PA$U327_MN*5+PA$U328_MN*2
PA$ETgda<-PA$U329_MN*5+PA$U330_MN*2
PA$ETs<-PA$U331b_MN*5+PA$U332b_MN*2
PA$ETom<-PA$U337_MN*5+PA$U338_MN*2
#sedentary
PA$Tsed<-{PA$U340_MN*5+PA$U341_MN*2+PA$U343_MN*5+PA$U344_MN*2+PA$U509_MN*5
  +PA$U510_MN*2+PA$U346_MN*5+PA$U347_MN*2+PA$U411_MN*5+PA$U412_MN*2
  +PA$U414_MN*5+PA$U415_MN*2+PA$U417_MN*5+PA$U418_MN*2+PA$U352A_MN*5+PA$U353A_MN*2}
#Saving the aggregated PA data
write.csv(PA,file="./data/Non-Occupational PA time_Individual.csv")

###(2)Calculate non-travel MET*hour per week####
##MET per week of each activity variable
#MET-hour/week value: w=3.6;c=5.4;TFS=8;SBT=7;BV=4.5;MA=4;GDA=3.5;s=2;OMT=4; sedentary=0
##marginal met=MET - 1
PA$METw<-PA$TTw*(3.6-1)/60
PA$METc<-PA$TTc*(5.4-1)/60

PA$METtfs<-PA$ETtfs/60*(8-1)
PA$METsbt<-PA$ETsbt/60*(7-1)

PA$METbv<-PA$ETbv/60*(4.5-1)
PA$METma<-PA$ETma/60*(4-1)
PA$METgda<-PA$ETgda/60*(3.5-1)
PA$METs<-PA$ETs/60*(2-1)
PA$METom<-PA$ETom/60*(4-1)

PA$METsed<-PA$Tsed/60*0

#Replace the NA value with 0 before applying the addition
PA[is.na(PA)] <- 0 #Does this cause the mean of the PA to be smaller? Because some rows that data are all NA should be deleted, but now they all become 0
PA$MET<-PA$METw+PA$METc+PA$METtfs+PA$METsbt+PA$METbv+PA$METma+PA$METgda+PA$METs+PA$METom+PA$METsed

subset(PA,MET<400,select=c("Idind","age","agegroup","GENDER","MET"))->PAbackground

###(3)Save and describe the individual data####
##Point figure found that some MET values are obviously too large. For adjust, the MET range is selected form 0 to 500 MET*h/week. 8 obs are excluded.
ggplot(PAbackground,aes(x=age,y=MET,color=factor(GENDER)))+geom_point()+xlim(0,100)+labs(y="MET-h/week")
PAbackground%>%ggplot(aes(x=age,y=MET,color=factor(GENDER)))+geom_point()+labs(y="MET-h/week")
#MET per week by age and gender
PAbackground%>%group_by(agegroup,GENDER)%>%summarise(AverageMET=mean(MET,na.rm = T))
ggplot(PAbackground,aes(x=age,y=MET,color=factor(GENDER)))+geom_smooth()+xlim(0,100)+labs(y="MET-h/week")
write.csv(PAbackground,"./data/PA backgroud_Individual.csv")


####***********************Merge the mean values of travel MET and individual physical activity data by age and gender**************************####
##1=public transport,2=car,3=bike,4=walk. Randomly generated mode factor according to a certain proportion
PAbackground$mode<-sample(c(1,2,3,4),8471,replace = TRUE,prob=c(16.4,9.3,11.1,63.2))

##population weight, according to the background individual count and active users by age,gender,and mode 
count<-PAbackground%>%group_by(agegroup,GENDER)%>%count(mode)
TMET <- read.csv("./data/Mean travel MMET by wlaking driving biking per week for merge.csv")
TMET2<-merge(TMET,count,by=c("agegroup","GENDER","mode"))
TMET2$popweight<-round(TMET2$popsize/TMET2$n)
##merge TMET and individual PA
PAmerged<-merge(PAbackground,TMET2,by=c("agegroup","GENDER","mode"))
##calculate the total MET for shared and noshared scenarios
PAmerged$METnoshared<-PAmerged$MET+PAmerged$tmetnoshared
PAmerged$METshared<-PAmerged$MET+PAmerged$tmetshared
write.csv(PAmerged,"./data/PA merged_Individual.csv")
