rm(list =ls()) 
#The change of physical activity in travel mode change
####***********************Calculation of mean travel MET-hours**************************####
####*0.Data preparation*####
#This process is used to import data and establish basic assumptions

#Required package
library(dplyr);library(openxlsx);library(ggplot2);library(tidyverse);library(rio);library(dbplyr);library(ggplot2)

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
#Since we don¡¯t have a record of every ride
#Unit: 10 thousand due to the unit in wts is 10 thousand

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
mean(bikepo$mil25mr)
mean(bikepo$mil2530mr)
mean(bikepo$mil3135mr)
mean(bikepo$mil3640mr)
mean(bikepo$mil40mr)

#female
bikepo$mil25fr <- bikepo$mil25f/bikepo$mil
bikepo$mil2530fr <- bikepo$mil2530f/bikepo$mil
bikepo$mil3135fr <- bikepo$mil3135f/bikepo$mil
bikepo$mil3640fr <- bikepo$mil3640f/bikepo$mil
bikepo$mil40fr <- bikepo$mil40f/bikepo$mil

###(2)Time spent in different travel modes before modal shift####
##Total
bikepo$walkt <- bikepo$milew*10000/(5/60)
#The walking speed was set as 5km/h,
#10000 is the distance unit(10 thousand km), 60 is the conversion of hour and minute in speed

bikepo$biket <- bikepo$mileb*10000/(bikepo$mil/bikepo$wtt)
#The riding speed was extract from actual data

bikepo$cart <- bikepo$milec*10000/(30/60)
#The driving speed was set as 30km/h

##gender-age distribution
#walk
bikepo$walkt25m <- bikepo$milew*bikepo$mil25mr*10000/(5/60)
bikepo$walkt2530m <- bikepo$milew*bikepo$mil2530mr*10000/(5/60)
bikepo$walkt3135m <- bikepo$milew*bikepo$mil3135mr*10000/(5/60)
bikepo$walkt3640m <- bikepo$milew*bikepo$mil3640mr*10000/(5/60)
bikepo$walkt40m <- bikepo$milew*bikepo$mil40mr*10000/(5/60)
bikepo$walkt25f <- bikepo$milew*bikepo$mil25fr*10000/(5/60)
bikepo$walkt2530f <- bikepo$milew*bikepo$mil2530fr*10000/(5/60)
bikepo$walkt3135f <- bikepo$milew*bikepo$mil3135fr*10000/(5/60)
bikepo$walkt3640f <- bikepo$milew*bikepo$mil3640fr*10000/(5/60)
bikepo$walkt40f <- bikepo$milew*bikepo$mil40fr*10000/(5/60)
#riding
bikepo$biket25m <- bikepo$mileb*bikepo$mil25mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket2530m <- bikepo$mileb*bikepo$mil2530mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket3135m <- bikepo$mileb*bikepo$mil3135mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket3640m <- bikepo$mileb*bikepo$mil3640mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket40m <- bikepo$mileb*bikepo$mil40mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket25f <- bikepo$mileb*bikepo$mil25fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket2530f <- bikepo$mileb*bikepo$mil2530fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket3135f <- bikepo$mileb*bikepo$mil3135fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket3640f <- bikepo$mileb*bikepo$mil3640fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket40f <- bikepo$mileb*bikepo$mil40fr*10000/(bikepo$mil/bikepo$wtt)
#driving
bikepo$cart25m <- bikepo$milec*bikepo$mil25mr*10000/(30/60)
bikepo$cart2530m <- bikepo$milec*bikepo$mil2530mr*10000/(30/60)
bikepo$cart3135m <- bikepo$milec*bikepo$mil3135mr*10000/(30/60)
bikepo$cart3640m <- bikepo$milec*bikepo$mil3640mr*10000/(30/60)
bikepo$cart40m <- bikepo$milec*bikepo$mil40mr*10000/(30/60)
bikepo$cart25f <- bikepo$milec*bikepo$mil25fr*10000/(30/60)
bikepo$cart2530f <- bikepo$milec*bikepo$mil2530fr*10000/(30/60)
bikepo$cart3135f <- bikepo$milec*bikepo$mil3135fr*10000/(30/60)
bikepo$cart3640f <- bikepo$milec*bikepo$mil3640fr*10000/(30/60)
bikepo$cart40f <- bikepo$milec*bikepo$mil40fr*10000/(30/60)
###(3)Travel MET-hour before and after travel mode shift per week(changed)####
##Travel MET-hour before travel mode shift per week
bikepo$tmetnoshared25m <- (bikepo$walkt25m*(3.6-1)+bikepo$biket25m*(5.4-1))/(bikepo$wau*10000*bikepo$ager25m)/60
bikepo$tmetnoshared2630m <- (bikepo$walkt2530m*(3.6-1)+bikepo$biket2530m*(5.4-1))/(bikepo$wau*10000*bikepo$ager2630m)/60
bikepo$tmetnoshared3135m <- (bikepo$walkt3135m*(3.6-1)+bikepo$biket3135m*(5.4-1))/(bikepo$wau*10000*bikepo$ager3135m)/60
bikepo$tmetnoshared3640m <- (bikepo$walkt3640m*(3.6-1)+bikepo$biket3640m*(5.4-1))/(bikepo$wau*10000*bikepo$ager3640m)/60
bikepo$tmetnoshared41m <- (bikepo$walkt40m*(3.6-1)+bikepo$biket40m*(5.4-1))/(bikepo$wau*10000*bikepo$ager41m)/60
bikepo$tmetnoshared25f <- (bikepo$walkt25f*(3.6-1)+bikepo$biket25f*(5.4-1))/(bikepo$wau*10000*bikepo$ager25f)/60
bikepo$tmetnoshared2630f <- (bikepo$walkt2530f*(3.6-1)+bikepo$biket2530f*(5.4-1))/(bikepo$wau*10000*bikepo$ager2630f)/60
bikepo$tmetnoshared3135f <- (bikepo$walkt3135f*(3.6-1)+bikepo$biket3135f*(5.4-1))/(bikepo$wau*10000*bikepo$ager3135f)/60
bikepo$tmetnoshared3640f <- (bikepo$walkt3640f*(3.6-1)+bikepo$biket3640f*(5.4-1))/(bikepo$wau*10000*bikepo$ager3640f)/60
bikepo$tmetnoshared41f <- (bikepo$walkt40f*(3.6-1)+bikepo$biket40f*(5.4-1))/(bikepo$wau*10000*bikepo$ager41f)/60
mean(bikepo$tmetnoshared25m,na.rm = T) 
mean(bikepo$tmetnoshared2630m,na.rm = T) 
mean(bikepo$tmetnoshared3135m,na.rm = T) 
mean(bikepo$tmetnoshared3640m,na.rm = T) 
mean(bikepo$tmetnoshared41m,na.rm = T) 
mean(bikepo$tmetnoshared25f,na.rm = T) 
mean(bikepo$tmetnoshared2630f,na.rm = T) 
mean(bikepo$tmetnoshared3135f,na.rm = T) 
mean(bikepo$tmetnoshared3640f,na.rm = T) 
mean(bikepo$tmetnoshared41f,na.rm = T)  
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
mean(bikepo$tmetshared25m,na.rm = T) 
mean(bikepo$tmetshared2630m,na.rm = T) 
mean(bikepo$tmetshared3135m,na.rm = T) 
mean(bikepo$tmetshared3640m,na.rm = T) 
mean(bikepo$tmetshared41m,na.rm = T) 
mean(bikepo$tmetshared25f,na.rm = T) 
mean(bikepo$tmetshared2630f,na.rm = T) 
mean(bikepo$tmetshared3135f,na.rm = T) 
mean(bikepo$tmetshared3640f,na.rm = T) 
mean(bikepo$tmetshared41f,na.rm = T)  
###(4)Output travel MET*hour per week before and after travel mode shift(changed)####
##output mean value and population size wight by age and gender (1=m,2=f)
agegroup<-c("<25","<25","25-30","25-30","31-35","31-35","36-40","36-40","41-59","41-59")
GENDER<-c("1","2","1","2","1","2","1","2","1","2")
tmetnoshared<-c(mean(bikepo$tmetnoshared25m),mean(bikepo$tmetnoshared25f),mean(bikepo$tmetnoshared2630m),mean(bikepo$tmetnoshared2630f),mean(bikepo$tmetnoshared3135m)
                ,mean(bikepo$tmetnoshared3135f),mean(bikepo$tmetnoshared3640m),mean(bikepo$tmetnoshared3640f),mean(bikepo$tmetnoshared41m),mean(bikepo$tmetnoshared41f))
tmetshared<-c(mean(bikepo$tmetshared25m),mean(bikepo$tmetshared25f),mean(bikepo$tmetshared2630m),mean(bikepo$tmetshared2630f),mean(bikepo$tmetshared3135m)
              ,mean(bikepo$tmetshared3135f),mean(bikepo$tmetshared3640m),mean(bikepo$tmetshared3640f),mean(bikepo$tmetshared41m),mean(bikepo$tmetshared41f))
poppro<-c(mean(bikepo$ager25m),mean(bikepo$ager25f),mean(bikepo$ager2630m),mean(bikepo$ager2630f),mean(bikepo$ager3135m)
           ,mean(bikepo$ager3135f),mean(bikepo$ager3640m),mean(bikepo$ager3640f),mean(bikepo$ager41m),mean(bikepo$ager41f))
meanTMET<-data.frame(agegroup,GENDER,poppro,tmetnoshared,tmetshared)
write.csv(meanTMET,file="Mean travel MET per week")

####***********************Calculation of individual non-travel MET-hours**************************####
####*0.Data preparation*####
###(1)Link age, gender, PA by ID####
##Age,gender,PA are scattered in three data sets, but they can be linked by ID

import("PA surveys_pub_12.sas7bdat")->pub
import("PA survey mast_pub_12.sas7bdat")->mast
import("PA survey pact.sas7bdat")->pact
names(pub)
names(mast)
names(pact)
colnames(pact)[1]<-'Idind'

subset(pub,wave==2015 & age<60,select=c("Idind","age"))->pubidage
subset(mast,select=c("Idind","GENDER"))->pubidgen
merge(pubidage,pubidgen,by="Idind")->pubid

subset(pact,WAVE==2015,select=c("Idind",
                                "U127A_MN","U129_MN",
                                "U327_MN","U328_MN","U329_MN","U330_MN","U331_MN","U332_MN","U331b_MN","U332b_MN","U333_MN","U334_MN","U335_MN","U336_MN","U337_MN","U338_MN",
                                "U340_MN","U341_MN","U343_MN","U344_MN","U509_MN","U510_MN","U346_MN","U347_MN","U411_MN","U412_MN","U414_MN","U415_MN","U417_MN","U418_MN","U352_MN","U353_MN","U352A_MN","U353A_MN"
))->pa_noccu1.2015
merge(pubid,pa_noccu1.2015,by="Idind")->PA_noccu2.2015
#Age are grouped 
agegroup<-cut(PA_noccu2.2015$age,breaks=c(0,25,30,35,40,60),include.lowest=T,labels = c("<25","25-30","31-35","36-40","41-59"))
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
write.csv(PA,file="Non-Occupational PA time_Individual")

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

###(3)Save and describe the individual data####
##Point figure found that some MET values are obviously too large. For adjust, the MET range is selected form 0 to 500 MET*h/week. 8 obs are excluded.
ggplot(PA,aes(x=age,y=MET,color=factor(GENDER)))+geom_point()+xlim(0,100)+labs(y="MET-h/week")
PA%>%filter(MET<500)%>%ggplot(aes(x=age,y=MET,color=factor(GENDER)))+geom_point()+labs(y="MET-h/week")
#MET per week by age and gender
PA%>%filter(MET<500)%>%group_by(agegroup,GENDER)%>%summarise(AverageMET=mean(MET,na.rm = T))
ggplot(PA,aes(x=age,y=MET,color=factor(GENDER)))+geom_smooth()+xlim(0,100)+labs(y="MET-h/week")
write.csv(PA,"Non-occupational PA MET_Individual")

####***********************Merge the mean values of travel MET and individual physical activity data by age and gender**************************####
MerPA <- merge(PA,meanTMET,by=c("agegroup","GENDER"))
MerPA$METnoshared<-MerPA$MET+MerPA$tmetnoshared
MerPA$METshared<-MerPA$MET+MerPA$tmetshared
MerPA%>%filter(METnoshared<500)%>%group_by(agegroup,GENDER)%>%summarise(AverageMETnoshared=mean(METnoshared))
MerPA%>%filter(METshared<500)%>%group_by(agegroup,GENDER)%>%summarise(AverageMETshared=mean(METshared))
MerPA%>%filter(METnoshared<500)%>%group_by(agegroup,GENDER)%>%count()
MerPAf%>%filter(METnoshared<500)%>%group_by(agegroup,GENDER)%>%count()
MerPAf<-subset(MerPA,select=c("Idind","age","agegroup","GENDER","MET","tmetnoshared","tmetshared","METnoshared","METshared"))
MerPAf%>%filter(METnoshared<500)%>%ggplot(aes(x=age,y=METnoshared,color=factor(GENDER)))+geom_smooth()+xlim(0,100)+labs(y="MET-h/week")
MerPAf%>%filter(METshared<500)%>%ggplot(aes(x=age,y=METshared,color=factor(GENDER)))+geom_smooth()+xlim(0,100)+labs(y="MET-h/week")

#density distribution for men and women separately 
##density distribution of no bike shared
MerPAf%>%filter(METnoshared<500)%>%ggplot(aes(x=METnoshared,color=factor(GENDER),fill=factor(GENDER),alpha=0.4))+geom_density()+xlim(0,150)+labs(x="MET-h/week")
##density distribution of bike shared
MerPAf%>%filter(METshared<500)%>%ggplot(aes(x=METshared,color=factor(GENDER),fill=factor(GENDER),alpha=0.))+geom_density()+xlim(0,150)+labs(x="MET-h/week")

#Assign weights to each individual in the dataset for scaling up population size to 59.4 million(mean weekly active user)
poppro<-c(mean(bikepo$ager25m),mean(bikepo$ager25f),mean(bikepo$ager2630m),mean(bikepo$ager2630f),mean(bikepo$ager3135m)
           ,mean(bikepo$ager3135f),mean(bikepo$ager3640m),mean(bikepo$ager3640f),mean(bikepo$ager41m),mean(bikepo$ager41f))
indcount<-MerPA%>%filter(METnoshared<500)%>%group_by(agegroup,GENDER)%>%count()
indcount$popsize<-59.4*1000000*poppro
indcount$popweight<-59.4*1000000*poppro/indcount$n
MerPAff <- merge(MerPAf,indcount,by=c("agegroup","GENDER"))

write.csv(MerPAff,"Merged MET result_Individual")

