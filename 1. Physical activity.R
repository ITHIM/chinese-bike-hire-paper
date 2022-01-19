
####*1.physical activity(PA)*####
#The change of physical activity in travel mode change


####***********************Calculation of MET-mins**************************####

####1.The MET-min before modal shift####
###(1)Total mileage divided by gender and age ratio####
##mileage ratio in gender###
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
bikepo$biket25m <- bikepo$milew*bikepo$mil25mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket2530m <- bikepo$milew*bikepo$mil2530mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket3135m <- bikepo$milew*bikepo$mil3135mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket3640m <- bikepo$milew*bikepo$mil3640mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket40m <- bikepo$milew*bikepo$mil40mr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket25f <- bikepo$milew*bikepo$mil25fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket2530f <- bikepo$milew*bikepo$mil2530fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket3135f <- bikepo$milew*bikepo$mil3135fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket3640f <- bikepo$milew*bikepo$mil3640fr*10000/(bikepo$mil/bikepo$wtt)
bikepo$biket40f <- bikepo$milew*bikepo$mil40fr*10000/(bikepo$mil/bikepo$wtt)
#driving
bikepo$cart25m <- bikepo$milew*bikepo$mil25mr*10000/(30/60)
bikepo$cart2530m <- bikepo$milew*bikepo$mil2530mr*10000/(30/60)
bikepo$cart3135m <- bikepo$milew*bikepo$mil3135mr*10000/(30/60)
bikepo$cart3640m <- bikepo$milew*bikepo$mil3640mr*10000/(30/60)
bikepo$cart40m <- bikepo$milew*bikepo$mil40mr*10000/(30/60)
bikepo$cart25f <- bikepo$milew*bikepo$mil25fr*10000/(30/60)
bikepo$cart2530f <- bikepo$milew*bikepo$mil2530fr*10000/(30/60)
bikepo$cart3135f <- bikepo$milew*bikepo$mil3135fr*10000/(30/60)
bikepo$cart3640f <- bikepo$milew*bikepo$mil3640fr*10000/(30/60)
bikepo$cart40f <- bikepo$milew*bikepo$mil40fr*10000/(30/60)

###(3)The change of MET-mins between modal shift####
##MET-min取值
#A.We obtain METs for walking and cycling from previous literature
#  are 3.6 and 5.4 respectively

#non-travel MET
#Male-age 25:4218.6; 26-30:6303.6; 31-35:7620.9; 36-40:7278.9; 40+:4690.1
#Femle-age 25:2885.0; 26-30:4745.7; 31-35:5470.6; 36-40:5719.4; 40+:2622.6

## MET-mins before travel mode shift per week
bikepo$metnoshared <- ((bikepo$wau*10000*bikepo$ager25m*4218.6+bikepo$walkt25m*3.6+bikepo$biket25m*5.4)+
  (bikepo$wau*10000*bikepo$ager2630m*6303.6+bikepo$walkt2530m*3.6+bikepo$biket2530m*5.4)+
  (bikepo$wau*10000*bikepo$ager3135m*7620.9+bikepo$walkt3135m*3.6+bikepo$biket3135m*5.4)+
  (bikepo$wau*10000*bikepo$ager3640m*7278.9+bikepo$walkt3640m*3.6+bikepo$biket3640m*5.4)+
  (bikepo$wau*10000*bikepo$ager41m*4690.1+bikepo$walkt40m*3.6+bikepo$biket40m*5.4)+
  (bikepo$wau*10000*bikepo$ager25f*2885.0+bikepo$walkt25f*3.6+bikepo$biket25f*5.4)+
  (bikepo$wau*10000*bikepo$ager2630f*4745.7+bikepo$walkt2530f*3.6+bikepo$biket2530f*5.4)+
  (bikepo$wau*10000*bikepo$ager3135f*5470.6+bikepo$walkt3135f*3.6+bikepo$biket3135f*5.4)+
  (bikepo$wau*10000*bikepo$ager3640f*5719.4+bikepo$walkt3640f*3.6+bikepo$biket3640f*5.4)+
  (bikepo$wau*10000*bikepo$ager41f*2622.6+bikepo$walkt40f*3.6+bikepo$biket40f*5.4))/(bikepo$wau*10000)/60
mean(bikepo$metnoshared,na.rm = T) 


##Total MET-mins after travel mode shift per week
bikepo$metshared <- (bikepo$wau*10000*(bikepo$ager25m*4218.6+bikepo$ager2630m*6303.6+bikepo$ager3135m*7620.9+
                                        bikepo$ager3640m*7278.9+bikepo$ager41m*4690.1+bikepo$ager25f*2885.0+
                                        bikepo$ager2630f*4745.7+bikepo$ager3135f*5470.6+bikepo$ager3640f*5719.4+
                                        bikepo$ager41f*2622.6)+bikepo$wtt*10000*5.4)/(bikepo$wau*10000)/60
mean(bikepo$metshared,na.rm = T) 

####(3)Average weekly MET-mins in every province####
#wmetnoshared  --met-mins before travel mode shift per week;
#wmetshared  --met-mins after travel mode shift per week
wmetnoshared <- aggregate(bikepo["metnoshared"],by=list(area=bikepo$area),mean,na.rm=T)
wmetshared <- aggregate(bikepo["metshared"],by=list(area=bikepo$area),mean,na.rm=T)

padata <- merge(wmetnoshared,wmetshared,by="area")

####2.Average weekly user in every Province####
###(1)population assumption####
#According to the ratio of daily average mileage:
# 0-3 walking; 3-5 cycling; 5+ driving
bikepo$acw <- (bikepo$dmilr1+bikepo$dmilr13)*bikepo$wau*10000  #原来走路
bikepo$acb <- bikepo$dmilr35*bikepo$wau*10000  #原来骑车
bikepo$acc <- (bikepo$dmilr57+bikepo$dmilr710+bikepo$dmilr10)*bikepo$wau*10000  #原来开车


###(2)The average number of user####
padata <- aggregate.data.frame(bikepo["wau"]*10000,by=list(area=bikepo$area),mean,na.rm=T) %>%
  merge(padata,by="area")


####3.Obtained reduced health risks from PA ####
#due to the non-liner relationship between PA and health,we used a weight coefficient to convert the RR
#The weight coefficient were obtain from previous literature
#Formula from: https://doi.org/10.1371/journal.pone.0051462
# Reduction in health outcome as a change of trval mode shift = 1 – RR^(a/b).
# a = Scenario MET hours per weeks^Transformation of exposure.
# b = Reference MET hours per week^Transformation of exposure

##A.all-cause mortality
#RR=0.81,weight coefficient=0.25
padata$RRt <- (1-0.81^((padata$metshared^0.25)/(padata$metnoshared^0.25)))/(0.81^((padata$metshared^0.25)/(padata$metnoshared^0.25)))  #骑车在前，无骑车在后
padata$RRtL <- (1-0.76^((padata$metshared^0.25)/(padata$metnoshared^0.25)))/(0.76^((padata$metshared^0.25)/(padata$metnoshared^0.25)))
padata$RRtH <- (1-0.85^((padata$metshared^0.25)/(padata$metnoshared^0.25)))/(0.85^((padata$metshared^0.25)/(padata$metnoshared^0.25)))

##B.cause-specific mortality
padata$cvdRRt <- (1-0.84^((padata$metshared^0.5)/(padata$metnoshared^0.5)))/(0.84^((padata$metshared^0.5)/(padata$metnoshared^0.5)))###心血管疾病,中风，缺血性心脏病###
padata$cvdRRtL <- (1-0.79^((padata$metshared^0.5)/(padata$metnoshared^0.5)))/(0.79^((padata$metshared^0.5)/(padata$metnoshared^0.5)))
padata$cvdRRtH <- (1-0.90^((padata$metshared^0.5)/(padata$metnoshared^0.5)))/(0.90^((padata$metshared^0.5)/(padata$metnoshared^0.5)))

padata$respRRt <- (1-0.88^((padata$metshared^0.5)/(padata$metnoshared^0.5)))/(0.88^((padata$metshared^0.5)/(padata$metnoshared^0.5)))###呼吸系统疾病，COPD，哮喘###
padata$respRRtL <- (1-0.77^((padata$metshared^0.5)/(padata$metnoshared^0.5)))/(0.77^((padata$metshared^0.5)/(padata$metnoshared^0.5)))
padata$respRRtH <- (1-0.99^((padata$metshared^0.5)/(padata$metnoshared^0.5)))/(0.99^((padata$metshared^0.5)/(padata$metnoshared^0.5)))

padata$t2dmRRt <- (1-0.85^((padata$metshared^0.375)/(padata$metnoshared^0.375)))/(0.85^((padata$metshared^0.375)/(padata$metnoshared^0.375)))###糖尿病###
padata$t2dmRRtL <- (1-0.79^((padata$metshared^0.375)/(padata$metnoshared^0.375)))/(0.79^((padata$metshared^0.375)/(padata$metnoshared^0.375)))
padata$t2dmRRtH <- (1-0.91^((padata$metshared^0.375)/(padata$metnoshared^0.375)))/(0.91^((padata$metshared^0.375)/(padata$metnoshared^0.375)))

padata$depRRt <- (1-0.83^((padata$metshared^0.5)/(padata$metnoshared^0.5)))/(0.83^((padata$metshared^0.5)/(padata$metnoshared^0.5)))###抑郁###
padata$depRRtL <- (1-0.79^((padata$metshared^0.5)/(padata$metnoshared^0.5)))/(0.79^((padata$metshared^0.5)/(padata$metnoshared^0.5)))
padata$depRRtH <- (1-0.87^((padata$metshared^0.5)/(padata$metnoshared^0.5)))/(0.87^((padata$metshared^0.5)/(padata$metnoshared^0.5)))

padata$demRRt <- (1-0.72^((padata$metshared^0.5)/(padata$metshared^0.5)))/(0.72^((padata$metshared^0.5)/(padata$metshared^0.5)))###阿兹海默（痴呆）###
padata$demRRtL <- (1-0.60^((padata$metshared^0.5)/(padata$metshared^0.5)))/(0.60^((padata$metshared^0.5)/(padata$metshared^0.5)))
padata$demRRtH <- (1-0.86^((padata$metshared^0.5)/(padata$metshared^0.5)))/(0.86^((padata$metshared^0.5)/(padata$metshared^0.5)))

####4.Calculation of mortality effects and YLL####

###(1)mortality effects####
#mortality rate
dierate <- read.csv("./data/pop-Incidence-mortality.csv",stringsAsFactors = F)
names(dierate)
disdierate <- dierate[,c(1,2,12:20)]
###convert the mortality from year(‰) to week(‰)
names(disdierate)
disdierate[,c(3:11)] <- disdierate[,c(3:11)]/100/365*7
names(disdierate)[3:11] <- paste("w",names(disdierate)[3:11],sep = "")
names(disdierate)

#combind the mortality into phsicial activity data
padata <- left_join(padata,disdierate,by="area")
names(padata)

###Calculate reduced deaths###
#all-cause
padata$benphall <- padata$wau/1000*padata$wtotalm*109*padata$RRt #1000为将人数转为每千人
padata$benphallL <- padata$wau/1000*padata$wtotalm*109*padata$RRtH
padata$benphallH <- padata$wau/1000*padata$wtotalm*109*padata$RRtL
padata$benphallse <- (padata$benphallH-padata$benphallL)/(2*1.96) #根据正态分布假设，计算se

#CVD
padata$benphcvd <- padata$wau/1000*padata$wcvdm*109*padata$cvdRRt
padata$benphcvdL <- padata$wau/1000*padata$wcvdm*109*padata$cvdRRtH
padata$benphcvdH <- padata$wau/1000*padata$wcvdm*109*padata$cvdRRtL
padata$benphcvdse <- (padata$benphcvdH-padata$benphcvdL)/(2*1.96)

#stork
padata$benphstro <- padata$wau/1000*padata$wstrorm*109*padata$cvdRRt
padata$benphstroL <- padata$wau/1000*padata$wstrorm*109*padata$cvdRRtH
padata$benphstroH <- padata$wau/1000*padata$wstrorm*109*padata$cvdRRtL
padata$benphstrose <- (padata$benphstroH-padata$benphstroL)/(2*1.96)

#IHD
padata$benphihd <- padata$wau/1000*padata$wihdrm*109*padata$cvdRRt
padata$benphihdL <- padata$wau/1000*padata$wihdrm*109*padata$cvdRRtH
padata$benphihdH <- padata$wau/1000*padata$wihdrm*109*padata$cvdRRtL
padata$benphihdse <- (padata$benphihdH-padata$benphihdL)/(2*1.96)

#RESP
padata$benphresp <- padata$wau/1000*padata$wrespm*109*padata$respRRt
padata$benphrespL <- padata$wau/1000*padata$wrespm*109*padata$respRRtH
padata$benphrespH <- padata$wau/1000*padata$wrespm*109*padata$respRRtL
padata$benphrespse <- (padata$benphrespH-padata$benphrespL)/(2*1.96)

#COPD
padata$benphcopd <- padata$wau/1000*padata$wcopdm*109*padata$respRRt
padata$benphcopdL <- padata$wau/1000*padata$wcopdm*109*padata$respRRtH
padata$benphcopdH <- padata$wau/1000*padata$wcopdm*109*padata$respRRtL
padata$benphcopdse <- (padata$benphcopdH-padata$benphcopdL)/(2*1.96)

#asthma
padata$benphastma <- padata$wau/1000*padata$washm*109*padata$respRRt
padata$benphastmaL <- padata$wau/1000*padata$washm*109*padata$respRRtH
padata$benphastmaH <- padata$wau/1000*padata$washm*109*padata$respRRtL
padata$benphastmase <- (padata$benphastmaH-padata$benphastmaL)/(2*1.96)

#T2DM
padata$benpht2dm <- padata$wau/1000*padata$wt2dmrm*109*padata$t2dmRRt
padata$benpht2dmL <- padata$wau/1000*padata$wt2dmrm*109*padata$t2dmRRtH
padata$benpht2dmH <- padata$wau/1000*padata$wt2dmrm*109*padata$t2dmRRtL
padata$benpht2dmse <- (padata$benpht2dmH-padata$benpht2dmL)/(2*1.96)

#Alzheimer (Dementia)
padata$benphdem <- padata$wau/1000*padata$wdemrm*109*padata$demRRt
padata$benphdemL <- padata$wau/1000*padata$wdemrm*109*padata$demRRtH
padata$benphdemH <- padata$wau/1000*padata$wdemrm*109*padata$demRRtL
padata$benphdemse <- (padata$benphdemH-padata$benphdemL)/(2*1.96)


###View the cumulative death benefit of PA
names(padata)
apply(padata[c(33,37,41,45,49,53,57,61,65)],2, sum) 

#Save the result to "padata1"
padata1 <- padata[c(1,33:68)]

###(2)Calculate YLL benefit from mortality benefit####
ylldata <- read.xlsx("./data/YLLdata.xlsx",sheet = 1)
names(ylldata)

#combined the YLL data
padata1 <- left_join(padata1,ylldata,by="area")


#Formula:
#YLL benefit=reduced death*YLL per death
#YLL per death=YLL per 100 thousand people in a year/mortality per 100 thousand people in a year

#all-cause YLL
padata1$yllall <- padata1$benphall*padata1$totyll/(padata$wtotalm/7*365*100)
padata1$yllallL <- padata1$benphallL*padata1$totyll/(padata$wtotalm/7*365*100)
padata1$yllallH <- padata1$benphallH*padata1$totyll/(padata$wtotalm/7*365*100)
padata1$yllallse <- (padata1$yllallH-padata1$yllallL)/(2*1.96)

#CVD YLL
padata1$yllcvd <- padata1$benphcvd*padata1$cvdyll/(padata$wcvdm/7*365*100)
padata1$yllcvdL <- padata1$benphcvdL*padata1$cvdyll/(padata$wcvdm/7*365*100)
padata1$yllcvdH <- padata1$benphcvdH*padata1$cvdyll/(padata$wcvdm/7*365*100)
padata1$yllcvdse <- (padata1$yllcvdH-padata1$yllcvdL)/(2*1.96)

#stork YLL
padata1$yllstro <- padata1$benphstro*padata1$storyll/(padata$wstrorm/7*365*100)
padata1$yllstroL <- padata1$benphstroL*padata1$storyll/(padata$wstrorm/7*365*100)
padata1$yllstroH <- padata1$benphstroH*padata1$storyll/(padata$wstrorm/7*365*100)
padata1$yllstrose <- (padata1$yllstroH-padata1$yllstroL)/(2*1.96)

#IHD YLL
padata1$yllihd <- padata1$benphihd*padata1$ihdyll/(padata$wihdrm/7*365*100)
padata1$yllihdL <- padata1$benphihdL*padata1$ihdyll/(padata$wihdrm/7*365*100)
padata1$yllihdH <- padata1$benphihdH*padata1$ihdyll/(padata$wihdrm/7*365*100)
padata1$yllihdse <- (padata1$yllihdH-padata1$yllihdL)/(2*1.96)

#resp YLL
padata1$yllresp <- padata1$benphresp*padata1$respyll/(padata$wrespm/7*365*100)
padata1$yllrespL <- padata1$benphrespL*padata1$respyll/(padata$wrespm/7*365*100)
padata1$yllrespH <- padata1$benphrespH*padata1$respyll/(padata$wrespm/7*365*100)
padata1$yllrespse <- (padata1$yllrespH-padata1$yllrespL)/(2*1.96)

#COPD YLL
padata1$yllcopd <- padata1$benphcopd*padata1$copdyll/(padata$wcopdm/7*365*100)
padata1$yllcopdL <- padata1$benphcopdL*padata1$copdyll/(padata$wcopdm/7*365*100)
padata1$yllcopdH <- padata1$benphcopdH*padata1$copdyll/(padata$wcopdm/7*365*100)
padata1$yllcopdse <- (padata1$yllcopdH-padata1$yllcopdL)/(2*1.96)

#ashma YLL
padata1$yllastma <- padata1$benphastma*padata1$astyll/(padata$washm/7*365*100)
padata1$yllastmaL <- padata1$benphastmaL*padata1$astyll/(padata$washm/7*365*100)
padata1$yllastmaH <- padata1$benphastmaH*padata1$astyll/(padata$washm/7*365*100)
padata1$yllastmase <- (padata1$yllastmaH-padata1$yllastmaL)/(2*1.96)

#T2DM YLL
padata1$yllt2dm <- padata1$benpht2dm*padata1$t2dmyll/(padata$wt2dmrm/7*365*100)
padata1$yllt2dmL <- padata1$benpht2dmL*padata1$t2dmyll/(padata$wt2dmrm/7*365*100)
padata1$yllt2dmH <- padata1$benpht2dmH*padata1$t2dmyll/(padata$wt2dmrm/7*365*100)
padata1$yllt2dmse <- (padata1$yllt2dmH-padata1$yllt2dmL)/(2*1.96)

#Alzheimer (Dementia) YLL
padata1$ylldem <- padata1$benphdem*padata1$demyll/(padata$wdemrm/7*365*100)
padata1$ylldemL <- padata1$benphdemL*padata1$demyll/(padata$wdemrm/7*365*100)
padata1$ylldemH <- padata1$benphdemH*padata1$demyll/(padata$wdemrm/7*365*100)
padata1$ylldemse <- (padata1$ylldemH-padata1$ylldemL)/(2*1.96)


names(padata1)
padata2 <- padata1[c(1,2:37,47:82)]

write.csv(padata2,"./result/PA-death&YLL.csv",row.names=F)


####5.Effect of PA on morbidity and YLD####
###(1)Calculation of morbidity effects####
###cause-specific morbidity
dismorrate <- read.csv("./data/pop-Incidence-mortality.csv",stringsAsFactors = F)
names(dismorrate)
dismorrate <- dismorrate[c(1:11)] 

dismorrate[,3:11] <- dismorrate[,3:11]/100/365*7
names(dismorrate)[3:11] <- paste("w",names(dismorrate)[3:11],sep = "")

##combind the morbidity into phsicial activity data
names(padata)
padatai <- padata[1:22]  #RR
padatai <- left_join(padatai,dismorrate,by="area")
names(padatai)

##Calculate reduced incident cases
#Formula:reduced incident cases=average weekly users*morbidity*reduced health risks*number of week

#CVD
padatai$benphcvdi <- padatai$wau/1000*padatai$wcvdi*109*padatai$cvdRRt
padatai$benphcvdiL <- padatai$wau/1000*padatai$wcvdi*109*padatai$cvdRRtH
padatai$benphcvdiH <- padatai$wau/1000*padatai$wcvdi*109*padatai$cvdRRtL
padatai$benphcvdise <- (padatai$benphcvdiH-padatai$benphcvdiL)/(2*1.96)

#strok
padatai$benphstroi <- padatai$wau/1000*padatai$wstrori*109*padatai$cvdRRt
padatai$benphstroiL <- padatai$wau/1000*padatai$wstrori*109*padatai$cvdRRtH
padatai$benphstroiH <- padatai$wau/1000*padatai$wstrori*109*padatai$cvdRRtL
padatai$benphstroise <- (padatai$benphstroiH-padatai$benphstroiL)/(2*1.96)

#IHD
padatai$benphihdi <- padatai$wau/1000*padatai$wihdri*109*padatai$cvdRRt
padatai$benphihdiL <- padatai$wau/1000*padatai$wihdri*109*padatai$cvdRRtH
padatai$benphihdiH <- padatai$wau/1000*padatai$wihdri*109*padatai$cvdRRtL
padatai$benphihdise <- (padatai$benphihdiH-padatai$benphihdiL)/(2*1.96)

#RESP
padatai$benphrespi <- padatai$wau/1000*padatai$wrespi*109*padatai$respRRt
padatai$benphrespiL <- padatai$wau/1000*padatai$wrespi*109*padatai$respRRtH
padatai$benphrespiH <- padatai$wau/1000*padatai$wrespi*109*padatai$respRRtL
padatai$benphrespise <- (padatai$benphrespiH-padatai$benphrespiL)/(2*1.96)

#COPD
padatai$benphcopdi <- padatai$wau/1000*padatai$wcopdi*109*padatai$respRRt
padatai$benphcopdiL <- padatai$wau/1000*padatai$wcopdi*109*padatai$respRRtH
padatai$benphcopdiH <- padatai$wau/1000*padatai$wcopdi*109*padatai$respRRtL
padatai$benphcopdise <- (padatai$benphcopdiH-padatai$benphcopdiL)/(2*1.96)

#Ashma
padatai$benphastmai <- padatai$wau/1000*padatai$washi*109*padatai$respRRt
padatai$benphastmaiL <- padatai$wau/1000*padatai$washi*109*padatai$respRRtH
padatai$benphastmaiH <- padatai$wau/1000*padatai$washi*109*padatai$respRRtL
padatai$benphastmaise <- (padatai$benphastmaiH-padatai$benphastmaiL)/(2*1.96)

#T2DM
padatai$benpht2dmi <- padatai$wau/1000*padatai$wt2dmi*109*padatai$t2dmRRt
padatai$benpht2dmiL <- padatai$wau/1000*padatai$wt2dmi*109*padatai$t2dmRRtH
padatai$benpht2dmiH <- padatai$wau/1000*padatai$wt2dmi*109*padatai$t2dmRRtL
padatai$benpht2dmise <- (padatai$benpht2dmiH-padatai$benpht2dmiL)/(2*1.96)

#Alzheimer (Dementia)
padatai$benphdemi <- padatai$wau/1000*padatai$wdemri*109*padatai$demRRt
padatai$benphdemiL <- padatai$wau/1000*padatai$wdemri*109*padatai$demRRtH
padatai$benphdemiH <- padatai$wau/1000*padatai$wdemri*109*padatai$demRRtL
padatai$benphdemise <- (padatai$benphdemiH-padatai$benphdemiL)/(2*1.96)

#depression
padatai$benphdepi <- padatai$wau/1000*padatai$wdepri*109*padatai$depRRt
padatai$benphdepiL <- padatai$wau/1000*padatai$wdepri*109*padatai$depRRtH
padatai$benphdepiH <- padatai$wau/1000*padatai$wdepri*109*padatai$depRRtL
padatai$benphdepise <- (padatai$benphdepiH-padatai$benphdepiL)/(2*1.96)



###数据整理3###
names(padatai)
apply(padatai[c(33,37,41,45,49,53,57,61,65)],2, sum)  #合计发病收益数

padatai1 <- padatai[c(1,33:68)]


###(2)Calculate YLD benefit from morbidity benefit####
ylddata <- read.xlsx("./data/YLDdata.xlsx",sheet = 1)
names(ylddata)

padatai1 <- left_join(padatai1,ylddata,by="area")
sum(is.na(padatai1))

#The calculation idea: 
#Every disease means a certain amount of YLD
#and the reduced incident cases will save the corresponding YLD

#CVD YLD
padatai1$yldcvd <- padatai1$benphcvdi*padatai1$cvdyld/(padatai$wcvdi/7*365*100)
padatai1$yldcvdL <- padatai1$benphcvdiL*padatai1$cvdyld/(padatai$wcvdi/7*365*100)
padatai1$yldcvdH <- padatai1$benphcvdiH*padatai1$cvdyld/(padatai$wcvdi/7*365*100)
padatai1$yldcvdse <- (padatai1$yldcvdH-padatai1$yldcvdL)/(2*1.96)


padatai1$yldstro <- padatai1$benphstroi*padatai1$storyld/(padatai$wstrori/7*365*100)
padatai1$yldstroL <- padatai1$benphstroiL*padatai1$storyld/(padatai$wstrori/7*365*100)
padatai1$yldstroH <- padatai1$benphstroiH*padatai1$storyld/(padatai$wstrori/7*365*100)
padatai1$yldstrose <- (padatai1$yldstroH-padatai1$yldstroL)/(2*1.96)


padatai1$yldihd <- padatai1$benphihdi*padatai1$ihdyld/(padatai$wihdri/7*365*100)
padatai1$yldihdL <- padatai1$benphihdiL*padatai1$ihdyld/(padatai$wihdri/7*365*100)
padatai1$yldihdH <- padatai1$benphihdiH*padatai1$ihdyld/(padatai$wihdri/7*365*100)
padatai1$yldihdse <- (padatai1$yldihdH-padatai1$yldihdL)/(2*1.96)


padatai1$yldresp <- padatai1$benphrespi*padatai1$respyld/(padatai$wrespi/7*365*100)
padatai1$yldrespL <- padatai1$benphrespiL*padatai1$respyld/(padatai$wrespi/7*365*100)
padatai1$yldrespH <- padatai1$benphrespiH*padatai1$respyld/(padatai$wrespi/7*365*100)
padatai1$yldrespse <- (padatai1$yldrespH-padatai1$yldrespL)/(2*1.96)


padatai1$yldcopd <- padatai1$benphcopdi*padatai1$copdyld/(padatai$wcopdi/7*365*100)
padatai1$yldcopdL <- padatai1$benphcopdiL*padatai1$copdyld/(padatai$wcopdi/7*365*100)
padatai1$yldcopdH <- padatai1$benphcopdiH*padatai1$copdyld/(padatai$wcopdi/7*365*100)
padatai1$yldcopdse <- (padatai1$yldcopdH-padatai1$yldcopdL)/(2*1.96)


padatai1$yldastma <- padatai1$benphastmai*padatai1$astyld/(padatai$washi/7*365*100)
padatai1$yldastmaL <- padatai1$benphastmaiL*padatai1$astyld/(padatai$washi/7*365*100)
padatai1$yldastmaH <- padatai1$benphastmaiH*padatai1$astyld/(padatai$washi/7*365*100)
padatai1$yldastmase <- (padatai1$yldastmaH-padatai1$yldastmaL)/(2*1.96)


padatai1$yldt2dm <- padatai1$benpht2dmi*padatai1$t2dmyld/(padatai$wt2dmi/7*365*100)
padatai1$yldt2dmL <- padatai1$benpht2dmiL*padatai1$t2dmyld/(padatai$wt2dmi/7*365*100)
padatai1$yldt2dmH <- padatai1$benpht2dmiH*padatai1$t2dmyld/(padatai$wt2dmi/7*365*100)
padatai1$yldt2dmse <- (padatai1$yldt2dmH-padatai1$yldt2dmL)/(2*1.96)


padatai1$ylddem <- padatai1$benphdemi*padatai1$demyld/(padatai$wdemri/7*365*100)
padatai1$ylddemL <- padatai1$benphdemiL*padatai1$demyld/(padatai$wdemri/7*365*100)
padatai1$ylddemH <- padatai1$benphdemiH*padatai1$demyld/(padatai$wdemri/7*365*100)
padatai1$ylddemse <- (padatai1$ylddemH-padatai1$ylddemL)/(2*1.96)


padatai1$ylddep <- padatai1$benphdepi*padatai1$depyld/(padatai$wdepri/7*365*100)
padatai1$ylddepL <- padatai1$benphdepiL*padatai1$depyld/(padatai$wdepri/7*365*100)
padatai1$ylddepH <- padatai1$benphdepiH*padatai1$depyld/(padatai$wdepri/7*365*100)
padatai1$ylddepse <- (padatai1$ylddepH-padatai1$ylddepL)/(2*1.96)



names(padatai1)