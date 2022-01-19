

#############################################
#####<3>Air pollution direct impact##########

##focus on PM2.5

####1.Calculate the PM2.5 exposure of different travel modes####

###(1)Calculate respiratory volume(L/min)####
##A.Basal metabolic rate##
#We first calculate the per capita comprehensive basal metabolic rate,
# according to the formula from the literature

#Basal metabolic rate for men under 30 years old, 
#formula: 63BW + 2896, body weight (BW) is 66.2Kg
bikepo$BMRm30d <- 63*66.2+2896

#For males over 30 years old, 48BW+3653, BW is 66.2Kg
bikepo$BMRm30u <- 48*66.2+3653  

#For females under 30 years old, 62BW＋2036, BW is 57.3Kg
bikepo$BMRf30d <- 62*57.3+2036

#For females over 30 years old, 34BW＋3538, BW is 57.3Kg
bikepo$BMRf30u <- 34*57.3+3538 

names(bikepo)
#Number of male and female users
bikepo$waum <- bikepo$wau*bikepo$mr
bikepo$wauf <- bikepo$wau*bikepo$fr
#According to the user ratio, calculate the per capita comprehensive basal metabolic rate
bikepo$BMR <- (bikepo$BMRm30d*(bikepo$ager25m+bikepo$ager2630m)*bikepo$waum*10000+
                 bikepo$BMRm30u*(bikepo$ager3135m+bikepo$ager3640m+bikepo$ager41m)*bikepo$waum*10000+
                 bikepo$BMRf30d*(bikepo$ager25f+bikepo$ager2630f)*bikepo$wauf*10000+
                 bikepo$BMRf30u*(bikepo$ager3135f+bikepo$ager3640f+bikepo$ager41f)*bikepo$wauf*10000)/(bikepo$wau*10000)

##B.Per capita respiratory volume##
bikepo$IRw <- (bikepo$BMR*0.05*27*1.5)/1440  #走路的人均呼吸量
bikepo$IRb <- (bikepo$BMR*0.05*27*4)/1440  #骑车的人均呼吸量
bikepo$IRc <- (bikepo$BMR*0.05*27*1.2)/1440  #开车的人均呼吸量
bikepo$IRrest <- (bikepo$BMR*0.05*27*1.0)/1440  #休息、睡眠时的人均呼吸量
bikepo$IRsit <- (bikepo$BMR*0.05*27*1.2)/1440 #工作时的人均呼吸量
#Formula: IR=(BMR*E*VQ*N)/1440
#The oxygen consumption per unit energy metabolism (E) is 0.05L/KJ
#the ventilation equivalent (VQ) is 27
#Take the N values of 1.5, 4, 1.2, 1 and 1.2 for walking, biking, driving, resting and working.


####(2)Calculation of long-term exposure changes in the concentration of pollutants####
#We estimate with 1 day's activity (24 hours)
#Only those who are walking and driving before travel mode shift changed their inhalation
#The change of air pollution inhalation was caused by the change of respiratory volume

##A.inhalation before travel mode shift
#I.The pollutant data is the monitoring data, 
#and the actual data should be multiplied by the coefficient: 1.6 for walking, 2.0 for cycling, 2.5 for driving
#II.24 hours is divided into 3 parts: commuting, sleep(8 hours) and other time (usually sitting and working)

bikepo$EPM2.5w <- bikepo$pm2.5*1.6*((bikepo$IRw*bikepo$walkt/7)/1000)+(bikepo$acw*8*60*bikepo$IRrest)/1000*bikepo$pm2.5+
  ((bikepo$acw*16*60-bikepo$walkt)*bikepo$IRsit)/1000*bikepo$pm2.5

bikepo$EPM2.5wb <- bikepo$pm2.5*2.0*((bikepo$IRb*((bikepo$milew*10000)/(bikepo$mil/bikepo$wtt))/7)/1000)+(bikepo$acw*8*60*bikepo$IRrest)/1000*bikepo$pm2.5+
  (bikepo$acw*16*60-(bikepo$milew*10000/(bikepo$mil/bikepo$wtt)))*bikepo$IRsit*bikepo$pm2.5/1000

bikepo$EPM2.5c <- bikepo$pm2.5*2.5*0.58*((bikepo$IRw*bikepo$cart/7)/1000)+(bikepo$acc*8*60*bikepo$IRrest)/1000*bikepo$pm2.5+
  ((bikepo$acc*16*60-bikepo$cart)*bikepo$IRsit)/1000*bikepo$pm2.5

bikepo$EPM2.5cb <- bikepo$pm2.5*2.0*((bikepo$IRw*(bikepo$milec*10000/(bikepo$mil/bikepo$wtt))/7)/1000)+(bikepo$acc*8*60*bikepo$IRrest)/1000*bikepo$pm2.5+
  (bikepo$acc*16*60-(bikepo$milec*10000/(bikepo$mil/bikepo$wtt)))*bikepo$IRsit*bikepo$pm2.5/1000

##B.Exposure concentration calculation##
#
bikepo$cPM2.5nosharedw <- bikepo$EPM2.5w/bikepo$acw/24#无共享单车，走路的暴露
bikepo$cPM2.5sharedw <- bikepo$EPM2.5wb/bikepo$acw/24  #有共享单车，走路转为骑车的暴露

bikepo$cPM2.5nosharedc <- bikepo$EPM2.5c/bikepo$acc/24#无共享单车，开车的暴露
bikepo$cPM2.5sharedc <- bikepo$EPM2.5cb/bikepo$acc/24  #有共享单车，开车转为骑车的暴露

##C.Long-term exposure concentration changes##
pro <- unique(bikepo$area)
airdirldata <- data.frame()
for (i in 1:length(pro)) {
  print(pro[i])
  subd <- subset(bikepo,area==pro[i])
  subd1 <- data.frame(area=pro[i],
                      acw=mean(subd$acw,na.rm = T),
                      acb=mean(subd$acb,na.rm = T),
                      acc=mean(subd$acc,na.rm = T),
                      #人均长期暴露浓度
                      lcPM2.5nosharedw = mean(subd$cPM2.5nosharedw,na.rm = T),
                      lcPM2.5sharedw = mean(subd$cPM2.5sharedw,na.rm = T),
                      lcPM2.5nosharedc = mean(subd$cPM2.5nosharedc,na.rm = T),
                      lcPM2.5sharedc = mean(subd$cPM2.5sharedc,na.rm = T))
  airdirldata <- rbind(airdirldata,subd1)
}

mean(airdirldata$lcPM2.5nosharedw-airdirldata$lcPM2.5sharedw)
mean(airdirldata$lcPM2.5nosharedc-airdirldata$lcPM2.5sharedc)

####2.RR value of different exposure concentrations####
##base on Global Exposure Mortality Model (GEMM)
#theta,alpha,miu,pai
RRcal <- function(con,theta,alpha,miu,pai){
  rr <- exp((log(1+((con-2.4)/alpha)))/(1+exp((miu-(con-2.4))/pai))*theta)
  return(rr)
}


##(1)Total RR####
#walking
airdirldata$nosharedw.totRR <- ifelse(airdirldata$lcPM2.5nosharedw < 2.4,1,
                                      RRcal(con=airdirldata$lcPM2.5nosharedw,
                                            theta = 0.143,alpha = 1.6,miu = 15.5,pai = 36.8))
airdirldata$nosharedw.totRRL <- ifelse(airdirldata$lcPM2.5nosharedw < 2.4,1,
                                      RRcal(con=airdirldata$lcPM2.5nosharedw,
                                            theta = 0.143-1.96*0.01807,alpha = 1.6,miu = 15.5,pai = 36.8))
airdirldata$nosharedw.totRRH <- ifelse(airdirldata$lcPM2.5nosharedw < 2.4,1,
                                       RRcal(con=airdirldata$lcPM2.5nosharedw,
                                             theta = 0.143+1.96*0.01807,alpha = 1.6,miu = 15.5,pai = 36.8))
#Driving
airdirldata$nosharedc.totRR <- ifelse(airdirldata$lcPM2.5nosharedc < 2.4,1,
                                      RRcal(con=airdirldata$lcPM2.5nosharedc,
                                            theta = 0.143,alpha = 1.6,miu = 15.5,pai = 36.8))
airdirldata$nosharedc.totRRL <- ifelse(airdirldata$lcPM2.5nosharedc < 2.4,1,
                                       RRcal(con=airdirldata$lcPM2.5nosharedc,
                                             theta = 0.143-1.96*0.01807,alpha = 1.6,miu = 15.5,pai = 36.8))
airdirldata$nosharedc.totRRH <- ifelse(airdirldata$lcPM2.5nosharedc < 2.4,1,
                                       RRcal(con=airdirldata$lcPM2.5nosharedc,
                                             theta = 0.143+1.96*0.01807,alpha = 1.6,miu = 15.5,pai = 36.8))
#Turn from walking to biking
airdirldata$sharedw.totRR <- ifelse(airdirldata$lcPM2.5sharedw < 2.4,1,
                                    RRcal(con=airdirldata$lcPM2.5sharedw,
                                          theta = 0.143,alpha = 1.6,miu = 15.5,pai = 36.8))
airdirldata$sharedw.totRRL <- ifelse(airdirldata$lcPM2.5sharedw < 2.4,1,
                                     RRcal(con=airdirldata$lcPM2.5sharedw,
                                           theta = 0.143-1.96*0.01807,alpha = 1.6,miu = 15.5,pai = 36.8))
airdirldata$sharedw.totRRH <- ifelse(airdirldata$lcPM2.5sharedw < 2.4,1,
                                     RRcal(con=airdirldata$lcPM2.5sharedw,
                                           theta = 0.143+1.96*0.01807,alpha = 1.6,miu = 15.5,pai = 36.8))
#Driving to cycling
airdirldata$sharedc.totRR <- ifelse(airdirldata$lcPM2.5sharedc < 2.4,1,
                                    RRcal(con=airdirldata$lcPM2.5sharedc,
                                          theta = 0.143,alpha = 1.6,miu = 15.5,pai = 36.8))
airdirldata$sharedc.totRRL <- ifelse(airdirldata$lcPM2.5sharedc < 2.4,1,
                                     RRcal(con=airdirldata$lcPM2.5sharedc,
                                           theta = 0.143-1.96*0.01807,alpha = 1.6,miu = 15.5,pai = 36.8))
airdirldata$sharedc.totRRH <- ifelse(airdirldata$lcPM2.5sharedc < 2.4,1,
                                     RRcal(con=airdirldata$lcPM2.5sharedc,
                                           theta = 0.143+1.96*0.01807,alpha = 1.6,miu = 15.5,pai = 36.8))

##(2)IHD-RR####
#walk
airdirldata$nosharedw.ihdRR <- ifelse(airdirldata$lcPM2.5nosharedw < 2.4,1,
                                      RRcal(con=airdirldata$lcPM2.5nosharedw,
                                            theta = 0.2969,alpha = 1.9,miu = 12,pai = 40.2))
airdirldata$nosharedw.ihdRRL <- ifelse(airdirldata$lcPM2.5nosharedw < 2.4,1,
                                       RRcal(con=airdirldata$lcPM2.5nosharedw,
                                             theta = 0.2969-1.96*0.01787,alpha = 1.9,miu = 12,pai = 40.2))
airdirldata$nosharedw.ihdRRH <- ifelse(airdirldata$lcPM2.5nosharedw < 2.4,1,
                                       RRcal(con=airdirldata$lcPM2.5nosharedw,
                                             theta = 0.2969+1.96*0.01787,alpha = 1.9,miu = 12,pai = 40.2))
#driving
airdirldata$nosharedc.ihdRR <- ifelse(airdirldata$lcPM2.5nosharedc < 2.4,1,
                                      RRcal(con=airdirldata$lcPM2.5nosharedc,
                                            theta = 0.2969,alpha = 1.9,miu = 12,pai = 40.2))
airdirldata$nosharedc.ihdRRL <- ifelse(airdirldata$lcPM2.5nosharedc < 2.4,1,
                                       RRcal(con=airdirldata$lcPM2.5nosharedc,
                                             theta = 0.2969-1.96*0.01787,alpha = 1.9,miu = 12,pai = 40.2))
airdirldata$nosharedc.ihdRRH <- ifelse(airdirldata$lcPM2.5nosharedc < 2.4,1,
                                       RRcal(con=airdirldata$lcPM2.5nosharedc,
                                             theta = 0.2969+1.96*0.01787,alpha = 1.9,miu = 12,pai = 40.2))
#Turn from walking to biking
airdirldata$sharedw.ihdRR <- ifelse(airdirldata$lcPM2.5sharedw < 2.4,1,
                                    RRcal(con=airdirldata$lcPM2.5sharedw,
                                          theta = 0.2969,alpha = 1.9,miu = 12,pai = 40.2))
airdirldata$sharedw.ihdRRL <- ifelse(airdirldata$lcPM2.5sharedw < 2.4,1,
                                     RRcal(con=airdirldata$lcPM2.5sharedw,
                                           theta = 0.2969-1.96*0.01787,alpha = 1.9,miu = 12,pai = 40.2))
airdirldata$sharedw.ihdRRH <- ifelse(airdirldata$lcPM2.5sharedw < 2.4,1,
                                     RRcal(con=airdirldata$lcPM2.5sharedw,
                                           theta = 0.2969+1.96*0.01787,alpha = 1.9,miu = 12,pai = 40.2))
#Driving to cycling
airdirldata$sharedc.ihdRR <- ifelse(airdirldata$lcPM2.5sharedc < 2.4,1,
                                    RRcal(con=airdirldata$lcPM2.5sharedc,
                                          theta = 0.2969,alpha = 1.9,miu = 12,pai = 40.2))
airdirldata$sharedc.ihdRRL <- ifelse(airdirldata$lcPM2.5sharedc < 2.4,1,
                                     RRcal(con=airdirldata$lcPM2.5sharedc,
                                           theta = 0.2969-1.96*0.01787,alpha = 1.9,miu = 12,pai = 40.2))
airdirldata$sharedc.ihdRRH <- ifelse(airdirldata$lcPM2.5sharedc < 2.4,1,
                                     RRcal(con=airdirldata$lcPM2.5sharedc,
                                           theta = 0.2969+1.96*0.01787,alpha = 1.9,miu = 12,pai = 40.2))


##(3)Stroke-RR####
#
airdirldata$nosharedw.stroRR <- ifelse(airdirldata$lcPM2.5nosharedw < 2.4,1,
                                       RRcal(con=airdirldata$lcPM2.5nosharedw,
                                             theta = 0.2720,alpha = 6.2,miu = 16.7,pai = 23.7))
airdirldata$nosharedw.stroRRL <- ifelse(airdirldata$lcPM2.5nosharedw < 2.4,1,
                                        RRcal(con=airdirldata$lcPM2.5nosharedw,
                                              theta = 0.2720-1.96*0.07697,alpha = 6.2,miu = 16.7,pai = 23.7))
airdirldata$nosharedw.stroRRH <- ifelse(airdirldata$lcPM2.5nosharedw < 2.4,1,
                                        RRcal(con=airdirldata$lcPM2.5nosharedw,
                                              theta = 0.2720+1.96*0.07697,alpha = 6.2,miu = 16.7,pai = 23.7))
#
airdirldata$nosharedc.stroRR <- ifelse(airdirldata$lcPM2.5nosharedc < 2.4,1,
                                       RRcal(con=airdirldata$lcPM2.5nosharedc,
                                             theta = 0.2720,alpha = 6.2,miu = 16.7,pai = 23.7))
airdirldata$nosharedc.stroRRL <- ifelse(airdirldata$lcPM2.5nosharedc < 2.4,1,
                                        RRcal(con=airdirldata$lcPM2.5nosharedc,
                                              theta = 0.2720-1.96*0.07697,alpha = 6.2,miu = 16.7,pai = 23.7))
airdirldata$nosharedc.stroRRH <- ifelse(airdirldata$lcPM2.5nosharedc < 2.4,1,
                                        RRcal(con=airdirldata$lcPM2.5nosharedc,
                                              theta = 0.2720+1.96*0.07697,alpha = 6.2,miu = 16.7,pai = 23.7))
#
airdirldata$sharedw.stroRR <- ifelse(airdirldata$lcPM2.5sharedw < 2.4,1,
                                     RRcal(con=airdirldata$lcPM2.5sharedw,
                                           theta = 0.2720,alpha = 6.2,miu = 16.7,pai = 23.7))
airdirldata$sharedw.stroRRL <- ifelse(airdirldata$lcPM2.5sharedw < 2.4,1,
                                      RRcal(con=airdirldata$lcPM2.5sharedw,
                                            theta = 0.2720-1.96*0.07697,alpha = 6.2,miu = 16.7,pai = 23.7))
airdirldata$sharedw.stroRRH <- ifelse(airdirldata$lcPM2.5sharedw < 2.4,1,
                                      RRcal(con=airdirldata$lcPM2.5sharedw,
                                            theta = 0.2720+1.96*0.07697,alpha = 6.2,miu = 16.7,pai = 23.7))
#
airdirldata$sharedc.stroRR <- ifelse(airdirldata$lcPM2.5sharedc < 2.4,1,
                                     RRcal(con=airdirldata$lcPM2.5sharedc,
                                           theta = 0.2720,alpha = 6.2,miu = 16.7,pai = 23.7))
airdirldata$sharedc.stroRRL <- ifelse(airdirldata$lcPM2.5sharedc < 2.4,1,
                                      RRcal(con=airdirldata$lcPM2.5sharedc,
                                            theta = 0.2720-1.96*0.07697,alpha = 6.2,miu = 16.7,pai = 23.7))
airdirldata$sharedc.stroRRH <- ifelse(airdirldata$lcPM2.5sharedc < 2.4,1,
                                      RRcal(con=airdirldata$lcPM2.5sharedc,
                                            theta = 0.2720+1.96*0.07697,alpha = 6.2,miu = 16.7,pai = 23.7))
##(4)COPD-RR####
#
airdirldata$nosharedw.copdRR <- ifelse(airdirldata$lcPM2.5nosharedw < 2.4,1,
                                       RRcal(con=airdirldata$lcPM2.5nosharedw,
                                             theta = 0.2510,alpha = 6.5,miu = 2.5,pai = 32))
airdirldata$nosharedw.copdRRL <- ifelse(airdirldata$lcPM2.5nosharedw < 2.4,1,
                                        RRcal(con=airdirldata$lcPM2.5nosharedw,
                                              theta = 0.2510-1.96*0.06762,alpha = 6.5,miu = 2.5,pai = 32))
airdirldata$nosharedw.copdRRH <- ifelse(airdirldata$lcPM2.5nosharedw < 2.4,1,
                                        RRcal(con=airdirldata$lcPM2.5nosharedw,
                                              theta = 0.2510+1.96*0.06762,alpha = 6.5,miu = 2.5,pai = 32))
#
airdirldata$nosharedc.copdRR <- ifelse(airdirldata$lcPM2.5nosharedc < 2.4,1,
                                       RRcal(con=airdirldata$lcPM2.5nosharedc,
                                             theta = 0.2510,alpha = 6.5,miu = 2.5,pai = 32))
airdirldata$nosharedc.copdRRL <- ifelse(airdirldata$lcPM2.5nosharedc < 2.4,1,
                                        RRcal(con=airdirldata$lcPM2.5nosharedc,
                                              theta = 0.2510-1.96*0.06762,alpha = 6.5,miu = 2.5,pai = 32))
airdirldata$nosharedc.copdRRH <- ifelse(airdirldata$lcPM2.5nosharedc < 2.4,1,
                                        RRcal(con=airdirldata$lcPM2.5nosharedc,
                                              theta = 0.2510+1.96*0.06762,alpha = 6.5,miu = 2.5,pai = 32))
#
airdirldata$sharedw.copdRR <- ifelse(airdirldata$lcPM2.5sharedw < 2.4,1,
                                     RRcal(con=airdirldata$lcPM2.5sharedw,
                                           theta = 0.2510,alpha = 6.5,miu = 2.5,pai = 32))
airdirldata$sharedw.copdRRL <- ifelse(airdirldata$lcPM2.5sharedw < 2.4,1,
                                      RRcal(con=airdirldata$lcPM2.5sharedw,
                                            theta = 0.2510-1.96*0.06762,alpha = 6.5,miu = 2.5,pai = 32))
airdirldata$sharedw.copdRRH <- ifelse(airdirldata$lcPM2.5sharedw < 2.4,1,
                                      RRcal(con=airdirldata$lcPM2.5sharedw,
                                            theta = 0.2510+1.96*0.06762,alpha = 6.5,miu = 2.5,pai = 32))
#
airdirldata$sharedc.copdRR <- ifelse(airdirldata$lcPM2.5sharedc < 2.4,1,
                                     RRcal(con=airdirldata$lcPM2.5sharedc,
                                           theta = 0.2510,alpha = 6.5,miu = 2.5,pai = 32))
airdirldata$sharedc.copdRRL <- ifelse(airdirldata$lcPM2.5sharedc < 2.4,1,
                                      RRcal(con=airdirldata$lcPM2.5sharedc,
                                            theta = 0.2510-1.96*0.06762,alpha = 6.5,miu = 2.5,pai = 32))
airdirldata$sharedc.copdRRH <- ifelse(airdirldata$lcPM2.5sharedc < 2.4,1,
                                      RRcal(con=airdirldata$lcPM2.5sharedc,
                                            theta = 0.2510+1.96*0.06762,alpha = 6.5,miu = 2.5,pai = 32))

####3.mortality and YLL changes####
####(1)Attributed deaths and YLL calculation####

###Match mortality and YLL data###
airdirldata <- left_join(airdirldata,disdierate,by="area") %>%
  left_join(ylldata,by="area")
names(airdirldata)
attach(airdirldata)

###A.all-cause mortality####
##Number of deaths without shared bicycles##
##Formula: number of user per week * death rate per week * 109 weeks * (RR-1)
airdirldata$dienosharedw <- acw*wtotalm/1000*109*(nosharedw.totRR-1)
airdirldata$dienosharedwL <- acw*wtotalm/1000*109*(nosharedw.totRRL-1)
airdirldata$dienosharedwH <- acw*wtotalm/1000*109*(nosharedw.totRRH-1)

airdirldata$dienosharedc <- acc*wtotalm/1000*109*(nosharedc.totRR-1)
airdirldata$dienosharedcL <- acc*wtotalm/1000*109*(nosharedc.totRRL-1)
airdirldata$dienosharedcH <- acc*wtotalm/1000*109*(nosharedc.totRRH-1)

##Number of deaths with shared bicycles##
airdirldata$diesharedw <- acw*wtotalm/1000*109*(sharedw.totRR-1)
airdirldata$diesharedwL <- acw*wtotalm/1000*109*(sharedw.totRRL-1)
airdirldata$diesharedwH <- acw*wtotalm/1000*109*(sharedw.totRRH-1)

airdirldata$diesharedc <- acc*wtotalm/1000*109*(sharedc.totRR-1)
airdirldata$diesharedcL <- acc*wtotalm/1000*109*(sharedc.totRRL-1)
airdirldata$diesharedcH <- acc*wtotalm/1000*109*(sharedc.totRRH-1)

###B.CVD####
airdirldata$diecvdnosharedw <- acw*wcvdm/1000*109*(nosharedw.stroRR-1)
airdirldata$diecvdnosharedwL <- acw*wcvdm/1000*109*(nosharedw.stroRRL-1)
airdirldata$diecvdnosharedwH <- acw*wcvdm/1000*109*(nosharedw.stroRRH-1)

airdirldata$diecvdnosharedc <- acc*wcvdm/1000*109*(nosharedc.stroRR-1)
airdirldata$diecvdnosharedcL <- acc*wcvdm/1000*109*(nosharedc.stroRRL-1)
airdirldata$diecvdnosharedcH <- acc*wcvdm/1000*109*(nosharedc.stroRRH-1)

#
airdirldata$diecvdsharedw <- acw*wcvdm/1000*109*(sharedw.stroRR-1)
airdirldata$diecvdsharedwL <- acw*wcvdm/1000*109*(sharedw.stroRRL-1)
airdirldata$diecvdsharedwH <- acw*wcvdm/1000*109*(sharedw.stroRRH-1)

airdirldata$diecvdsharedc <- acc*wcvdm/1000*109*(sharedc.stroRR-1)
airdirldata$diecvdsharedcL <- acc*wcvdm/1000*109*(sharedc.stroRRL-1)
airdirldata$diecvdsharedcH <- acc*wcvdm/1000*109*(sharedc.stroRRH-1)

###C.stroke####
airdirldata$diestronosharedw <- acw*wstrorm/1000*109*(nosharedw.stroRR-1)
airdirldata$diestronosharedwL <- acw*wstrorm/1000*109*(nosharedw.stroRRL-1)
airdirldata$diestronosharedwH <- acw*wstrorm/1000*109*(nosharedw.stroRRH-1)

airdirldata$diestronosharedc <- acc*wstrorm/1000*109*(nosharedc.stroRR-1)
airdirldata$diestronosharedcL <- acc*wstrorm/1000*109*(nosharedc.stroRRL-1)
airdirldata$diestronosharedcH <- acc*wstrorm/1000*109*(nosharedc.stroRRH-1)

##
airdirldata$diestrosharedw <- acw*wstrorm/1000*109*(sharedw.stroRR-1)
airdirldata$diestrosharedwL <- acw*wstrorm/1000*109*(sharedw.stroRRL-1)
airdirldata$diestrosharedwH <- acw*wstrorm/1000*109*(sharedw.stroRRH-1)

airdirldata$diestrosharedc <- acc*wstrorm/1000*109*(sharedc.stroRR-1)
airdirldata$diestrosharedcL <- acc*wstrorm/1000*109*(sharedc.stroRRL-1)
airdirldata$diestrosharedcH <- acc*wstrorm/1000*109*(sharedc.stroRRH-1)

###D.IHD####
#
airdirldata$dieihdnosharedw <- acw*wihdrm/1000*109*(nosharedw.ihdRR-1)
airdirldata$dieihdnosharedwL <- acw*wihdrm/1000*109*(nosharedw.ihdRRL-1)
airdirldata$dieihdnosharedwH <- acw*wihdrm/1000*109*(nosharedw.ihdRRH-1)

airdirldata$dieihdnosharedc <- acc*wihdrm/1000*109*(nosharedc.ihdRR-1)
airdirldata$dieihdnosharedcL <- acc*wihdrm/1000*109*(nosharedc.ihdRRL-1)
airdirldata$dieihdnosharedcH <- acc*wihdrm/1000*109*(nosharedc.ihdRRH-1)

#
airdirldata$dieihdsharedw <- acw*wihdrm/1000*109*(sharedw.ihdRR-1)
airdirldata$dieihdsharedwL <- acw*wihdrm/1000*109*(sharedw.ihdRRL-1)
airdirldata$dieihdsharedwH <- acw*wihdrm/1000*109*(sharedw.ihdRRH-1)

airdirldata$dieihdsharedc <- acc*wihdrm/1000*109*(sharedc.ihdRR-1)
airdirldata$dieihdsharedcL <- acc*wihdrm/1000*109*(sharedc.ihdRRL-1)
airdirldata$dieihdsharedcH <- acc*wihdrm/1000*109*(sharedc.ihdRRH-1)

###E.RESP算####
##
airdirldata$dierespnosharedw <- acw*wrespm/1000*109*(nosharedw.copdRR-1)
airdirldata$dierespnosharedwL <- acw*wrespm/1000*109*(nosharedw.copdRRL-1)
airdirldata$dierespnosharedwH <- acw*wrespm/1000*109*(nosharedw.copdRRH-1)

airdirldata$dierespnosharedc <- acc*wrespm/1000*109*(nosharedc.copdRR-1)
airdirldata$dierespnosharedcL <- acc*wrespm/1000*109*(nosharedc.copdRRL-1)
airdirldata$dierespnosharedcH <- acc*wrespm/1000*109*(nosharedc.copdRRH-1)

#
airdirldata$dierespsharedw <- acw*wrespm/1000*109*(sharedw.copdRR-1)
airdirldata$dierespsharedwL <- acw*wrespm/1000*109*(sharedw.copdRRL-1)
airdirldata$dierespsharedwH <- acw*wrespm/1000*109*(sharedw.copdRRH-1)

airdirldata$dierespsharedc <- acc*wrespm/1000*109*(sharedc.copdRR-1)
airdirldata$dierespsharedcL <- acc*wrespm/1000*109*(sharedc.copdRRL-1)
airdirldata$dierespsharedcH <- acc*wrespm/1000*109*(sharedc.copdRRH-1)

###F.COPD####
airdirldata$diecopdnosharedw <- acw*wcopdm/1000*109*(nosharedw.copdRR-1)
airdirldata$diecopdnosharedwL <- acw*wcopdm/1000*109*(nosharedw.copdRRL-1)
airdirldata$diecopdnosharedwH <- acw*wcopdm/1000*109*(nosharedw.copdRRH-1)

airdirldata$diecopdnosharedc <- acc*wcopdm/1000*109*(nosharedc.copdRR-1)
airdirldata$diecopdnosharedcL <- acc*wcopdm/1000*109*(nosharedc.copdRRL-1)
airdirldata$diecopdnosharedcH <- acc*wcopdm/1000*109*(nosharedc.copdRRH-1)

##
airdirldata$diecopdsharedw <- acw*wcopdm/1000*109*(sharedw.copdRR-1)
airdirldata$diecopdsharedwL <- acw*wcopdm/1000*109*(sharedw.copdRRL-1)
airdirldata$diecopdsharedwH <- acw*wcopdm/1000*109*(sharedw.copdRRH-1)

airdirldata$diecopdsharedc <- acc*wcopdm/1000*109*(sharedc.copdRR-1)
airdirldata$diecopdsharedcL <- acc*wcopdm/1000*109*(sharedc.copdRRL-1)
airdirldata$diecopdsharedcH <- acc*wcopdm/1000*109*(sharedc.copdRRH-1)

###G.Asthma####
airdirldata$dieashnosharedw <- acw*washm/1000*109*(nosharedw.copdRR-1)
airdirldata$dieashnosharedwL <- acw*washm/1000*109*(nosharedw.copdRRL-1)
airdirldata$dieashnosharedwH <- acw*washm/1000*109*(nosharedw.copdRRH-1)

airdirldata$dieashnosharedc <- acc*washm/1000*109*(nosharedc.copdRR-1)
airdirldata$dieashnosharedcL <- acc*washm/1000*109*(nosharedc.copdRRL-1)
airdirldata$dieashnosharedcH <- acc*washm/1000*109*(nosharedc.copdRRH-1)

##
airdirldata$dieashsharedw <- acw*washm/1000*109*(sharedw.copdRR-1)
airdirldata$dieashsharedwL <- acw*washm/1000*109*(sharedw.copdRRL-1)
airdirldata$dieashsharedwH <- acw*washm/1000*109*(sharedw.copdRRH-1)

airdirldata$dieashsharedc <- acc*washm/1000*109*(sharedc.copdRR-1)
airdirldata$dieashsharedcL <- acc*washm/1000*109*(sharedc.copdRRL-1)
airdirldata$dieashsharedcH <- acc*washm/1000*109*(sharedc.copdRRH-1)

detach(airdirldata)

####(2)Changed count of deaths####
###A.Various ending calculations####
attach(airdirldata)

#Total
airdirldata$dienoshared <- dienosharedw+dienosharedc
airdirldata$dieshared <- diesharedw+diesharedc
airdirldata$beniapdirtot <- airdirldata$dienoshared-airdirldata$dieshared
airdirldata$beniapdirtotyll <- airdirldata$beniapdirtot*totyll/(wtotalm/7*365*100)


#CVD
airdirldata$diecvdnoshared <- diecvdnosharedw+diecvdnosharedc
airdirldata$diecvdshared <- diecvdsharedw+diecvdsharedc
airdirldata$beniapdircvd <- airdirldata$diecvdnoshared-airdirldata$diecvdshared
airdirldata$beniapdircvdyll <- airdirldata$beniapdircvd*cvdyll/(wcvdm/7*365*100)

#IHD
airdirldata$dieihdnoshared <- dieihdnosharedw+dieihdnosharedc
airdirldata$dieihdshared <- dieihdsharedw+dieihdsharedc
airdirldata$beniapdirihd <- airdirldata$dieihdnoshared-airdirldata$dieihdshared
airdirldata$beniapdirihdyll <- airdirldata$beniapdirihd*ihdyll/(wihdrm/7*365*100)

#stroke
airdirldata$diestronoshared <- diestronosharedw+diestronosharedc
airdirldata$diestroshared <- diestrosharedw+diestrosharedc
airdirldata$beniapdirstro <- airdirldata$diestronoshared-airdirldata$diestroshared
airdirldata$beniapdirstroyll <- airdirldata$beniapdirstro*storyll/(wstrorm/7*365*100)

#RESP
airdirldata$dierespnoshared <- dierespnosharedw+dierespnosharedc
airdirldata$dierespshared <- dierespsharedw+dierespsharedc
airdirldata$beniapdirresp <- airdirldata$dierespnoshared-airdirldata$dierespshared
airdirldata$beniapdirrespyll <- airdirldata$beniapdirresp*respyll/(wrespm/7*365*100)

#COPD
airdirldata$diecopdnoshared <- diecopdnosharedw+diecopdnosharedc
airdirldata$diecopdshared <- diecopdsharedw+diecopdsharedc
airdirldata$beniapdircopd <- airdirldata$diecopdnoshared-airdirldata$diecopdshared
airdirldata$beniapdircopdyll <- airdirldata$beniapdircopd*copdyll/(wcopdm/7*365*100)

#asthma
airdirldata$dieashnoshared <- dieashnosharedw+dieashnosharedc
airdirldata$dieashshared <- dieashsharedw+dieashsharedc
airdirldata$beniapdirash <- airdirldata$dieashnoshared-airdirldata$dieashshared
airdirldata$beniapdirashyll <- airdirldata$beniapdirash*astyll/(washm/7*365*100)
detach(airdirldata)


####4.Morbidity and YLD benefits####

names(airdirldata)
airdirldis <- airdirldata[,c(1:56)]

###Match incidence rate and YLD data###
airdirldis <- left_join(airdirldis,dismorrate,by="area") %>%
  left_join(ylddata,by="area")
names(airdirldis)

####(1)Attributable incidence and YLD calculation####
attach(airdirldis)

###A.CVD attribution calculation####
##No shared bicycle incidence number##
airdirldis$discvdnosharedw <- acw*wcvdi/1000*109*(nosharedw.stroRR-1)
airdirldis$discvdnosharedwL <- acw*wcvdi/1000*109*(nosharedw.stroRRL-1)
airdirldis$discvdnosharedwH <- acw*wcvdi/1000*109*(nosharedw.stroRRH-1)

airdirldis$discvdnosharedc <- acc*wcvdi/1000*109*(nosharedc.stroRR-1)
airdirldis$discvdnosharedcL <- acc*wcvdi/1000*109*(nosharedc.stroRRL-1)
airdirldis$discvdnosharedcH <- acc*wcvdi/1000*109*(nosharedc.stroRRH-1)

##Sharing bicycle incidence number##
airdirldis$discvdsharedw <- acw*wcvdi/1000*109*(sharedw.stroRR-1)
airdirldis$discvdsharedwL <- acw*wcvdi/1000*109*(sharedw.stroRRL-1)
airdirldis$discvdsharedwH <- acw*wcvdi/1000*109*(sharedw.stroRRH-1)

airdirldis$discvdsharedc <- acc*wcvdi/1000*109*(sharedc.stroRR-1)
airdirldis$discvdsharedcL <- acc*wcvdi/1000*109*(sharedc.stroRRL-1)
airdirldis$discvdsharedcH <- acc*wcvdi/1000*109*(sharedc.stroRRH-1)

###C.Stroke####
airdirldis$disstronosharedw <- acw*wstrori/1000*109*(nosharedw.stroRR-1)
airdirldis$disstronosharedwL <- acw*wstrori/1000*109*(nosharedw.stroRRL-1)
airdirldis$disstronosharedwH <- acw*wstrori/1000*109*(nosharedw.stroRRH-1)

airdirldis$disstronosharedc <- acc*wstrori/1000*109*(nosharedc.stroRR-1)
airdirldis$disstronosharedcL <- acc*wstrori/1000*109*(nosharedc.stroRRL-1)
airdirldis$disstronosharedcH <- acc*wstrori/1000*109*(nosharedc.stroRRH-1)

##
airdirldis$disstrosharedw <- acw*wstrori/1000*109*(sharedw.stroRR-1)
airdirldis$disstrosharedwL <- acw*wstrori/1000*109*(sharedw.stroRRL-1)
airdirldis$disstrosharedwH <- acw*wstrori/1000*109*(sharedw.stroRRH-1)

airdirldis$disstrosharedc <- acc*wstrori/1000*109*(sharedc.stroRR-1)
airdirldis$disstrosharedcL <- acc*wstrori/1000*109*(sharedc.stroRRL-1)
airdirldis$disstrosharedcH <- acc*wstrori/1000*109*(sharedc.stroRRH-1)

###D.IHD####
airdirldis$disihdnosharedw <- acw*wihdri/1000*109*(nosharedw.ihdRR-1)
airdirldis$disihdnosharedwL <- acw*wihdri/1000*109*(nosharedw.ihdRRL-1)
airdirldis$disihdnosharedwH <- acw*wihdri/1000*109*(nosharedw.ihdRRH-1)

airdirldis$disihdnosharedc <- acc*wihdri/1000*109*(nosharedc.ihdRR-1)
airdirldis$disihdnosharedcL <- acc*wihdri/1000*109*(nosharedc.ihdRRL-1)
airdirldis$disihdnosharedcH <- acc*wihdri/1000*109*(nosharedc.ihdRRH-1)

##
airdirldis$disihdsharedw <- acw*wihdri/1000*109*(sharedw.ihdRR-1)
airdirldis$disihdsharedwL <- acw*wihdri/1000*109*(sharedw.ihdRRL-1)
airdirldis$disihdsharedwH <- acw*wihdri/1000*109*(sharedw.ihdRRH-1)

airdirldis$disihdsharedc <- acc*wihdri/1000*109*(sharedc.ihdRR-1)
airdirldis$disihdsharedcL <- acc*wihdri/1000*109*(sharedc.ihdRRL-1)
airdirldis$disihdsharedcH <- acc*wihdri/1000*109*(sharedc.ihdRRH-1)

###E.RESP####

airdirldis$disrespnosharedw <- acw*wrespi/1000*109*(nosharedw.copdRR-1)
airdirldis$disrespnosharedwL <- acw*wrespi/1000*109*(nosharedw.copdRRL-1)
airdirldis$disrespnosharedwH <- acw*wrespi/1000*109*(nosharedw.copdRRH-1)

airdirldis$disrespnosharedc <- acc*wrespi/1000*109*(nosharedc.copdRR-1)
airdirldis$disrespnosharedcL <- acc*wrespi/1000*109*(nosharedc.copdRRL-1)
airdirldis$disrespnosharedcH <- acc*wrespi/1000*109*(nosharedc.copdRRH-1)

##
airdirldis$disrespsharedw <- acw*wrespi/1000*109*(sharedw.copdRR-1)
airdirldis$disrespsharedwL <- acw*wrespi/1000*109*(sharedw.copdRRL-1)
airdirldis$disrespsharedwH <- acw*wrespi/1000*109*(sharedw.copdRRH-1)

airdirldis$disrespsharedc <- acc*wrespi/1000*109*(sharedc.copdRR-1)
airdirldis$disrespsharedcL <- acc*wrespi/1000*109*(sharedc.copdRRL-1)
airdirldis$disrespsharedcH <- acc*wrespi/1000*109*(sharedc.copdRRH-1)

###F.COPD####
airdirldis$discopdnosharedw <- acw*wcopdi/1000*109*(nosharedw.copdRR-1)
airdirldis$discopdnosharedwL <- acw*wcopdi/1000*109*(nosharedw.copdRRL-1)
airdirldis$discopdnosharedwH <- acw*wcopdi/1000*109*(nosharedw.copdRRH-1)

airdirldis$discopdnosharedc <- acc*wcopdi/1000*109*(nosharedc.copdRR-1)
airdirldis$discopdnosharedcL <- acc*wcopdi/1000*109*(nosharedc.copdRRL-1)
airdirldis$discopdnosharedcH <- acc*wcopdi/1000*109*(nosharedc.copdRRH-1)

#
airdirldis$discopdsharedw <- acw*wcopdi/1000*109*(sharedw.copdRR-1)
airdirldis$discopdsharedwL <- acw*wcopdi/1000*109*(sharedw.copdRRL-1)
airdirldis$discopdsharedwH <- acw*wcopdi/1000*109*(sharedw.copdRRH-1)

airdirldis$discopdsharedc <- acc*wcopdi/1000*109*(sharedc.copdRR-1)
airdirldis$discopdsharedcL <- acc*wcopdi/1000*109*(sharedc.copdRRL-1)
airdirldis$discopdsharedcH <- acc*wcopdi/1000*109*(sharedc.copdRRH-1)

###G.asthma####
airdirldis$disashnosharedw <- acw*washi/1000*109*(nosharedw.copdRR-1)
airdirldis$disashnosharedwL <- acw*washi/1000*109*(nosharedw.copdRRL-1)
airdirldis$disashnosharedwH <- acw*washi/1000*109*(nosharedw.copdRRH-1)

airdirldis$disashnosharedc <- acc*washi/1000*109*(nosharedc.copdRR-1)
airdirldis$disashnosharedcL <- acc*washi/1000*109*(nosharedc.copdRRL-1)
airdirldis$disashnosharedcH <- acc*washi/1000*109*(nosharedc.copdRRH-1)

##
airdirldis$disashsharedw <- acw*washi/1000*109*(sharedw.copdRR-1)
airdirldis$disashsharedwL <- acw*washi/1000*109*(sharedw.copdRRL-1)
airdirldis$disashsharedwH <- acw*washi/1000*109*(sharedw.copdRRH-1)

airdirldis$disashsharedc <- acc*washi/1000*109*(sharedc.copdRR-1)
airdirldis$disashsharedcL <- acc*washi/1000*109*(sharedc.copdRRL-1)
airdirldis$disashsharedcH <- acc*washi/1000*109*(sharedc.copdRRH-1)

detach(airdirldis)

####(2)Calculation of benefit incidence and YLD####
###A.all type result####
attach(airdirldis)

#CVD
airdirldis$discvdnoshared <- discvdnosharedw+discvdnosharedc
airdirldis$discvdshared <- discvdsharedw+discvdsharedc
airdirldis$beniapdircvd <- airdirldis$discvdnoshared-airdirldis$discvdshared
airdirldis$beniapdircvdyld <- airdirldis$beniapdircvd*cvdyld/(wcvdi/7*365*100)


#IHD
airdirldis$disihdnoshared <- disihdnosharedw+disihdnosharedc
airdirldis$disihdshared <- disihdsharedw+disihdsharedc
airdirldis$beniapdirihd <- airdirldis$disihdnoshared-airdirldis$disihdshared
airdirldis$beniapdirihdyld <- airdirldis$beniapdirihd*ihdyld/(wihdri/7*365*100)

#stroke
airdirldis$disstronoshared <- disstronosharedw+disstronosharedc
airdirldis$disstroshared <- disstrosharedw+disstrosharedc
airdirldis$beniapdirstro <- airdirldis$disstronoshared-airdirldis$disstroshared
airdirldis$beniapdirstroyld <- airdirldis$beniapdirstro*storyld/(wstrori/7*365*100)

#RESP
airdirldis$disrespnoshared <- disrespnosharedw+disrespnosharedc
airdirldis$disrespshared <- disrespsharedw+disrespsharedc
airdirldis$beniapdirresp <- airdirldis$disrespnoshared-airdirldis$disrespshared
airdirldis$beniapdirrespyld <- airdirldis$beniapdirresp*respyld/(wrespi/7*365*100)

#COPD
airdirldis$discopdnoshared <- discopdnosharedw+discopdnosharedc
airdirldis$discopdshared <- discopdsharedw+discopdsharedc
airdirldis$beniapdircopd <- airdirldis$discopdnoshared-airdirldis$discopdshared
airdirldis$beniapdircopdyld <- airdirldis$beniapdircopd*copdyld/(wcopdi/7*365*100)

#asthma
airdirldis$disashnoshared <- disashnosharedw+disashnosharedc
airdirldis$disashshared <- disashsharedw+disashsharedc
airdirldis$beniapdirash <- airdirldis$disashnoshared-airdirldis$disashshared
airdirldis$beniapdirashyld <- airdirldis$beniapdirash*astyld/(washi/7*365*100)
detach(airdirldis)


