

#################################################
#####<4>Air pollution indirect impact##########

#Focus on PM2.5

####1.Proportion of air pollutants from motor vehicles####
##the proportions were obtain from previous literature
bikepo$pm2.5r <- 0.2409

##Gasoline consumption (ten thousand liters)
oildata <- read.csv("./data/oildata.csv",stringsAsFactors = F)
bikepo <- left_join(bikepo,oildata,by="area")

#Converted from annual consumption to weekly
bikepo$woil <- bikepo$oil/365*7

####2.Reduced gasoline consumption and changes in air concentration due to cycling####
##(1)Reduced gasoline consumption by switching to cycling####
#Unit: liters;
#6.9 is the fuel consumption per 100 kilometers
#100 is convert coefficient from "10 thousand kilometers" in milec "100 kilometers"

bikepo$oilsave <- bikepo$milec*100*6.9 

##(2)PM2.5 concentration from motor vehicles####
#Calculated based on the actual concentration, which is the concentration of shared bicycles
bikepo$pm2.5vshared <- bikepo$pm2.5*bikepo$pm2.5r

#When there is no shared bicycle, the PM2.5 concentration of motor vehicle source (v)
bikepo$pm2.5vnoshared <- bikepo$pm2.5vshared*(bikepo$woil*10000/(bikepo$woil*10000-bikepo$oilsave))

#PM2.5 concentration without shared bicycles
bikepo$pm2.5noshared <- bikepo$pm2.5-bikepo$pm2.5vshared+bikepo$pm2.5vnoshared

####3.Calculation of long-term exposure concentration and RR value####
##(1)Long-term exposure concentration calculation####
pro <- unique(bikepo$area)
airindirldata <- data.frame()
for (i in 1:length(pro)) {
  print(pro[i])
  subd <- subset(bikepo,area==pro[i])
  subd1 <- data.frame(area=pro[i],
                      #Per capita exposure concentration
                      lcPM2.5shared = mean(subd$pm2.5,na.rm = T),
                      lcPM2.5noshared = mean(subd$pm2.5noshared,na.rm = T))
  airindirldata <- rbind(airindirldata,subd1)
}

mean(airindirldata$lcPM2.5noshared-airindirldata$lcPM2.5shared)

##(2)Calculate the RR value of the corresponding concentration####
RRcal <- function(con,theta,alpha,miu,pai){
  rr <- exp((log(1+((con-2.4)/alpha)))/(1+exp((miu-(con-2.4))/pai))*theta)
  return(rr)
}

##A.Total RR####
airindirldata$shared.totRR <- ifelse(airindirldata$lcPM2.5shared < 2.4,1,
                                     RRcal(con=airindirldata$lcPM2.5shared,
                                           theta = 0.143,alpha = 1.6,miu = 15.5,pai = 36.8))
airindirldata$shared.totRRL <- ifelse(airindirldata$lcPM2.5shared < 2.4,1,
                                     RRcal(con=airindirldata$lcPM2.5shared,
                                           theta = 0.143-1.96*0.01807,alpha = 1.6,miu = 15.5,pai = 36.8))
airindirldata$shared.totRRH <- ifelse(airindirldata$lcPM2.5shared < 2.4,1,
                                      RRcal(con=airindirldata$lcPM2.5shared,
                                            theta = 0.143+1.96*0.01807,alpha = 1.6,miu = 15.5,pai = 36.8))

airindirldata$noshared.totRR <- ifelse(airindirldata$lcPM2.5noshared < 2.4,1,
                                       RRcal(con=airindirldata$lcPM2.5noshared,
                                             theta = 0.143,alpha = 1.6,miu = 15.5,pai = 36.8))
airindirldata$noshared.totRRL <- ifelse(airindirldata$lcPM2.5noshared < 2.4,1,
                                        RRcal(con=airindirldata$lcPM2.5noshared,
                                              theta = 0.143-1.96*0.01807,alpha = 1.6,miu = 15.5,pai = 36.8))
airindirldata$noshared.totRRH <- ifelse(airindirldata$lcPM2.5noshared < 2.4,1,
                                        RRcal(con=airindirldata$lcPM2.5noshared,
                                              theta = 0.143+1.96*0.01807,alpha = 1.6,miu = 15.5,pai = 36.8))
##B.IHD-RR####
airindirldata$shared.ihdRR <- ifelse(airindirldata$lcPM2.5shared < 2.4,1,
                                     RRcal(con=airindirldata$lcPM2.5shared,
                                           theta = 0.2969,alpha = 1.9,miu = 12,pai = 40.2))
airindirldata$shared.ihdRRL <- ifelse(airindirldata$lcPM2.5shared < 2.4,1,
                                      RRcal(con=airindirldata$lcPM2.5shared,
                                            theta = 0.2969-1.96*0.01787,alpha = 1.9,miu = 12,pai = 40.2))
airindirldata$shared.ihdRRH <- ifelse(airindirldata$lcPM2.5shared < 2.4,1,
                                      RRcal(con=airindirldata$lcPM2.5shared,
                                            theta = 0.2969+1.96*0.01787,alpha = 1.9,miu = 12,pai = 40.2))

airindirldata$noshared.ihdRR <- ifelse(airindirldata$lcPM2.5noshared < 2.4,1,
                                       RRcal(con=airindirldata$lcPM2.5noshared,
                                             theta = 0.2969,alpha = 1.9,miu = 12,pai = 40.2))
airindirldata$noshared.ihdRRL <- ifelse(airindirldata$lcPM2.5noshared < 2.4,1,
                                        RRcal(con=airindirldata$lcPM2.5noshared,
                                              theta = 0.2969-1.96*0.01787,alpha = 1.9,miu = 12,pai = 40.2))
airindirldata$noshared.ihdRRH <- ifelse(airindirldata$lcPM2.5noshared < 2.4,1,
                                        RRcal(con=airindirldata$lcPM2.5noshared,
                                              theta = 0.2969+1.96*0.01787,alpha = 1.9,miu = 12,pai = 40.2))

##C.Stroke-RR####
airindirldata$shared.stroRR <- ifelse(airindirldata$lcPM2.5shared < 2.4,1,
                                      RRcal(con=airindirldata$lcPM2.5shared,
                                            theta = 0.2720,alpha = 6.2,miu = 16.7,pai = 23.7))
airindirldata$shared.stroRRL <- ifelse(airindirldata$lcPM2.5shared < 2.4,1,
                                       RRcal(con=airindirldata$lcPM2.5shared,
                                             theta = 0.2720-1.96*0.07697,alpha = 6.2,miu = 16.7,pai = 23.7))
airindirldata$shared.stroRRH <- ifelse(airindirldata$lcPM2.5shared < 2.4,1,
                                       RRcal(con=airindirldata$lcPM2.5shared,
                                             theta = 0.2720+1.96*0.07697,alpha = 6.2,miu = 16.7,pai = 23.7))

airindirldata$noshared.stroRR <- ifelse(airindirldata$lcPM2.5noshared < 2.4,1,
                                        RRcal(con=airindirldata$lcPM2.5noshared,
                                              theta = 0.2720,alpha = 6.2,miu = 16.7,pai = 23.7))
airindirldata$noshared.stroRRL <- ifelse(airindirldata$lcPM2.5noshared < 2.4,1,
                                         RRcal(con=airindirldata$lcPM2.5noshared,
                                               theta = 0.2720-1.96*0.07697,alpha = 6.2,miu = 16.7,pai = 23.7))
airindirldata$noshared.stroRRH <- ifelse(airindirldata$lcPM2.5noshared < 2.4,1,
                                         RRcal(con=airindirldata$lcPM2.5noshared,
                                               theta = 0.2720+1.96*0.07697,alpha = 6.2,miu = 16.7,pai = 23.7))
##D.COPD-RR####
airindirldata$shared.copdRR <- ifelse(airindirldata$lcPM2.5shared < 2.4,1,
                                      RRcal(con=airindirldata$lcPM2.5shared,
                                            theta = 0.2510,alpha = 6.5,miu = 2.5,pai = 32))
airindirldata$shared.copdRRL <- ifelse(airindirldata$lcPM2.5shared < 2.4,1,
                                       RRcal(con=airindirldata$lcPM2.5shared,
                                             theta = 0.2510-1.96*0.06762,alpha = 6.5,miu = 2.5,pai = 32))
airindirldata$shared.copdRRH <- ifelse(airindirldata$lcPM2.5shared < 2.4,1,
                                       RRcal(con=airindirldata$lcPM2.5shared,
                                             theta = 0.2510+1.96*0.06762,alpha = 6.5,miu = 2.5,pai = 32))

airindirldata$noshared.copdRR <- ifelse(airindirldata$lcPM2.5noshared < 2.4,1,
                                        RRcal(con=airindirldata$lcPM2.5noshared,
                                              theta = 0.2510,alpha = 6.5,miu = 2.5,pai = 32))
airindirldata$noshared.copdRRL <- ifelse(airindirldata$lcPM2.5noshared < 2.4,1,
                                         RRcal(con=airindirldata$lcPM2.5noshared,
                                               theta = 0.2510-1.96*0.06762,alpha = 6.5,miu = 2.5,pai = 32))
airindirldata$noshared.copdRRH <- ifelse(airindirldata$lcPM2.5noshared < 2.4,1,
                                         RRcal(con=airindirldata$lcPM2.5noshared,
                                               theta = 0.2510+1.96*0.06762,alpha = 6.5,miu = 2.5,pai = 32))


####The following process is consistent with direct pollution exposure, 
#except that the user was transferred to the population


####4.Death and YLL income calculation####
#匹配人口数、死亡率和YLL数据
airindirldata <- left_join(airindirldata,disdierate,by="area") %>%
  left_join(ylldata,by="area")

####(1)归因死亡数计算####
names(airindirldata)
attach(airindirldata)
###A.总死亡归因计算####
##无共享单车下，空气污染导致的人群死亡数
#人口数*每周死亡率*109周*(RR-1)
airindirldata$dienoshared <- pop*10*wtotalm*(noshared.totRR-1)*109
airindirldata$dienosharedL <- pop*10*wtotalm*(noshared.totRRL-1)*109
airindirldata$dienosharedH <- pop*10*wtotalm*(noshared.totRRH-1)*109

##有共享单车
airindirldata$dieshared <- pop*10*wtotalm*(shared.totRR-1)*109
airindirldata$diesharedL <- pop*10*wtotalm*(shared.totRRL-1)*109
airindirldata$diesharedH <- pop*10*wtotalm*(shared.totRRH-1)*109

###B.CVD归因计算####
##无共享单车
airindirldata$diecvdnoshared <- pop*10*wcvdm*(noshared.stroRR-1)*109
airindirldata$diecvdnosharedL <- pop*10*wcvdm*(noshared.stroRRL-1)*109
airindirldata$diecvdnosharedH <- pop*10*wcvdm*(noshared.stroRRH-1)*109

##有共享单车
airindirldata$diecvdshared <- pop*10*wcvdm*(shared.stroRR-1)*109
airindirldata$diecvdsharedL <- pop*10*wcvdm*(shared.stroRRL-1)*109
airindirldata$diecvdsharedH <- pop*10*wcvdm*(shared.stroRRH-1)*109

###C.中风归因计算####
##无共享单车
airindirldata$diestronoshared <- pop*10*wstrorm*(noshared.stroRR-1)*109
airindirldata$diestronosharedL <- pop*10*wstrorm*(noshared.stroRRL-1)*109
airindirldata$diestronosharedH <- pop*10*wstrorm*(noshared.stroRRH-1)*109

##有共享单车
airindirldata$diestroshared <- pop*10*wstrorm*(shared.stroRR-1)*109
airindirldata$diestrosharedL <- pop*10*wstrorm*(shared.stroRRL-1)*109
airindirldata$diestrosharedH <- pop*10*wstrorm*(shared.stroRRH-1)*109


###D.IHD归因计算####
##无共享单车
airindirldata$dieihdnoshared <- pop*10*wihdrm*(noshared.ihdRR-1)*109
airindirldata$dieihdnosharedL <- pop*10*wihdrm*(noshared.ihdRRL-1)*109
airindirldata$dieihdnosharedH <- pop*10*wihdrm*(noshared.ihdRRH-1)*109

##有共享单车
airindirldata$dieihdshared <- pop*10*wihdrm*(shared.ihdRR-1)*109
airindirldata$dieihdsharedL <- pop*10*wihdrm*(shared.ihdRRL-1)*109
airindirldata$dieihdsharedH <- pop*10*wihdrm*(shared.ihdRRH-1)*109

###E.RESP归因计算####
##无共享单车
airindirldata$dierespnoshared <- pop*10*wrespm*(noshared.copdRR-1)*109
airindirldata$dierespnosharedL <- pop*10*wrespm*(noshared.copdRRL-1)*109
airindirldata$dierespnosharedH <- pop*10*wrespm*(noshared.copdRRH-1)*109

##有共享单车
airindirldata$dierespshared <- pop*10*wrespm*(shared.copdRR-1)*109
airindirldata$dierespsharedL <- pop*10*wrespm*(shared.copdRRL-1)*109
airindirldata$dierespsharedH <- pop*10*wrespm*(shared.copdRRH-1)*109

###F.COPD归因计算####
##无共享单车
airindirldata$diecopdnoshared <- pop*10*wcopdm*(noshared.copdRR-1)*109
airindirldata$diecopdnosharedL <- pop*10*wcopdm*(noshared.copdRRL-1)*109
airindirldata$diecopdnosharedH <- pop*10*wcopdm*(noshared.copdRRH-1)*109

##有共享单车
airindirldata$diecopdshared <- pop*10*wcopdm*(shared.copdRR-1)*109
airindirldata$diecopdsharedL <- pop*10*wcopdm*(shared.copdRRL-1)*109
airindirldata$diecopdsharedH <- pop*10*wcopdm*(shared.copdRRH-1)*109


###G.哮喘归因计算####
##无共享单车
airindirldata$dieashnoshared <- pop*10*washm*(noshared.copdRR-1)*109
airindirldata$dieashnosharedL <- pop*10*washm*(noshared.copdRRL-1)*109
airindirldata$dieashnosharedH <- pop*10*washm*(noshared.copdRRH-1)*109

##有共享单车
airindirldata$dieashshared <- pop*10*washm*(shared.copdRR-1)*109
airindirldata$dieashsharedL <- pop*10*washm*(shared.copdRRL-1)*109
airindirldata$dieashsharedH <- pop*10*washm*(shared.copdRRH-1)*109

detach(airindirldata)

####(2)收益死亡数和YLL计算####
###A.各类结局计算####
attach(airindirldata)

#总死亡
airindirldata$beniapindirtot <- airindirldata$dienoshared-airindirldata$dieshared
airindirldata$beniapindirtotyll <- airindirldata$beniapindirtot*totyll/(wtotalm/7*365*100)

#CVD
airindirldata$beniapindircvd <- airindirldata$diecvdnoshared-airindirldata$diecvdshared

airindirldata$beniapindircvdyll <- airindirldata$beniapindircvd*cvdyll/(wcvdm/7*365*100)

#IHD
airindirldata$beniapindirihd <- airindirldata$dieihdnoshared-airindirldata$dieihdshared

airindirldata$beniapindirihdyll <- airindirldata$beniapindirihd*ihdyll/(wihdrm/7*365*100)

#stroke
airindirldata$beniapindirstro <- airindirldata$diestronoshared-airindirldata$diestroshared

airindirldata$beniapindirstroyll <- airindirldata$beniapindirstro*storyll/(wstrorm/7*365*100)

#RESP
airindirldata$beniapindirresp <- airindirldata$dierespnoshared-airindirldata$dierespshared

airindirldata$beniapindirrespyll <- airindirldata$beniapindirresp*respyll/(wrespm/7*365*100)

#COPD
airindirldata$beniapindircopd <- airindirldata$diecopdnoshared-airindirldata$diecopdshared

airindirldata$beniapindircopdyll <- airindirldata$beniapindircopd*copdyll/(wcopdm/7*365*100)

#哮喘
airindirldata$beniapindirash <- airindirldata$dieashnoshared-airindirldata$dieashshared

airindirldata$beniapindirashyll <- airindirldata$beniapindirash*astyll/(washm/7*365*100)

detach(airindirldata)



####5.Calculation of morbidity and YLD benefits####
#建立发病和YLD的数据库
#与死亡使用相同的RR
names(airindirldata)
airindirldis <- airindirldata[,c(1:27)]

###匹配死亡率和YLL数据###
airindirldis <- left_join(airindirldis,dismorrate,by="area") %>%
  left_join(ylddata,by="area")
names(airindirldis)

####(1)归因发病数和YLD计算####
attach(airindirldis)

###A.CVD归因计算####
airindirldis$discvdnoshared <- pop*10*wcvdi*(noshared.stroRR-1)*109
##有共享单车
airindirldis$discvdshared <- pop*10*wcvdi*(shared.stroRR-1)*109

###B.中风归因计算####
##无共享单车
airindirldis$disstronoshared <- pop*10*wstrori*(noshared.stroRR-1)*109

##有共享单车
airindirldis$disstroshared <- pop*10*wstrori*(shared.stroRR-1)*109

###C.IHD归因计算####
##无共享单车
airindirldis$disihdnoshared <- pop*10*wihdri*(noshared.ihdRR-1)*109

##有共享单车
airindirldis$disihdshared <- pop*10*wihdri*(shared.ihdRR-1)*109

###D.RESP归因计算####
##无共享单车
airindirldis$disrespnoshared <- pop*10*wrespi*(noshared.copdRR-1)*109

##有共享单车
airindirldis$disrespshared <- pop*10*wrespi*(shared.copdRR-1)*109

###E.COPD归因计算####
##无共享单车
airindirldis$discopdnoshared <- pop*10*wcopdi*(noshared.copdRR-1)*109

##有共享单车
airindirldis$discopdshared <- pop*10*wcopdi*(shared.copdRR-1)*109


###F.哮喘归因计算####
##无共享单车
airindirldis$disashnoshared <- pop*10*washi*(noshared.copdRR-1)*109

##有共享单车
airindirldis$disashshared <- pop*10*washi*(shared.copdRR-1)*109

detach(airindirldis)


####(2)收益发病数和YLD计算####
###A.各类结局计算####
attach(airindirldis)

#CVD
airindirldis$beniapindircvd <- airindirldis$discvdnoshared-airindirldis$discvdshared

airindirldis$beniapindircvdyld <- airindirldis$beniapindircvd*cvdyld/(wcvdi/7*365*100)

#IHD
airindirldis$beniapindirihd <- airindirldis$disihdnoshared-airindirldis$disihdshared

airindirldis$beniapindirihdyld <- airindirldis$beniapindirihd*ihdyld/(wihdri/7*365*100)

#stroke
airindirldis$beniapindirstro <- airindirldis$disstronoshared-airindirldis$disstroshared

airindirldis$beniapindirstroyld <- airindirldis$beniapindirstro*storyld/(wstrori/7*365*100)

#RESP
airindirldis$beniapindirresp <- airindirldis$disrespnoshared-airindirldis$disrespshared

airindirldis$beniapindirrespyld <- airindirldis$beniapindirresp*respyld/(wrespi/7*365*100)

#COPD
airindirldis$beniapindircopd <- airindirldis$discopdnoshared-airindirldis$discopdshared

airindirldis$beniapindircopdyld <- airindirldis$beniapindircopd*copdyld/(wcopdi/7*365*100)

#哮喘
airindirldis$beniapindirash <- airindirldis$disashnoshared-airindirldis$disashshared

airindirldis$beniapindirashyld <- airindirldis$beniapindirash*astyld/(washi/7*365*100)

detach(airindirldis)

