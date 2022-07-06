rm(list = ls())
getwd()
###################################
##Calculate the direct impact of air pollution on health burden###

library(openxlsx)
library(dplyr)
library(reshape2)
library(tidyr)

###1.imported data####
#bicycle data
biked <- read.csv("./data/bikedata210415.csv",stringsAsFactors = F)
biked <- subset(biked,area != "nationwide")

proname <- unique(biked$area)


#air pollution data#
podata <- read.csv("./data/pollutiondata/poldata.csv",stringsAsFactors = F)
biked1 <- left_join(biked,podata,by=c("area","week"))

#population data
pop <- read.xlsx("./data/GBD input/population/Population_National and Provinces V4_Hong all age.xlsx") %>%
  subset(area != "National wide")
nationpop <- read.xlsx("./data/GBD input/population/Population_National and Provinces V4_Hong all age.xlsx") %>%
  subset(area == "National wide") %>%
  subset(select=c("age","sex","population"))
names(nationpop)[3] <- "totpop"

#GBD data
agefile <- read.xlsx("./data/GBD input/Classification file-age.xlsx")
GBD <- read.csv("./data/GBD input/IHME-GBD_2019_DATA_all.csv") %>%
  subset(location_name=="China") %>%
  subset(select=c("measure_name","location_name","sex_id","sex_name",
                  "age_id","age_name","cause_id","cause_name",
                  "val","upper","lower")) %>%
  left_join(agefile,by="age_name") %>%
  subset(cause_name %in% c("All causes","Cardiovascular diseases","Ischemic heart disease","Stroke",
                           "Chronic respiratory diseases","Chronic obstructive pulmonary disease",
                           "Asthma","Lower respiratory infections")) %>%
  subset(measure_name %in% c("Deaths","YLLs (Years of Life Lost)"))
GBD$measure_name <- ifelse(GBD$measure_name=="Deaths","Deaths","YLL")

GBD1 <- aggregate(GBD["val"],
                  by=list(measure_name=GBD$measure_name,
                          sex=GBD$sex_name,
                          age=GBD$age,
                          cause_id=GBD$cause_id,
                          cause_name=GBD$cause_name),
                  sum)
names(GBD1)
GBD2 <- spread(data = GBD1,
               key = "measure_name",
               value = "val") %>%
  left_join(nationpop,by=c("age","sex"))



##function of Global Exposure Mortality Model (GEMM)
#con,theta,alpha,miu,pai
RRcal <- function(con,theta,alpha,miu,pai){
  rr <- exp((log(1+((con-2.4)/alpha)))/(1+exp((miu-(con-2.4))/pai))*theta)
  return(rr)
}

###2.loop for each province####
prodata <- data.frame()
for (i in 1:length(proname)) {

  print(proname[i])
  bikepo <- subset(biked1,area==proname[i])
  
  ###(1)Basic assumptions####
  bikepo <- mutate(bikepo,
                   milbelow1=wts*onemilr0.5*0.25+wts*onemilr1*0.75,
                   milover3=wts*onemilr5.*6+wts*onemilr5*4.75+wts*onemilr4.5*4.25+wts*onemilr4*3.75+wts*onemilr3.5*3.25,
                   milbetween13=mil-milbelow1-milover3,
                   
                   milew=milbelow1*0.9094,
                   mileb=milbelow1*0.0906+milbetween13*0.3137,
                   milec=                 milbetween13*0.2483+milover3*0.3619,
                   milep=                 milbetween13*0.4380+milover3*0.6381
  )
  
  ##(2)gender-age distribution(10 type)####
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
  
  #active users of each modes are ontained based on the online survey
  bikepo$acw <- 0.632*bikepo$wau*10000  #users walking before 
  bikepo$acb <- 0.111*bikepo$wau*10000 #users biking before
  bikepo$acc <- 0.093*bikepo$wau*10000  #users driving before
  bikepo$acp <- 0.164*bikepo$wau*10000  #users taking public transport before
  
  usertype <- data.frame(bt=c("25m","2530m","3135m","3640m","40m",
                              "25f","2530f","3135f","3640f","40f"),
                         sex=c("Male","Male","Male","Male","Male",
                               "Female","Female","Female","Female","Female"),
                         age=c("15-25","25-30","31-35","36-40","40-60",
                               "15-25","25-30","31-35","36-40","40-60"),
                         value1=c(63,63,48,48,48,62,62,34,34,34),#Parameters for calculating basal metabolic rate
                         BW=c(66.2,66.2,66.2,66.2,66.2,
                              57.3,57.3,57.3,57.3,57.3),
                         value2=c(2896,2896,3653,3653,3653,
                                  2036,2036,3538,3538,3538))
  
  ##(3)loop for each type of age and sex####
  typedata <- data.frame()
  for (j in 1:nrow(usertype)) {
  
    print(paste0(proname[i],"-",usertype$sex[j],":",usertype$age[j]))
    
    #basal metabolic rate
    BMR <- usertype$value1[j]*usertype$BW[j]+usertype$value2[j]
    
    #per capita respiratory volume
    #IR=(BMR*E*VQ*N)/1440
    IRw <- (BMR*0.05*27*1.5)/1440  #walking
    IRb <- (BMR*0.05*27*4)/1440  #cycling
    IRc <- (BMR*0.05*27*1.2)/1440  #driving and bus
    IRrest <- (BMR*0.05*27*1.0)/1440  #rest or sleep
    IRsit <- (BMR*0.05*27*1.2)/1440 #sitting or working
    
    #ratio
    typer <- bikepo[paste0("mil",usertype$bt[j],"r")]
    names(typer) <- "pratio"
    typer1 <- typer$pratio
    
    #usage time
    walkt <- bikepo$milew*typer1*10000/(5/60) 
    biket <- bikepo$mileb*typer1*10000/(bikepo$mil/bikepo$wtt)
    cart <- bikepo$milec*typer1*10000/(30/60)
    bust <- bikepo$milep*typer1*10000/(30/60)
    
    ####A. Inhalation of pollutants (ug/24h) and Concentration(ug/m3)####
    EPM2.5w <- bikepo$pm2.5*1.6*((IRw*walkt/7)/1000)+
      (bikepo$acw*typer1*8*60*IRrest)/1000*bikepo$pm2.5+
      ((bikepo$acw*typer1*16*60-walkt)*IRsit)/1000*bikepo$pm2.5
    cPM2.5nosharedw <- EPM2.5w/(bikepo$acw*typer1)/24
    EPM2.5wb <- bikepo$pm2.5*2.0*((IRb*((bikepo$milew*typer1*10000)/(bikepo$mil/bikepo$wtt))/7)/1000)+
      (bikepo$acw*typer1*8*60*IRrest)/1000*bikepo$pm2.5+
      (bikepo$acw*typer1*16*60-(bikepo$milew*10000/(bikepo$mil/bikepo$wtt)))*IRsit*bikepo$pm2.5/1000
    cPM2.5sharedw <- EPM2.5wb/(bikepo$acw*typer1)/24
    
    EPM2.5c <- bikepo$pm2.5*2.5*0.58*((IRc*cart/7)/1000)+
      (bikepo$acc*typer1*8*60*IRrest)/1000*bikepo$pm2.5+
      ((bikepo$acc*typer1*16*60-cart)*IRsit)/1000*bikepo$pm2.5
    cPM2.5nosharedc <- EPM2.5c/(bikepo$acc*typer1)/24
    EPM2.5cb <- bikepo$pm2.5*2.0*((IRb*(bikepo$milec*typer1*10000/(bikepo$mil/bikepo$wtt))/7)/1000)+
      (bikepo$acc*typer1*8*60*IRrest)/1000*bikepo$pm2.5+
      (bikepo$acc*typer1*16*60-(bikepo$milec*typer1*10000/(bikepo$mil/bikepo$wtt)))*IRsit*bikepo$pm2.5/1000
    cPM2.5sharedc <- EPM2.5cb/(bikepo$acc*typer1)/24
    
    EPM2.5p <- bikepo$pm2.5*2.5*0.58*((IRc*bust/7)/1000)+
      (bikepo$acp*typer1*8*60*IRrest)/1000*bikepo$pm2.5+
      ((bikepo$acp*typer1*16*60-cart)*IRsit)/1000*bikepo$pm2.5
    cPM2.5nosharedp <- EPM2.5p/(bikepo$acp*typer1)/24
    EPM2.5pb <- bikepo$pm2.5*2.0*((IRb*(bikepo$milep*typer1*10000/(bikepo$mil/bikepo$wtt))/7)/1000)+
      (bikepo$acp*typer1*8*60*IRrest)/1000*bikepo$pm2.5+
      (bikepo$acp*typer1*16*60-(bikepo$milep*typer1*10000/(bikepo$mil/bikepo$wtt)))*IRsit*bikepo$pm2.5/1000
    cPM2.5sharedp <- EPM2.5pb/(bikepo$acp*typer1)/24
    
    
    ####B.Changes in long-term exposure concentrations####
    lcPM2.5nosharedw = mean(cPM2.5nosharedw,na.rm = T)
    lcPM2.5sharedw = mean(cPM2.5sharedw,na.rm = T)
    lcPM2.5nosharedc = mean(cPM2.5nosharedc,na.rm = T)
    lcPM2.5sharedc = mean(cPM2.5sharedc,na.rm = T)
    lcPM2.5nosharedp = mean(cPM2.5nosharedp,na.rm = T)
    lcPM2.5sharedp = mean(cPM2.5sharedp,na.rm = T)
    
    longex <- data.frame(mode=c("lcnosharedw","lcsharedw","lcnosharedc","lcsharedc",
                                "lcnosharedp","lcsharedp"),
                         con=c(lcPM2.5nosharedw,lcPM2.5sharedw,
                               lcPM2.5nosharedc,lcPM2.5sharedc,
                               lcPM2.5nosharedp,lcPM2.5sharedp),
                         commute=c("walking","walking","driving","driving",
                                   "bus","bus"),
                         change=c("noshared","shared",
                                  "noshared","shared",
                                  "noshared","shared"),
                         meanuser=c(mean(bikepo$acw*typer1,na.rm = T)/10000,
                                    mean(bikepo$acw*typer1,na.rm = T)/10000,
                                    mean(bikepo$acc*typer1,na.rm = T)/10000,
                                    mean(bikepo$acc*typer1,na.rm = T)/10000,
                                    mean(bikepo$acp*typer1,na.rm = T)/10000,
                                    mean(bikepo$acp*typer1,na.rm = T)/10000))
    
    ####c.Generate the RR value####
    #Calculated parameters
    calpar <- data.frame(cause_name=c("All causes","Cardiovascular diseases","Ischemic heart disease",
                                      "Stroke","Lower respiratory infections",
                                      "Chronic respiratory diseases","Asthma",
                                      "Chronic obstructive pulmonary disease"),
                         theta1=c(0.143,0.2720,0.2969,0.2720,0.2510,0.2510,0.2510,0.2510),
                         theta1se=c(0.01807,0.07697,0.01787,0.07697,0.06762,0.06762,0.06762,0.06762),
                         alpha1=c(1.6,6.2,1.9,6.2,6.5,6.5,6.5,6.5),
                         miu1=c(15.5,16.7,12,16.7,2.5,2.5,2.5,2.5),
                         pai1=c(36.8,23.7,40.2,23.7,32,32,32,32))
    
    ###cross calpar and lcPM2.5nosharedw variable###
    RRdata <- data.frame()
    for (p in 1:nrow(calpar)) {
      subpar <- calpar[p,]
      for (q in 1:nrow(longex)) {
        RR <- ifelse(longex$con[q]< 2.4,1,
                     RRcal(con=longex$con[q],
                           theta = subpar$theta1[1],alpha = subpar$alpha1[1],
                           miu = subpar$miu1[1],pai = subpar$pai1[1]))
        RRL <- ifelse(longex$con[q]< 2.4,1,
                      RRcal(con=longex$con[q],
                            theta = subpar$theta1[1]-1.96*subpar$theta1se[1],alpha = subpar$alpha1[1],
                            miu = subpar$miu1[1],pai = subpar$pai1[1]))
        
        RRH <- ifelse(longex$con[q]< 2.4,1,
                      RRcal(con=longex$con[q],
                            theta = subpar$theta1[1]+1.96*subpar$theta1se[1],alpha = subpar$alpha1[1],
                            miu = subpar$miu1[1],pai = subpar$pai1[1]))
        
        RRd <- data.frame(cause_name=subpar$cause_name[1],
                          mode=longex$mode[q],
                          commute=longex$commute[q],
                          change=longex$change[q],
                          RR=RR,RRL=RRL,RRH=RRH,
                          meanuser=longex$meanuser[q])
        RRdata <- rbind(RRdata,RRd)
      }
    }
    
    
    ###death rate and YLL per death
    GBDtype <- subset(GBD2,age==usertype$age[j]&sex==usertype$sex[j])
    GBDtype$deathr <- GBDtype$Deaths/GBDtype$totpop/365*7 #per week
    GBDtype$YLL1death <- GBDtype$YLL/GBDtype$Deaths
    diedata <- left_join(GBDtype,RRdata,by="cause_name")
    
    typedata <- rbind(typedata,diedata)
  }
  subpro <- data.frame(area=proname[i],
                       typedata)
  prodata <- rbind(prodata,subpro)
}

###3.calculation and aggregation####
##calculation
#attributable death
prodata$ad <- prodata$meanuser*prodata$deathr*(prodata$RR-1)*109
prodata$adse <- ((prodata$meanuser*prodata$deathr*(prodata$RRH-1)*109)-
  (prodata$meanuser*prodata$deathr*(prodata$RRL-1)*109))/(1.96*2)

#attributable YLL
prodata$aY <- prodata$ad*prodata$YLL1death
prodata$aYse <- ((prodata$meanuser*prodata$deathr*(prodata$RRH-1)*109)*prodata$YLL1death-
                   (prodata$meanuser*prodata$deathr*(prodata$RRL-1)*109)*prodata$YLL1death)/(1.96*2)




##aggregate to mode change
#Standard Deviation Calculation Function
calse <- function(x){
  se <- (sum(x^2))^0.5
  return(se)
}


prodata1 <- aggregate(prodata[c("ad","aY")],
                      by=list(cause_id=prodata$cause_id,
                              cause_name=prodata$cause_name,
                              change=prodata$change,
                              area=prodata$area,
                              sex=prodata$sex,
                              age=prodata$age),
                      sum) %>%
  left_join(aggregate(prodata[c("adse","aYse")],
                      by=list(cause_id=prodata$cause_id,
                              cause_name=prodata$cause_name,
                              change=prodata$change,
                              area=prodata$area,
                              sex=prodata$sex,
                              age=prodata$age),
                      calse),
            by=c("cause_id","cause_name","change",
                 "area","sex","age"))
names(prodata1)
prodata2 <- gather(prodata1,
                   key = "index",
                   value = "num",
                   ad:aYse) %>%
  spread(key = "change",
         value = 'num')



##aggregate to region and subgroup
names(prodata2)
prodata2.1 <- subset(prodata2,index %in% c("ad","aY"))
prodata2.2 <- subset(prodata2,index %in% c("adse","aYse"))
prodata2.2$index <- ifelse(prodata2.2$index=="adse","ad","aY")
names(prodata2.2)[7:8] <- c("noshared.se","shared.se")

####(1)national####
nationresult1.1 <- aggregate(prodata2.1[c("noshared","shared")],
                             by=list(cause_id=prodata2.1$cause_id,
                                     cause_name=prodata2.1$cause_name,
                                     sex=prodata2.1$sex,
                                     age=prodata2.1$age,
                                     index=prodata2.1$index),
                             sum) %>%
  left_join(aggregate(prodata2.2[c("noshared.se","shared.se")],
                      by=list(cause_id=prodata2.2$cause_id,
                              cause_name=prodata2.2$cause_name,
                              sex=prodata2.2$sex,
                              age=prodata2.2$age,
                              index=prodata2.2$index),
                      calse),
            by=c("cause_id","cause_name","sex","age","index"))

nationresult1.1$nosharedL <- nationresult1.1$noshared-1.96*nationresult1.1$noshared.se
nationresult1.1$nosharedH <- nationresult1.1$noshared+1.96*nationresult1.1$noshared.se

nationresult1.1$sharedL <- nationresult1.1$shared-1.96*nationresult1.1$shared.se
nationresult1.1$sharedH <- nationresult1.1$shared+1.96*nationresult1.1$shared.se

nationresult1.1$ben <- nationresult1.1$shared-nationresult1.1$noshared
nationresult1.1$benL <- nationresult1.1$sharedL-nationresult1.1$nosharedL
nationresult1.1$benH <- nationresult1.1$sharedH-nationresult1.1$nosharedH



nationresult1.2 <- aggregate(prodata2.1[c("noshared","shared")],
                             by=list(cause_id=prodata2.1$cause_id,
                                     cause_name=prodata2.1$cause_name,
                                     index=prodata2.1$index),
                             sum) %>%
  left_join(aggregate(prodata2.2[c("noshared.se","shared.se")],
                      by=list(cause_id=prodata2.2$cause_id,
                              cause_name=prodata2.2$cause_name,
                              index=prodata2.2$index),
                      calse),
            by=c("cause_id","cause_name","index"))

nationresult1.2$age <- "all"
nationresult1.2$sex <- "all"

nationresult1.2$nosharedL <- nationresult1.2$noshared-1.96*nationresult1.2$noshared.se
nationresult1.2$nosharedH <- nationresult1.2$noshared+1.96*nationresult1.2$noshared.se

nationresult1.2$sharedL <- nationresult1.2$shared-1.96*nationresult1.2$shared.se
nationresult1.2$sharedH <- nationresult1.2$shared+1.96*nationresult1.2$shared.se

nationresult1.2$ben <- nationresult1.2$shared-nationresult1.2$noshared
nationresult1.2$benL <- nationresult1.2$sharedL-nationresult1.2$nosharedL
nationresult1.2$benH <- nationresult1.2$sharedH-nationresult1.2$nosharedH


#rbind
nationresult <- rbind(subset(nationresult1.1,select = c("cause_id","cause_name","sex","age","index",
                                                        "ben","benL","benH")),
                      subset(nationresult1.2,select = c("cause_id","cause_name","sex","age","index",
                                                        "ben","benL","benH")))
nationresult$ben <- round(nationresult$ben,2)
nationresult$benL <- round(nationresult$benL,2)
nationresult$benH <- round(nationresult$benH,2)

write.csv(nationresult,"./output/AP new/nationalben-direct new.csv",row.names = F)


####(2)provincial####
proresult <- aggregate(prodata2.1[c("noshared","shared")],
                       by=list(cause_id=prodata2.1$cause_id,
                               cause_name=prodata2.1$cause_name,
                               area=prodata2.1$area,
                               index=prodata2.1$index),
                       sum) %>%
  left_join(aggregate(prodata2.2[c("noshared.se","shared.se")],
                      by=list(cause_id=prodata2.2$cause_id,
                              cause_name=prodata2.2$cause_name,
                              area=prodata2.1$area,
                              index=prodata2.2$index),
                      calse),
            by=c("cause_id","cause_name","area","index"))

proresult$nosharedL <- proresult$noshared-1.96*proresult$noshared.se
proresult$nosharedH <- proresult$noshared+1.96*proresult$noshared.se

proresult$sharedL <- proresult$shared-1.96*proresult$shared.se
proresult$sharedH <- proresult$shared+1.96*proresult$shared.se

proresult$ben <- proresult$shared-proresult$noshared
proresult$benL <- proresult$sharedL-proresult$nosharedL
proresult$benH <- proresult$sharedH-proresult$nosharedH

proresult <- rbind(subset(proresult,select = c("cause_id","cause_name","index",
                                                "ben","benL","benH")))
proresult$ben <- round(proresult$ben,2)
proresult$benL <- round(proresult$benL,2)
proresult$benH <- round(proresult$benH,2)

write.csv(proresult,"./output/AP new/provincialben-direct.csv",row.names = F)


