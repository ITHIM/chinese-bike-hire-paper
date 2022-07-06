
rm(list = ls())
###################################
##Calculate the indirect impact of air pollution on health burden###

library(openxlsx)
library(dplyr)
library(reshape2)
library(tidyr)

###1.imported and prepare data####
##Ratio of car sources of PM2.5
pm2.5r <- 0.2409

##Gas consumption (10,000 liters)
oildata <- read.csv("../data/oildata.csv",stringsAsFactors = F)
oildata$woil <- oildata$oil/365*7  #weekly

##Fuel consumption per 100 km
oilc <- 6.9

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
popother <- subset(pop,area %in% c("QH","NX","XZ"))
popother1 <- aggregate(popother["population"],
                       by=list(age=popother$age,
                               sex=popother$sex),
                       sum)
popother1$area <- "other"
pop <- rbind(pop,popother1)

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
  bikepo <- subset(biked1,area==proname[i]) %>%
    left_join(oildata,by="area")
  
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
  
  ###(2)Cycling reduces gasoline consumption and changes in air concentration####
  bikepo$oilsave <- bikepo$milec*100*oilc
  
  #PM2.5 from motor vehicles with sharedbike
  bikepo$pm2.5vshared <- bikepo$pm2.5*pm2.5r
  
  #PM2.5 from motor vehicles without sharedbike
  bikepo$pm2.5vnoshared <- bikepo$pm2.5vshared*(bikepo$woil*10000/(bikepo$woil*10000-bikepo$oilsave))
  
  #PM2.5 concentration without bike sharing
  bikepo$pm2.5noshared <- bikepo$pm2.5-bikepo$pm2.5vshared+bikepo$pm2.5vnoshared
  
  #Long-term per capita exposure concentration
  lcPM2.5shared = mean(bikepo$pm2.5,na.rm = T)
  lcPM2.5noshared = mean(bikepo$pm2.5noshared,na.rm = T)
  
  longex <- data.frame(con=c(lcPM2.5shared,lcPM2.5noshared),
                       change=c("noshared","shared"))
  ###(3)Generate the RR value####
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
      
      RRd <- data.frame(area=proname[i],
                        cause_name=subpar$cause_name[1],
                        change=longex$change[q],
                        RR=RR,RRL=RRL,RRH=RRH)
      RRdata <- rbind(RRdata,RRd)
    }
  }
  
  poptype <- data.frame(area=rep(proname[i],14),
                        age=c("0-15","15-25","25-30","31-35","36-40","40-60",">60",
                              "0-15","15-25","25-30","31-35","36-40","40-60",">60"),
                        sex=c("Male","Male","Male","Male","Male","Male","Male",
                              "Female","Female","Female","Female","Female","Female","Female"))
  poptype1 <- left_join(poptype,pop,by=c("area","sex","age"))
  for (j in 1:nrow(poptype1)) {
    print(paste0(proname[i],"-",poptype1$sex[j],":",poptype1$age[j]))
    GBDpro <- subset(GBD2,sex==poptype1$sex[j]&age==poptype1$age[j])
    GBDpro1 <- left_join(RRdata,GBDpro,by=c("cause_name")) %>%
      left_join(pop,by=c("area","sex","age"))
    
    ###death rate and YLL per death
    GBDpro1$deathr <- GBDpro1$Deaths/GBDpro1$totpop/365*7
    GBDpro1$YLL1death <- GBDpro1$YLL/GBDpro1$Deaths
    GBDpro1$YLL1death[is.na(GBDpro1$YLL1death)] <- 0
    
    prodata <- rbind(prodata,GBDpro1)
  }
}


###3.calculation and aggregation####
##calculation
#attributable death
prodata$ad <- prodata$population*prodata$deathr*(prodata$RR-1)*109
prodata$adse <- ((prodata$population*prodata$deathr*(prodata$RRH-1)*109)-
                   (prodata$population*prodata$deathr*(prodata$RRL-1)*109))/(1.96*2)

#attributable YLL
prodata$aY <- prodata$ad*prodata$YLL1death
prodata$aYse <- ((prodata$population*prodata$deathr*(prodata$RRH-1)*109)*prodata$YLL1death-
                   (prodata$population*prodata$deathr*(prodata$RRL-1)*109)*prodata$YLL1death)/(1.96*2)

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

write.csv(nationresult,"./output/indirectAP/nationalben-indirect.csv",row.names = F)


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

write.csv(proresult,"./output/indirectAP/provincialben-indirect.csv",row.names = F)
