rm(list = ls())
###################################
##calculate health burden in traffic injury###


library(openxlsx)
library(dplyr)
library(reshape2)

###1.imported data####
#bicycle data
biked <- read.csv("./data/bikedata210415.csv",stringsAsFactors = F)
names(biked)

proname <- unique(biked$area)
proname <- proname[proname!="nationwide"]

#Deaths per million kilometers
bikedeath <- 0.048
cardeath <- 0.0074
walkdeath <- 0.108
busdeath <- 0.0006475

#GBD data
agefile <- read.xlsx("./data/GBD input/Classification file-age.xlsx")
GBD <- read.csv("./data/GBD input/IHME-GBD_2019_DATA_all.csv") %>%
  subset(location_name=="China") %>%
  subset(select=c("measure_name","location_name","sex_id","sex_name",
                  "age_id","age_name","cause_id","cause_name",
                  "val","upper","lower")) %>%
  left_join(agefile,by="age_name")
names(GBD)
GBD1 <- aggregate(GBD["val"],
                  by=list(measure_name=GBD$measure_name,
                          sex=GBD$sex_name,
                          age=GBD$age,
                          cause_id=GBD$cause_id,
                          cause_name=GBD$cause_name),
                  sum)
#population data
pop <- read.xlsx("./data/GBD input/population/Population_National and Provinces V4_Hong all age.xlsx") %>%
  subset(area != "National wide")

###2.loop for each province####
totalre <- data.frame()
for (i in 1:length(proname)) {
  print(proname[i])
  bikepo <- subset(biked,area==proname[i])
  
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
  # mean(bikepo$mil25mr+bikepo$mil2530mr+bikepo$mil3135mr+bikepo$mil3640mr+bikepo$mil40mr+bikepo$mil25fr+bikepo$mil2530fr+bikepo$mil3135fr+bikepo$mil3640fr+bikepo$mil40fr,na.rm = T)
  
  ##GBD
  #death
  injurydeath <- subset(GBD1,cause_name=="Road injuries"&
                        measure_name=="Deaths") %>%
    subset(select=c("sex","age","val"))
  names(injurydeath)  <- c("sex","age","death")
  
  #YLL
  injuryyll <- subset(GBD1,cause_name=="Road injuries"&
                        measure_name=="YLLs (Years of Life Lost)") %>%
    left_join(injurydeath,by=c("sex","age"))
  
  #YLL per death
  injuryyll$yllpd <- injuryyll$val/injuryyll$death
  
  ###(3)injury death before mode shift by age and gender####
  usertype <- data.frame(bt=c("25m","2530m","3135m","3640m","40m",
                              "25f","2530f","3135f","3640f","40f"),
                         sex=c("Male","Male","Male","Male","Male",
                               "Female","Female","Female","Female","Female"),
                         age=c("15-25","25-30","31-35","36-40","40-60",
                               "15-25","25-30","31-35","36-40","40-60"))
  prore <- data.frame()
  for (j in 1:nrow(usertype)) {
    print(paste0(proname[i],"-",usertype$sex[j],":",usertype$age[j]))
    
    #yll
    yllsub <- injuryyll$yllpd[injuryyll$age==usertype$age[j]&
                                injuryyll$sex==usertype$sex[j]]
    
    #ratio
    typer <- bikepo[paste0("mil",usertype$bt[j],"r")]
    
    #walk
    walkdie <- (bikepo$milew*typer/100)*walkdeath;names(walkdie) <- "walkdie"
    walkyll <- walkdie*yllsub;names(walkyll) <- "walkyll"
    
    #bike
    bikedie <- (bikepo$mileb*typer/100)*bikedeath;names(bikedie) <- "bikedie"
    bikeyll <- bikedie*yllsub;names(bikeyll) <- "bikeyll"
    
    #car
    cardie <- (bikepo$milec*typer/100)*cardeath;names(cardie) <- "cardie"
    caryll <- cardie*yllsub;names(caryll) <- "caryll"
    
    #bus
    busdie <- (bikepo$milep*typer/100)*busdeath;names(busdie) <- "busdie"
    busyll <- busdie*yllsub;names(busyll) <- "busyll"
    
    ##all death before mode shift by age and gender
    beforedie <- walkdie+bikedie+cardie+busdie;names(beforedie) <- "beforedie"
    beforeyll <- walkyll+bikeyll+caryll+busyll;names(beforeyll) <- "beforeyll"
      
    ###(4)injury death after mode shift by age and gender####
    afterdie <- bikepo$mil*typer/100*bikedeath;names(afterdie) <- "afterdie"
    afteryll <- afterdie*yllsub;names(afteryll) <- "afteryll"
    
    #one type result
    type1 <- data.frame(area=proname[i],
                       sex=usertype$sex[j],
                       age=usertype$age[j],
                       walkdie=sum(walkdie,na.rm=T),
                       walkyll=sum(walkyll,na.rm=T),
                       bikedie=sum(bikedie,na.rm=T),
                       bikeyll=sum(bikeyll,na.rm=T),
                       cardie=sum(cardie,na.rm=T),
                       caryll=sum(caryll,na.rm=T),
                       busdie=sum(busdie,na.rm=T),
                       busyll=sum(busyll,na.rm=T),
                       beforedie=sum(beforedie,na.rm=T),
                       beforeyll=sum(beforeyll,na.rm=T),
                       afterdie=sum(afterdie,na.rm=T),
                       afteryll=sum(afteryll,na.rm=T))
    prore <- rbind(prore,type1)
  }
  totalre <- rbind(totalre,prore)
}
totalre$benedeath <- totalre$afterdie - totalre$beforedie
totalre$beneyll <- totalre$afteryll - totalre$beforeyll

write.csv(totalre,"./output/injury/death&yllbeneifit-alltype.csv",row.names = F)


##3.aggregate calculate####
names(totalre)
allre <- aggregate(totalre[c("beforedie","beforeyll","afterdie","afteryll","benedeath","beneyll")],
                   by=list(area=totalre$area),
                   sum)
allre[30,] <- c("total",apply(allre[2:7], 2, sum))
write.csv(allre,"./output/injury/death&yllbeneifit-bypro.csv",row.names = F)



