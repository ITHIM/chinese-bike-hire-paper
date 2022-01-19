
###################################
#####<2>Traffic injury##########
library(openxlsx)
library(dplyr)

####1.obtain the distance mortality for walking, biking and driving#####
kmdeath <- read.xlsx("./data/Deaths per million kilometers-region.xlsx")
proregion <- read.xlsx("./data/region-aera.xlsx")


####2.Extract the database ####
names(bikepo)
injurydata <- bikepo[c("area","milew","milec","mileb","mil")]

injurydata <- left_join(injurydata,proregion,by="area") %>%
  left_join(kmdeath,"region")


####3.Traffic deaths before travel mode shift####
#(1)walk
injurydata$diewalk <- injurydata$milew/100*injurydata$walkdeath
injurydata$diewalkL <- injurydata$milew/100*injurydata$walkdeathL
injurydata$diewalkH <- injurydata$milew/100*injurydata$walkdeathH

#(2)ride
injurydata$diebike <- injurydata$milew/100*injurydata$bikedeath
injurydata$diebikeL <- injurydata$milew/100*injurydata$bikedeathL
injurydata$diebikeH <- injurydata$milew/100*injurydata$bikedeathH


#(3)driving
injurydata$diecar <- injurydata$milew/100*injurydata$cardeath
injurydata$diecarL <- injurydata$milew/100*injurydata$cardeathL
injurydata$diecarH <- injurydata$milew/100*injurydata$cardeathH


#(4)sum
injurydata$dienoshared <- injurydata$diewalk+injurydata$diebike+injurydata$diecar


####4.Traffic deaths after travel mode shift####
injurydata$dieshared <- injurydata$mil/100*injurydata$bikedeath
injurydata$diesharedL <- injurydata$mil/100*injurydata$bikedeathL
injurydata$diesharedH <- injurydata$mil/100*injurydata$bikedeathH


####5.Calculate the change in traffic deaths after a change in travel mode####
injurydata$beniInj <- injurydata$dienoshared-injurydata$dieshared



####6.YLL DATA####
transinjury <- read.xlsx("./data/transport mortality yll.xlsx",sheet = 1)
benIj <- left_join(injurydata,transinjury,by="area")

benIj$beniInjyll <- benIj$beniInj*(benIj$transyll/benIj$transm)

