

#################################################
##############<6>CO2 co-benifit##################

#IPCC reported that every 1L of gasoline burned will release 2.26KG CO2

bikepo$co2E <- bikepo$oilsave * 2.26
bikepo$co2EL <- bikepo$oilsave * 2.20
bikepo$co2EH <- bikepo$oilsave * 2.37

#Convert units from kilograms to tons
bikepo$co2Et <- bikepo$co2E/1000  
bikepo$co2EtL <- bikepo$co2EL/1000  
bikepo$co2EtH <- bikepo$co2EH/1000 

