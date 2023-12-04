#Project BEFD
#Wine forecasting 

####################
#    Plan / Goal   #
####################

#Target : exports_france_value (we want to predict for 1, 2 or 3 years)

# - [check] create some useful plots 
# - take a decision on where we want to focus on
# - do we take 13.2 billions for our target in 2022 from internet ???
# - do an ARIMA on our target can be good
# - do a GBM on our target with 2 rectangular shock (one in 2009 and one in 2020)
# - 
# -

####################
# Import libraries #
####################

library(readxl)
library(DIMORA)

####################
#    Import Data   #
####################

data = read_excel("data.xlsx")

#Create tables from the big table data
consumption_france = data$cons_france_volume
consumption_world = data$cons_world_volume
gdp_france = data$gdp_france_value
production_france = data$prod_france_volume
production_italy = data$prod_italy_volume
production_spain = data$prod_spain_volume
exports_france_value = data$exp_france_value  
exports_france_volume = data$exp_france_volume

selected_years <- data$year[seq(1, length(data$year), by = 5)]

####################
#       Plots      #
####################

#Plots of production, consumption and exports for France 
#=> low peak in 2017 and 2020 for production 
plot(production_france, type= "b",main="France time series", xlab="Year", ylab="Volume (1000hl)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6, ylim=c(0,65000))
lines(consumption_france, type = "l", col = "red")
lines(exports_france_volume, type = "l", col = "blue")
legend("topright", legend = c("Production", "Consumption","Exports"), col = c("black", "red","blue"), lwd = 2, cex = 0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)



#Production comparisons  
#=> It seems that the 3 time series are correlated (specially between France and Italy)
plot(production_france, type= "b",main="Production comparisons", xlab="Year", ylab="Volume (1000hl)", xaxt="n", lty=3, lwd=2, pch=16, cex=0.6, ylim=c(0,65000))
lines(production_italy, type = "l", col = "red")
lines(production_spain, type = "l", col = "blue")
legend("topright", legend = c("France", "Italy","Spain"), col = c("black", "red","blue"), lty = 1, lwd = 2, cex = 0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)



#Plots consumption world vs France (in this plot you need to zoom to see)
#Not a really correlation might be useless to use this for our analysis
par(mfrow = c(2, 1))
plot(consumption_world, type= "b",main="World consumption", xlab="Year", ylab="Volume (1000hl)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)
plot(consumption_france, type= "b",main="France consumption", xlab="Year", ylab="Volume (1000hl)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)
dev.off()



#Financial plots for France
#=>Low peak for 2009 and 2020 due to the 2 CRISIS ! 
par(mfrow = c(2, 1))
plot(gdp_france, type= "b",main="GDP of France", xlab="Year", ylab="Value ($)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)
plot(exports_france_value, type= "b",main="Wine exports of France", xlab="Year", ylab="Value ($)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)
dev.off()


####################
#  Modelling part  #
####################

#it can be interesting to compare correlation between our variables and the target

#errors because we don't have the value of exports in 1995,1996,1997 and 2022
#cor(gdp_france,exports_france_value, method="pearson")
