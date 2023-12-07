#Project BEFD
#Wine forecasting 

####################
#    Plan / Goal   #
####################

#Target : exports_france_value (we want to predict for 1, 2 or 3 years)


# TO DO LIST

# - [check] create some useful plots 
# - take a decision on where we want to focus on
# - [check] do an ARIMA on our target can be good
# - do a GBM on our target with 2 rectangular shocks (one in 2009 and one in 2020)
# - use some splines models to analyze our data (be careful we use it for modelling)
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

years = data$year
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



#Plots for export from France in value vs. volume
exports_france_value <- as.numeric(exports_france_value)
exports_france_value_index = (exports_france_value/exports_france_value[1])*100
exports_france_volume_index = (exports_france_volume/exports_france_volume[1])*100


plot(exports_france_value_index, type= "b",main="Exported value vs volume", xlab="Year", ylab="Index", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6, ylim=c(70,300))
lines(exports_france_volume_index, type = "l", col = "red")
legend("topright", legend = c("Export value", "Export volume"), col = c("black", "red"), lwd = 2, cex = 0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)


#it can be interesting to compare correlation between our variables and the target
cor(gdp_france,exports_france_value, method="pearson") #=> 61%

library(corrplot)
corrplot(cor(data), method = "number", tl.col = "black")
#we can say something about the correlation, we can see together ! 





####################
#  Modelling part  #
####################

### ARIMA 

#there is a trend so the time series is not stationary 
plot(exports_france_value, type= "b",main="Wine exports of France", xlab="Year", ylab="Value ($)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)

#acf peaks decrease slowly, confirming the presence of a linear trend
tsdisplay(exports_france_value) 

#first difference 
exports_france_value.diff <- diff(exports_france_value,differences=1)

#it's better and we can see that there is no seasonality
tsdisplay(exports_france_value.diff) 

# very difficult to find p and q (think it's better to them equal to 0)


############
#first model  -> AIC = 52.44 
############
auto.a<- auto.arima(exports_france_value)
auto.a 
#ARIMA(p,d,q) here : ARIMA(0,1,0), so the auto ARIMA do just the fist difference and then AR(0) / MA(0)
#AIC = 52.44 

#check residuals (=> white noise) 
#why ? -> graph of residuals / ACF : peaks in the bands / mean = 0 / Ljung-Box test : Q* = 4.224 
checkresiduals(auto.a)

#plot target and the fitted model
plot(exports_france_value, type= "b",main="Wine exports of France fitted with ARIMA(0,1,0)", xlab="Year", ylab="Value ($)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
fit1<- fitted(auto.a)
lines(fit1, col=2)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)

##shift of one year ?????????

#plot the forecast
autoplot(forecast(auto.a))


#############
#second model   -> AIC = 59.65
#############

a2<- Arima(exports_france_value, order=c(3,1,0))
#ARIMA(3,1,0) so AR(3) / MA(0)
a2 #AIC = 59.65

#check residuals (=> white noise)
checkresiduals(a2)

plot(exports_france_value, type= "b",main="Wine exports of France fitted with ARIMA(3,1,0)", xlab="Year", ylab="Value ($)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
fit2<- fitted(a2)
lines(fit2, col=2)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)

#plot the forecast
autoplot(forecast(a2))



# 
# We can conclude for the ARIMA that ARIMA(0,1,0) is the best with an AIC = 52.44 
# 

#Forecast 

#we just have to change year_forecast
year_forecast = 5
forecast.ARIMA = forecast(auto.a)

plot(exports_france_value,xlim=c(0,23+year_forecast),ylim=c(5,20),type= "b",main="France Wine exports forecast", xlab="Year", ylab="Value ($)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)

lines(forecast.ARIMA$upper[,2],col="blue",lty=2,type= "b",pch=16, lwd=2, cex=0.6)
lines(forecast.ARIMA$mean,col="red",lty=2,type= "b",pch=16, lwd=2, cex=0.6)
lines(forecast.ARIMA$lower[,2],col="blue",lty=2,type= "b",pch=16, lwd=2, cex=0.6)
legend("topright",legend=c("Upper and lower bands", "Mean forecasting"),col=c("blue","red"),pch=3,lty = 1, lwd = 2, cex = 0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)




### GBM with 2 rectangular shocks (one in 2009 and one in 2020) 









