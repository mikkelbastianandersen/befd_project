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
library(dplyr)
library(corrplot)
library(lmtest) 
library(forecast)
library(fpp2)

####################
#    Import Data   #
####################

data = read_excel("befd_project/data.xlsx")
data$exp_france_value = as.numeric(data$exp_france_value)
data = data[6:28,]
## create new columns


#Create tables from the big table data
consumption_france = data$cons_france_volume
consumption_world = data$cons_world_volume
gdp_france = data$gdp_france_value_dollars
production_france = data$prod_france_volume
production_italy = data$prod_italy_volume
production_spain = data$prod_spain_volume
exports_france_value = data$exp_france_value  
exports_france_volume = data$exp_france_volume
real_gdp_france = data$real_gdp_france_value_euros
years = data$year
total_exp_france = data$total_exp_france_dollars
population_france = data$population_france
unemployment_france = data$unemployment_france


#scaling
gdp_france_billions =gdp_france/1000000000
total_exp_france_millions = total_exp_france/1000000
population_france_millions = population_france/1000000
exports_france_value_billions = as.numeric(exports_france_value)/1000000000



### from 2000 to 2022
selected_years <- data$year[seq(1, length(data$year), by = 5)]




####################
#       Plots      #
####################


### GDP of France
plot(gdp_france_billions, type= "b",main="GDP of France", xlab="Year", ylab="Value (in billions of $)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)



### Total export of goods from France
plot(total_exp_france_millions, type= "b",main="Total exports of France", xlab="Year", ylab="Value (in millions of $)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)


### Population of France
plot(population_france_millions, type= "b",main="Population of France", xlab="Year", ylab="Value (in millions)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)


### Unemployment percentage of France
plot(unemployment_france, type= "b",main="Unemployment of 15-74 year-olds in France", xlab="Year", ylab="% of total 15-74 year-olds", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)



### Production comparisons  
plot(production_france, type= "b",main="Wine production", xlab="Year", ylab="Volume (1000hl)", xaxt="n", lty=3, lwd=2, pch=16, cex=0.6, ylim=c(0,65000))
lines(production_italy, type = "l", col = "red")
lines(production_spain, type = "l", col = "blue")
legend("topright", legend = c("France", "Italy","Spain"), col = c("black", "red","blue"), lty = 1, lwd = 2, cex = 0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)
#=> It seems that the 3 time series are correlated (specially between France and Italy)



### Export of wine from France in value vs. volume
exports_france_value <- as.numeric(exports_france_value)
exports_france_value_index = (exports_france_value/exports_france_value[1])*100
exports_france_volume_index = (exports_france_volume/exports_france_volume[1])*100


plot(exports_france_value_index, type= "b",main="Exported value and volume of wine for France", xlab="Year", ylab="Index starting in 100 for both", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6, ylim=c(70,300))
lines(exports_france_volume_index, type = "l", col = "red")
legend("topright", legend = c("Export value", "Export volume"), col = c("black", "red"), lwd = 2, cex = 0.6)
axis(side = 1, at = seq(1, length(years), by = 5), labels = selected_years)









### Correlation between variables

#### Correlation between GDP and export value of wine
cor(gdp_france,exports_france_value, method="pearson") #=> 61%


#### Correlation matric
##### Selecting only the relevant columns for this
corrplot(cor(data[,c(2,3,4,6,7,8,9,10,11,12,13)]), method = "circle", tl.col = "black")





####################
#  Modelling part  #
####################

### Simple regression

tsdisplay(exports_france_value_billions) 
acf(exports_france_value_billions)

##fit a linear regression model 
tt<- 1:NROW(exports_france_value_billions)
fit1 <- lm(exports_france_value_billions~ tt)
summary(fit1)

##plot of the model
plot(tt, exports_france_value_billions, xlab="Time", ylab="Export of wine in billions of €")
abline(fit1, col=3)

#### Testing if the residuals are autocorrelated
dwtest(fit1)

#### Plotting the residuals
resfit1<- residuals(fit1)
plot(resfit1,xlab="Time", ylab="residuals" )




#### TSLM Model gives the exact same results obviously
exports_france_value_billions.ts <- ts(exports_france_value_billions, frequency = 1)
ts.plot(exports_france_value_billions.ts, type="o")

## we fit a linear model with the tslm function
fitts<- tslm(exports_france_value_billions.ts~trend)
summary(fitts)

dwtest(fitts)



#### LM model as a function of the other variables (Multiple Regression)
fit2<- lm(exports_france_value_billions~consumption_world+consumption_france+gdp_france_billions+production_france+production_italy+production_spain+total_exp_france_millions+population_france_millions+unemployment_france)
summary(fit2)
CV(fit2)


dev.new()
plot(exports_france_value_billions)
lines(fitted(fit2), col=2)









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
auto.a<- auto.arima(exports_france_value[6:28])
auto.a 
#ARIMA(p,d,q) here : ARIMA(0,1,0), so the auto ARIMA do just the fist difference and then AR(0) / MA(0)
#AIC = 52.44 

#check residuals (=> white noise) 
#why ? -> graph of residuals / ACF : peaks in the bands / mean = 0 / Ljung-Box test : Q* = 4.224 
checkresiduals(auto.a)

#plot target and the fitted model
plot(exports_france_value[6:28], type= "b",main="Wine exports of France fitted with ARIMA(0,1,0)", xlab="Year", ylab="Value ($)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
fit1<- fitted(auto.a)
lines(fit1, col=2)
axis(side = 1, at = seq(1, length(data$year)-5, by = 5), labels = selected_years2)

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









