#Project BEFD
#Wine forecasting 


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
library(gam)


####################
#    Import Data   #
####################

data = read_excel("data.xlsx")
data$exp_france_value = as.numeric(data$exp_france_value)

## create new columns

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
gdp_france_billions = gdp_france/1000000000
total_exp_france_millions = total_exp_france/1000000
population_france_millions = population_france/1000000
exports_france_value_billions = as.numeric(exports_france_value)/1000000000


### from 2000 to 2022
selected_years <- data$year[seq(1, length(data$year), by = 5)]




####################
#       Plots      #
####################


### GDP of France
par(mfrow=c(2,2))
plot(gdp_france_billions, type= "b",main="GDP of France", xlab="Year", ylab="Value (in billions of $)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)


### Total export of goods from France
plot(total_exp_france_millions, type= "b",main="Total exports of France", xlab="Year", ylab="Value (in millions of $)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)


### Population of France
plot(population_france_millions, type= "b",main="Population of France", xlab="Year", ylab="Value (in millions)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)


### Unemployment percentage of France
plot(unemployment_france, type= "b",main="Unemployment of 15-74 year-olds in France", xlab="Year", ylab="Pourcentage (%)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)
dev.off()


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

plot(exports_france_value_index, type= "b",main="Indexed exported value and volume of wine for France", xlab="Year", ylab="Pourcentage (%)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6, ylim=c(70,300))
lines(exports_france_volume_index, type = "l", col = "red")
legend("topright", legend = c("Export value", "Export volume"), col = c("black", "red"), lwd = 2, cex = 0.6)
axis(side = 1, at = seq(1, length(years), by = 5), labels = selected_years)


### Correlation between variables
corrplot(cor(data[,c(2,3,4,6,7,8,9,10,11,12,13)]), order="hclust", tl.col="black", tl.srt=45)









####################
#  Modelling part  #
####################


#####################
### Simple regression
#####################

tsdisplay(exports_france_value_billions) 
acf(exports_france_value_billions)

##fit a linear regression model 
tt<- 1:NROW(exports_france_value_billions)
fit1 <- lm(exports_france_value_billions~ tt)
summary(fit1)
AIC(fit1) #=> 54.77

##plot of the model
plot(tt, exports_france_value_billions, main = "France wine exports fitted with a linear regression model", xlab="Year", ylab="Value (in millions of $)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
lines(fitted(fit1), col=3)
axis(side = 1, at = seq(1, length(years), by = 5), labels = selected_years)

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









########################
#### Multiple Regression
########################

fit2<- lm(exports_france_value_billions~tt+consumption_world+consumption_france+gdp_france_billions+production_france+production_italy+production_spain+total_exp_france_millions+population_france_millions+unemployment_france+exports_france_volume)
summary(fit2)
AIC(fit2) #=> 13.42


fit3 <- step(fit2, direction="both")
summary(fit3)
AIC(fit3) #=> 4.09 BETTER

#exports_france_volume non-significant we can remove it even if the AIC goes a little up

fit4 <- lm(exports_france_value_billions~tt+gdp_france_billions+production_france+total_exp_france_millions+population_france_millions)
summary(fit4)
AIC(fit4)

plot(exports_france_value_billions, type= "b",main="France wine exports forecast with stepwise regression", xlab="Year", ylab="Value (in millions of $)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
lines(fitted(fit4), col=2)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)










#########
### ARIMA 
#########
#there is a trend so the time series is not stationary 


plot(exports_france_value_billions, type= "b",main="France wine exports", xlab="Year", ylab="Value (in millions of $)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)

#acf peaks decrease slowly, confirming the presence of a linear trend
tsdisplay(exports_france_value) 

#first differentiation
exports_france_value.diff <- diff(exports_france_value,differences=1)

#it's better and we can see that there is no seasonality
tsdisplay(exports_france_value.diff) 

# very difficult to find p and q (think it's better to them equal to 0)


############
#auto arima model 
############
auto.a<- auto.arima(exports_france_value_billions)
auto.a 
AIC(auto.a) #=> AIC = 52.43

#ARIMA(p,d,q) here : ARIMA(0,1,0), so the auto ARIMA do just the fist difference and then AR(0) / MA(0)

#check residuals (=> white noise) 
#why ? -> graph of residuals / ACF : peaks in the bands / mean = 0 / Ljung-Box test : Q* = 4.224 
checkresiduals(auto.a)

#plot target and the fitted model
plot(exports_france_value_billions, type= "b",main="France wine exports fitted with ARIMA(0,1,0)", xlab="Year", ylab="Value (in millions of $)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
lines(fitted(auto.a), col=2)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)

#plot the forecast
autoplot(forecast(auto.a))


#Forecast 

year_forecast = 5
forecast.ARIMA = forecast(auto.a)

plot(exports_france_value_billions,xlim=c(0,23+year_forecast),ylim=c(5,20),type= "b",main="France wine exports forecast", xlab="Year", ylab="Value (in millions of $)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)

lines(forecast.ARIMA$upper[,2],col="blue",lty=2,type= "b",pch=16, lwd=2, cex=0.6)
lines(forecast.ARIMA$mean,col="red",lty=2,type= "b",pch=16, lwd=2, cex=0.6)
lines(forecast.ARIMA$lower[,2],col="blue",lty=2,type= "b",pch=16, lwd=2, cex=0.6)
legend("topright",legend=c("Upper and lower bands", "Mean forecasting"),col=c("blue","red"),pch=3,lty = 1, lwd = 2, cex = 0.6)
axis(side = 1, at = seq(1, length(data$year)+5, by = 5), labels = c(selected_years,c(2025)))










######
### BM 
######

bm_exports_france_value_billions<-BM(exports_france_value_billions,display = T)
summary(bm_exports_france_value_billions)
#m and q non-significant and we don't take into account the shocks

pred_bm<- predict(bm_exports_france_value_billions, newx = c(1:30)) #newx : time windows
pred.inst<- make.instantaneous(pred_bm)

plot(cumsum(exports_france_value_billions),type= "b",main="Cumulative sum of wine exports and BM", xlab="Year", ylab="Cumulative value (in millions of $)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
lines(pred_bm, lwd=2, col=2)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)

plot(exports_france_value_billions, type= "b",main="France wine exports fitted with BM", xlab="Year", ylab="Value (in millions of $",xaxt="n", pch=16, lty=3, lwd=2, cex=0.6,ylim=c(5,15))
lines(pred.inst, lwd=2, col=2)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)


#GBM with 2 shocks (one in 2009 and one in 2020) 

m = 3.784530e+03
p = 1.315909e-03
q = 3.506564e-02


#exponential

GBMe<- GBM(exports_france_value_billions, shock = "exp",nshock = 1,prelimestimates = c(m, p, q, 9,-0.9,3))

#GBMe<- GBM(exports_france_value_billions,shock = "exp",nshock = 2,prelimestimates = c(m, p, q, 9,-0.3,3,21,-0.1,-1))

#pred_GBMe<- predict(GBMe, newx=c(1:30))
#pred_GBMe.inst<- make.instantaneous(pred_GBMe)

#plot(cumsum(exports_france_value_billions), type= "b",xlab="Quarter", ylab="Cumulative revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,30))
#lines(pred_GBMe, lwd=2, col=2)

#plot(tw, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,30))
#lines(pred_GBMe.inst, lwd=2, col=2)










####
#GAM
####


g1 = gam(exports_france_value_billions~tt+consumption_world+consumption_france+gdp_france_billions+production_france+production_italy+production_spain+total_exp_france_millions+population_france_millions+unemployment_france+exports_france_volume)
summary(g1)
AIC(g1) #=> 13.42 it's the same as multiple regression because we didn't add splines


#Perform stepwise selection using gam scope

#create a data of the same features, mandatory for the scope (we take all the features)
new_data <- data.frame(exports_france_value_billions = exports_france_value_billions, tt=tt, consumption_world = consumption_world, gdp_france_billions = gdp_france_billions, production_france = production_france, production_italy = production_italy, production_spain = production_spain, total_exp_france_millions = total_exp_france_millions , population_france_millions = population_france_millions, unemployment_france=unemployment_france,exports_france_volume=exports_france_volume)

#the target is the first column so we have to specify it in the scope
sc = gam.scope(new_data, response=1, arg=c("df=2","df=3","df=4"))
g2<- step.Gam(g1, scope=sc, trace=T)
summary(g2)
AIC(g2) #-23

par(mfrow=c(3,3))
plot(g2, se=T)
dev.off()

#PROBLEM here is than the Nonparametric parameters are NON significant
#=> useless to use GAM even if the AIC is smaller
#=> here warning model too complex, maybe if we try something manually it will work


#MANUALLY

#g3 here is stepwise multiple regression
g3 = gam(exports_france_value_billions~tt+gdp_france_billions+production_france+total_exp_france_millions+population_france_millions)
summary(g3)
AIC(g3) #5.98

new_data2 <- data.frame(exports_france_value_billions = exports_france_value_billions, tt=tt, gdp_france_billions = gdp_france_billions, production_france = production_france, total_exp_france_millions = total_exp_france_millions , population_france_millions = population_france_millions)
sc = gam.scope(new_data2, response=1, arg=c("df=2","df=3","df=4"))
g<- step.Gam(g3, scope=sc, trace=T)
summary(g3) #here the stepwise change nothing 
AIC(g3) 

