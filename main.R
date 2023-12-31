#Project BEFD
#Wine forecasting 


####################
# Check list for the models #
# Simple regression - Check
#     We can see a clear linear trend from this
# Multiple regression - Check
#     This performs very well, but how do we check for overfitting?
# ARIMA - Check
#     The auto.arima suggests an ARIMA(0,1,0) model which is just a random walk
#     This suggests that the ARIMA model is not appropriate for our data
# Bass Model (BM) - Check
#     Seems to work okay, but it looks alot like the simple regression
# Generalized Bass Model (GBM) - 

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
library(Metrics)

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
plot(tt, exports_france_value_billions, main = "France wine exports fitted with a linear regression model", xlab="Year", ylab="Value (in billions of €)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
lines(fitted(fit1), col=3)
axis(side = 1, at = seq(1, length(years), by = 5), labels = selected_years)

#### Testing if the residuals are autocorrelated
dwtest(fit1)

#### Plotting the residuals
resfit1<- residuals(fit1)
plot(resfit1,xlab="Time", ylab="residuals",xaxt="n", pch=20, lty=5, lwd=5, cex=0.6,col = "blue")
axis(side = 1, at = seq(1, length(years), by = 5), labels = selected_years)

rmse(exports_france_value_billions,fitted(fit1))
mae(exports_france_value_billions,fitted(fit1))



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

fit4 <- lm(exports_france_value_billions~tt+gdp_france_billions+total_exp_france_millions+unemployment_france)
summary(fit4)
AIC(fit4)
vif(fit4)

plot(exports_france_value_billions, type= "b",main="France wine exports forecast with stepwise regression", xlab="Year", ylab="Value (in billions of €)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
lines(fitted(fit4), col=2)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)

#### Testing if the residuals are autocorrelated
dwtest(fit4)

#### Plotting the residuals
resfit1<- residuals(fit4)
plot(resfit1,main="Residuals graph",xlab="Year", ylab="Value",xaxt="n", pch=20, lty=5, lwd=5, cex=0.6,col = "blue")
axis(side = 1, at = seq(1, length(years), by = 5), labels = selected_years)




rmse(exports_france_value_billions,fitted(fit4))
mae(exports_france_value_billions,fitted(fit4))

#####try all possible configurationhttp://127.0.0.1:28797/graphics/plot_zoom_png?width=603&height=421

#variables : 

#tt
#gdp_france_billions
#production_france
#total_exp_france_millions
#population_france_millions


fit5 <- lm(exports_france_value_billions~tt+total_exp_france_millions)
summary(fit5)
AIC(fit5)
vif(fit5)

plot(exports_france_value_billions, type= "b",main="France wine exports forecast with stepwise regression", xlab="Year", ylab="Value (in billions of €)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
lines(fitted(fit5), col=2)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)

rmse(exports_france_value_billions,fitted(fit5))
mae(exports_france_value_billions,fitted(fit5))


#########
### ARIMA 
#########
tsdisplay(exports_france_value) 


### WE HAVE A CLEAR LINEAR TREND SO WE NEED TO DO DIFFERENCING

#first differencing
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
#ARIMA(0,1,0) is just a random walk so Y_t = Y_(t-1) + average_trend
#check residuals (=> white noise) 

checkresiduals(auto.a)

#plot target and the fitted model
plot(exports_france_value_billions, type= "b",main="France wine exports fitted with ARIMA(0,1,0)", xlab="Year", ylab="Value (in billions of €)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
lines(fitted(auto.a), col=2)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)

#plot the forecast
autoplot(forecast(auto.a))


#Forecast 

year_forecast = 5
forecast.ARIMA = forecast(auto.a)

plot(exports_france_value_billions,xlim=c(0,23+year_forecast),ylim=c(5,20),type= "b",main="France wine exports forecast", xlab="Year", ylab="Value (in billions of €)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)

lines(forecast.ARIMA$upper[,2],col="blue",lty=2,type= "b",pch=16, lwd=2, cex=0.6)
lines(forecast.ARIMA$mean,col="red",lty=2,type= "b",pch=16, lwd=2, cex=0.6)
lines(forecast.ARIMA$lower[,2],col="blue",lty=2,type= "b",pch=16, lwd=2, cex=0.6)
legend("topright",legend=c("Upper and lower bands", "Mean forecasting"),col=c("blue","red"),pch=3,lty = 1, lwd = 2, cex = 0.6)
axis(side = 1, at = seq(1, length(data$year)+5, by = 5), labels = c(selected_years,c(2025)))

rmse(exports_france_value_billions,fitted(auto.a))
mae(exports_france_value_billions,fitted(auto.a))








######
### BM 
######

bm_exports_france_value_billions<-BM(exports_france_value_billions,display = T)
summary(bm_exports_france_value_billions)
#m and p non-significant and we don't take into account the shocks

pred_bm<- predict(bm_exports_france_value_billions, newx = c(1:30)) #newx : time windows
pred.inst<- make.instantaneous(pred_bm)

plot(cumsum(exports_france_value_billions),type= "b",main="Cumulative sum of wine exports and BM", xlab="Year", ylab="Cumulative value (in billions of €)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
lines(pred_bm, lwd=2, col=2)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)

plot(exports_france_value_billions, type= "b",main="France wine exports fitted with BM", xlab="Year", ylab="Value (in billions of €)",xaxt="n", pch=16, lty=3, lwd=2, cex=0.6,ylim=c(5,15))
lines(pred.inst, lwd=2, col=2)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)

rmse(exports_france_value_billions,pred.inst[0:23])
mae(exports_france_value_billions,pred.inst[0:23])



######
### GBM 
######


#GBM with 2 shocks (one in 2009 and one in 2020) 


## estimates from BM
m = 3.784530e+03
p = 1.315909e-03
q = 3.506564e-02


#exponential shock

### 1 Shock
GBMe1 <- GBM(exports_france_value_billions
             , shock = "exp"
             ,nshock = 1
             ,prelimestimates = c(5.573871e+03
                                  , 9.234885e-04
                                  , 3.300564e-02
                                  , 9.4
                                  ,-1.7
                                  ,-5.4)
             ,oos=5
             , display = T)

summary(GBMe1)


pred_gbm1<- predict(GBMe1, newx = c(1:30)) 
pred_gbm1.inst<- make.instantaneous(pred_gbm1)

plot(exports_france_value_billions, type= "b",main="France wine exports fitted with GBM (2 shocks)", xlab="Year", ylab="Value (in billions of €)",xaxt="n", pch=16, lty=3, lwd=2, cex=0.6,ylim=c(5,15))
lines(pred_gbm1.inst, lwd=2, col=2)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)

rmse(exports_france_value_billions,pred_gbm1.inst[0:23])
mae(exports_france_value_billions,pred_gbm1.inst[0:23])


### 2 Shocks (best attempt)
GBMe2 <- GBM(exports_france_value_billions
             , shock = "exp",nshock = 2
             ,prelimestimates = c(GBMe1$Estimate[1,1]
                                  , GBMe1$Estimate[2,1]
                                  , GBMe1$Estimate[3,1]
                                  , 9
                                  ,GBMe1$Estimate[5,1]
                                  ,GBMe1$Estimate[6,1]
                                  ,20.5,-1,-1)
             ,oos=5
             , display = T)


summary(GBMe2)


pred_gbm2<- predict(GBMe2, newx = c(1:30)) 
pred_gbm2.inst<- make.instantaneous(pred_gbm2)

plot(exports_france_value_billions, type= "b",main="France wine exports fitted with GBM (2 shocks)", xlab="Year", ylab="Value (in billions of €)",xaxt="n", pch=16, lty=3, lwd=2, cex=0.6,ylim=c(5,15))
lines(pred_gbm2.inst, lwd=2, col=2)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)

rmse(exports_france_value_billions,pred_gbm2.inst[0:23])
mae(exports_france_value_billions,pred_gbm2.inst[0:23])




####
#GGM
####

GGM1 <- GGM(exports_france_value_billions,prelimestimates = c(m, 0.001, 0.01, p, q),oos=5)
summary(GGM1)



### PLOT
pred_GGM1<- predict(GGM1, newx=c(1:30))
pred_GGM1.inst<- make.instantaneous(pred_GGM1)

plot(exports_france_value_billions, type= "b",xlab="Year", ylab="Value (in billions of €)", xaxt="n", pch=16, lty=3, cex=0.6, xlim=c(1,30))
lines(pred_GGM1.inst, lwd=2, col=2) ## ggm
lines(pred_gbm.inst, lwd=2, col=3) ##gbm with shocks
legend(x = "topleft",          # Position
       legend = c("GGM", "GBM with shocks"),  # Legend texts
       col = c(2, 3),           # Line colors
       lwd = 2)                 # Line width)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)


rmse(exports_france_value_billions,pred_GGM1.inst[0:23])
mae(exports_france_value_billions,pred_GGM1.inst[0:23])


##################
#Gradient boosting
##################





































####
#GAM
####

## I take all the regressors from the multiple regression

g1 = gam(exports_france_value_billions ~ s(tt) + s(gdp_france_billions) + s(production_france) + s(total_exp_france_millions) + s(population_france_millions))
summary(g1) 
AIC(g1) #=> 16.63996 after adding splines ((compared to 5.982558 for the multiple regression))

#Perform stepwise selection using gam scope

#create a data of the same features, mandatory for the scope (we take all the features)
new_data <- data.frame(exports_france_value_billions = exports_france_value_billions, tt=tt, consumption_world = consumption_world, gdp_france_billions = gdp_france_billions, production_france = production_france, production_italy = production_italy, production_spain = production_spain, total_exp_france_millions = total_exp_france_millions , population_france_millions = population_france_millions, unemployment_france=unemployment_france,exports_france_volume=exports_france_volume)

#the target is the first column so we have to specify it in the scope
sc = gam.scope(new_data, response=1, arg=c("df=2","df=3","df=4"))
g2<- step.Gam(g1, scope=sc, trace=T)
summary(g2)
AIC(g2) # 16.63996 just as before

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




## tried removing , production_france = production_france
new_data3 <- data.frame(exports_france_value_billions = exports_france_value_billions, tt=tt, gdp_france_billions = gdp_france_billions, total_exp_france_millions = total_exp_france_millions , population_france_millions = population_france_millions)
sc = gam.scope(new_data3, response=1, arg=c("df=2","df=3","df=4"))
g<- step.Gam(g3, scope=sc, trace=T)
summary(g3) #here the stepwise change nothing 
AIC(g3) 

