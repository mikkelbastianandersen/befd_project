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


### TRAINING
training_data = data[1:19,]

training_data
### TEST
test_data = data[20:23,]



#### Simple Regression

#FULL SAMPLE
tt_fullsample<- 1:NROW(exports_france_value_billions)
linreg_fullsample <- lm(exports_france_value_billions~ tt_fullsample)
summary(linreg_fullsample)

# DIVIDED INTO TRAINING/TEST SETS
tt_train<- 1:(NROW(exports_france_value_billions)-4)
linreg_train <- lm(exports_france_value_billions[1:19]~ tt_train)
summary(linreg_train)
pred_test <- predict(linreg_train,data.frame(tt_train = c(20,21,22,23)))

# STATISTICS

AIC(linreg_fullsample) #=> 54.77
rmse(exports_france_value_billions[20:23],pred_test)
mae(exports_france_value_billions[20:23],pred_test)





########################
#### Multiple Regression
########################
lm(exports_france_value_billions~tt+gdp_france_billions+total_exp_france_millions+unemployment_france)

## Final Multiple Regression
# Includes: time, gdp, total exports france, unemployment_france
final_mreg <- lm(exports_france_value_billions~tt
                 +gdp_france_billions
                 +total_exp_france_millions
                 +unemployment_france)
summary(final_mreg)
AIC(final_mreg) #=> 21.01


## Final Multiple Regression on training set
final_mreg_train <- lm(exp_france_value~tt_train
                       +gdp_france_value_dollars
                       +total_exp_france_dollars
                       +unemployment_france, data = training_data)



summary(final_mreg_train)


test_data

pred_mreg_test <- predict(final_mreg_train,data.frame(tt_train=c(20,21,22,23), 
                                                      gdp_france_value_dollars = test_data$gdp_france_value_dollars,
                                                      total_exp_france_dollars = test_data$total_exp_france_dollars,
                                                      unemployment_france = test_data$unemployment_france
                                                      ))
pred_mreg_test <- pred_mreg_test/1000000000

## STATISTICS
AIC(final_mreg) #=> 5.98 
rmse(exports_france_value_billions[20:23],pred_mreg_test)
mae(exports_france_value_billions[20:23],pred_mreg_test)



plot(exports_france_value_billions, type= "b",main="France wine exports forecast with stepwise regression on test set", xlab="Year", ylab="Value (in billions of €)", xaxt="n", pch=16, lty=3, lwd=2, cex=0.6)
lines(20:23,pred_mreg_test, col=2)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)



### ARIMA


auto.a<- auto.arima(exports_france_value_billions)
auto.a 
AIC(auto.a) #=> AIC = 52.43


#TEST
auto.a2<- auto.arima(training_data$exp_france_value)
drift1 <- auto.a2$coef[1]
drift1
arima_preds <- c(0,0,0,0)
arima_preds[1] <- test_data$exp_france_value[1]+215555556
arima_preds[2] <- test_data$exp_france_value[1]+2*215555556
arima_preds[3] <- test_data$exp_france_value[1]+3*215555556
arima_preds[4] <- test_data$exp_france_value[1]+4*215555556
arima_preds


### Don't know if this makes sense

arima_preds <- arima_preds/1000000000


### STATISTICS
rmse(exports_france_value_billions[20:23],arima_preds)
mae(exports_france_value_billions[20:23],arima_preds)





### Bass model


bm_train <-BM(training_data$exp_france_value,display = T)
summary(bm_train) 

m <- bm_train$Estimate[1,1]
p <- bm_train$Estimate[2,1]
q <- bm_train$Estimate[3,1]

### Now q is almost not significant anymore
### m and p are not significant

### The predictions for 2019 are found by calculating z(20)-z(19)
### where z(t) is the cumulative fitted value using the parameters from bm_train
bm_preds <- c(0,0,0,0)
bm_preds[1] <- m*((1-exp(-(p+q)*20))/(1+q/p*exp(-(p+q)*20)))-m*((1-exp(-(p+q)*19))/(1+q/p*exp(-(p+q)*19)))
bm_preds[2] <- m*((1-exp(-(p+q)*21))/(1+q/p*exp(-(p+q)*21)))-m*((1-exp(-(p+q)*20))/(1+q/p*exp(-(p+q)*20)))
bm_preds[3] <- m*((1-exp(-(p+q)*22))/(1+q/p*exp(-(p+q)*22)))-m*((1-exp(-(p+q)*21))/(1+q/p*exp(-(p+q)*21)))
bm_preds[4] <- m*((1-exp(-(p+q)*23))/(1+q/p*exp(-(p+q)*23)))-m*((1-exp(-(p+q)*22))/(1+q/p*exp(-(p+q)*22)))
bm_preds <- bm_preds/1000000000


### STATISTICS
### We can't find AIC because the class DIMORA doesn't support the AIC function
rmse(exports_france_value_billions[20:23],bm_preds)
mae(exports_france_value_billions[20:23],bm_preds)





######
### GBM 
######


### 1 Shock
GBMe <- GBM(exports_france_value_billions
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

summary(GBMe)


### 2 Shocks (best attempt)
GBMe2 <- GBM(exports_france_value_billions
             , shock = "exp",nshock = 2
             ,prelimestimates = c(GBMe$Estimate[1,1]
                                  , GBMe$Estimate[2,1]
                                  , GBMe$Estimate[3,1]
                                  , 9
                                  ,GBMe$Estimate[5,1]
                                  ,GBMe$Estimate[6,1]
                                  ,20.5,-1,-1)
             ,oos=5
             , display = T)


summary(GBMe2)

pred_gbm<- predict(GBMe2, newx = c(1:30)) 
pred_gbm.inst<- make.instantaneous(pred_gbm)
pred_gbm.inst

plot(exports_france_value_billions, type= "b",main="France wine exports fitted with BM", xlab="Year", ylab="Value (in billions of €",xaxt="n", pch=16, lty=3, lwd=2, cex=0.6,ylim=c(5,15))
lines(pred_gbm.inst, lwd=2, col=2)
axis(side = 1, at = seq(1, length(data$year), by = 5), labels = selected_years)


#### How can I predict the values of the test set when we have a shock there?






####
#GGM
####

GGM1 <- GGM(exports_france_value_billions,prelimestimates = c(m, 0.001, 0.01, p, q),oos=5)
summary(GGM1)



### PLOT
pred_GGM1<- predict(GGM1, newx=c(1:30))
pred_GGM1.inst<- make.instantaneous(pred_GGM1)

plot(exports_france_value_billions, type= "b",xlab="Time", ylab="Billions of €",  pch=16, lty=3, cex=0.6, xlim=c(1,30))
lines(pred_GGM1.inst, lwd=2, col=2) ## ggm
lines(pred_gbm.inst, lwd=2, col=3) ##gbm with shocks
legend(x = "topright",          # Position
       legend = c("GGM", "GBM with shocks"),  # Legend texts
       col = c(2, 3),           # Line colors
       lwd = 2)                 # Line width)






