# Forecast the CocaCola prices and Airlines Passengers data set. 
# Prepare a document for each model explaining 
# how many dummy variables you have created and RMSE value for each model. 
# Finally which model you will use for Forecasting.

install.packages("forecast")
install.packages("fpp")
install.packages("smooth")

library(forecast)
library(fpp)
library(smooth)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(readxl)
library(tseries)

cocacola_sale<-read_excel(file.choose())


# Converting data into time series object
time_cocacola<-ts(cocacola_sale$Sales,frequency = 4,start=c(86))
View(time_cocacola)

# dividing entire data into training and testing data 
train_cocacola <- time_cocacola[1:38]
test_cocacola <- time_cocacola[39:42] 
# seasonality is 4 Quarters

# converting time series object
train_cocacola<-ts(train_cocacola,frequency = 4)
test_cocacola<-ts(test_cocacola,frequency = 4)

# Plotting time series data
plot(time_cocacola) 
# plot shows that it has level, trend, 
# seasonality => Additive seasonality 

#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter

holtwinter_alpha<-HoltWinters(train_cocacola,alpha = 0.2,beta = F,gamma = F)
holtwinter_alpha
# Coefficients:
#   [,1]
# a 4020.406

holtwinter_pred<-data.frame(predict(holtwinter_alpha,n.ahead=4))

# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(holtwinter_alpha,h=4))
hwa_mape<-MAPE(holtwinter_pred$fit,test_cocacola)*100

# with alpha = 0.2, beta = 0.3
# Assuming time series data has level and trend parameter 

hw_alpha_beta<-HoltWinters(train_cocacola,alpha = 0.2,beta = 0.3,gamma = F)
hw_alpha_beta
# Coefficients:
#   [,1]
# a 4410.5624
# b  120.3595

hwab_pred<-data.frame(predict(hw_alpha_beta,n.ahead = 4))

# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(hw_alpha_beta,h=4))
hwab_mape<-MAPE(hwab_pred$fit,test_cocacola)*100

# with alpha = 0.2, beta = 0.2, gamma = 0.1 
# Assuming time series data has level,trend and seasonality 

hw_alpha_beta_gamma<-HoltWinters(train_cocacola,alpha = 0.2,beta = 0.2,gamma = 0.1)
hw_alpha_beta_gamma
# Coefficients:
#   [,1]
# a  4325.79025
# b   117.75076
# s1  337.53333
# s2   30.06662
# s3 -317.00736
# s4  326.67416

hwabg_pred<-data.frame(predict(hw_alpha_beta_gamma,n.ahead = 4))

# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(hw_alpha_beta_gamma,h=4))
hwabg_mape<-MAPE(hwabg_pred$fit,test_cocacola)*100

# With out optimum values 
hw_no_alpha<-HoltWinters(train_cocacola,beta = F,gamma = F)
hw_no_alpha
# Smoothing parameters:
#   alpha: 0.502
# beta : FALSE
# gamma: FALSE
# 
# Coefficients:
#   [,1]
# a 4456.709

hwna_pred<-data.frame(predict(hw_no_alpha,n.ahead = 4))
hwna_pred
# fit
# 1 4456.709
# 2 4456.709
# 3 4456.709
# 4 4456.709

plot(forecast(hw_no_alpha,h=4))
hwna_mape<-MAPE(hwna_pred$fit,test_cocacola)*100

hw_noalpha_beta<-HoltWinters(train_cocacola,gamma=F)
hw_noalpha_beta
# Smoothing parameters:
#   alpha: 0.5747386
# beta : 0.3105725
# gamma: FALSE
# 
# Coefficients:
#   [,1]
# a 4581.1447
# b  182.7749

hwnab_pred<-data.frame(predict(hw_noalpha_beta,n.ahead=4))
hwnab_pred
# fit
# 1 4763.920
# 2 4946.695
# 3 5129.470
# 4 5312.244

plot(forecast(hw_noalpha_beta,h=4))
hwnab_mape<-MAPE(hwnab_pred$fit,test_cocacola)*100

hw_no_alpha_beta_gamma<-HoltWinters(train_cocacola)
hw_no_alpha_beta_gamma
# Smoothing parameters:
# alpha: 0.3784328
# beta : 0.2526015
# gamma: 0.8897278
# 
# Coefficients:
#   [,1]
# a  4200.72210
# b   118.93562
# s1  556.79856
# s2   13.14018
# s3 -204.24618
# s4  732.44912

hwnabg_pred<-data.frame(predict(hw_no_alpha_beta_gamma,n.ahead =4))
hwnabg_pred
# fit
# 1 4876.456
# 2 4451.734
# 3 4353.283
# 4 5408.914

plot(forecast(hw_no_alpha_beta_gamma,h=4)) ## this plot gives better results as historical data
hwnabg_mape<-MAPE(hwnabg_pred$fit,test_cocacola)*100

## now we will store all values into dataframe

dataframe_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))

colnames(dataframe_mape)<-c("MAPE","VALUES")
View(dataframe_mape)

## here we got min Mape value for w_no_alpha_beta_gamma ie.2.39 so will accept this model and will go ahead

# Based on the MAPE value who choose holts winter exponential tecnique which assumes the time series
# Data level, trend, seasonality characters with default values of alpha, beta and gamma

new_model <- HoltWinters(time_cocacola)

plot(forecast(new_model,n.ahead=4))

# lets forecast values for the next 4 quarters
forecast_new <- data.frame(predict(new_model,n.ahead=4))

# 1	5215.150
# 2	4672.568
# 3	4556.262
# 4	5630.019