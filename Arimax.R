
# Implementation of Arima with multiple predictor variables in R
#The sales data consists of weekday,day number whether the day is christmas or not 
#and number of customer visits

rm(list=ls(all=TRUE))
library(RCurl)
data=read.table(text=getURL("https://raw.githubusercontent.com/rajsiddarth/Time_Series_Forecasting/master/Sales_data"),header=T,sep=",")
str(data)
summary(data)
names(data)
table(data$Christmas)
table(data$Weekday)

#Converting weekdaysto independent categorical variables
library(dummies)
categ_data=data.frame(sapply(data["Weekday"], as.factor))
categ_data=dummy.data.frame(categ_data)

#Changing names 
names(categ_data)=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

#Adding all independent variables in a data frame
ind_data=cbind(categ_data,'Christmas'=data$Christmas,'Day'=data$Day)
head(ind_data)
str(ind_data)

#Creating time series data based on customer visits
data_ts=ts(data$Customer_Visit,frequency=7)

#Plotting time series data
par(mfrow = c(1, 1))
plot(data_ts)
par(mfrow = c(1, 2))
acf(data_ts, lag.max = 20)
pacf(data_ts, lag.max = 20)

#.........Building Auto arima model...................#
library(forecast)
model_arima= auto.arima(data_ts)
summary(model_arima)

#Checking ACF and PACF values
acf(model_arima$residuals, lag.max = 20)
pacf(model_arima$residuals, lag.max = 20)

#Checking randomness for residuals
Box.test(model_arima$residuals, lag = 20, type = c("Ljung-Box"))

# forecasting using arima models

forecast_arima= forecast(model_arima, h=20)

par(mfrow = c(1, 1))
plot(forecast_arima, shadecols = "oldstyle")

library(tseries)

#Adding independent variables
# Find ARIMAX model
model_with_var= auto.arima(data_ts, xreg=ind_data)
summary(model_with_var)
par(mfrow = c(1, 2))
acf(model_with_var$residuals, lag.max = 20)
pacf(model_with_var$residuals, lag.max = 20)
Box.test(model_with_var$residuals, lag = 20, type = c("Ljung-Box"))

#Forecasting of time series data using above model
forecast_data=forecast(model_with_var, xreg=ind_data, h=5)
par(mfrow = c(1, 1))
plot(forecast_data, shadecols = "oldstyle")
forecast_data

