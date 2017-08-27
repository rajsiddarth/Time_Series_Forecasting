#Implementing time series forecasting using Auto ARIMA method
# Dataset contains Revenue Passenger Miles for an airline same as the one used to implement Arima 
#   Unit: Thousand Miles
#   Data Source: http://www.bts.gov/xml/air_traffic/src/index.xml
#   and https://datamarket.com/data/set/281x/us-air-carrier-traffic-statistics-revenue-passenger-miles 

rm(list=ls(all=TRUE))
library(RCurl)
data = read.table(text=getURL("https://raw.githubusercontent.com/rajsiddarth/Time_Series_Forecasting/master/Arima_airmiles_dataset.csv"),
                  header = T,sep = ",")
str(data)

#We do not divide into test and train data.Hence not computing Error  metrics
#This code just shows the implementation of Auto arima in R
#Converting data to time series 
library(stats)
data_ts=ts(data, frequency = 12, start = c(1996,1))
data_ts

#Plotting time series
par(mfrow=c(1,1))
plot(data_ts)

#Decomposing time series into trend,seasonality and randomness assuming additive model
data_ts_components= decompose(data_ts, type = "additive")
plot(data_ts_components)

#ACF and PACF of data
par(mfrow=c(1,3))
plot.ts(data_ts)
acf(data_ts, lag.max=20)
pacf(data_ts, lag.max=20)

# Build time series model using auto.arima
library(forecast)

model_autoarima= auto.arima(data_ts,ic='aic')
model_autoarima

#Forecasting using auto arima model
data_forecasts=forecast(model_autoarima,h=36)
par(mfrow=c(1,1))
plot(data_forecasts)
data_forecasts

