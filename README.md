# Time Series Forecasting Techniques using R

## ARIMA & Auto-ARIMA
Using Arima_Airlines_dataset which contains revenue-passenger-miles every month from 1996,I built a simple ARIMA model.Auto Regressive Integrated Moving Average requires us to manually check for the number of differences,trend and seasonal components based on Auto correlation and Partial Auto correlation plots.

Auto Arima algorithm inherently incorporates the calculation of trend and seasonal components.

## ARIMAX
ARIMAX is implented on sales data which contains Date,Weekday,Day,Customer_Visit and whether the day customer visited is Christmas or not.
ARIMAX provides the way to implement time series forecasting with not only trend and seasonal components but also other independent variables which can affect the dependendent variable.
