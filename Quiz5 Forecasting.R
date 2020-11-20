require(fma)
?Arima
Arima(dowjones, order = c(1,0,0)) ##1st number in concatanation is the order
##Have notes on this in OneNote
?arima
plot(dowjones) ##Has trend so not very suitable for arima models
plot(beer) ##Has seasonality so not very suitable
##As not stationary in mean, var
Arima(beer, order=c(1,0,0))
Arima(beer, order=c(2,0,0)) ##Number under ar1 and ar2 are R's best estimates for the best parameter for phi 1 and phi2
##Picked in order to minimize SSE while ensuring constrainst are obeyed

forecast(Arima(dowjones, order = c(1,0,0)), h=1)
plot(forecast(Arima(dowjones, order = c(1,0,0)), h=10))
plot(forecast(Arima(dowjones, order = c(1,0,0)), h=1000)) ##Can see Conf. Intervals grow quickly and then appear to be bounded at far away predictions

##
