require(fma) ## has seasonality of 12
myts = read.csv("TimeSeries.csv") ## Reads in time series number 12
dim(myts) ## This time series has 314 observations
## Answer 1: n=314

tsdisplay(myts)


myts[1:3,]
## First 3 values of time series
mean(myts[1:3,])
## Answer 2: 616. Think correct: https://www.thefreedictionary.com/Empirical+mean

2/(1-0.5-(-0.5))
## Answer 3: 2

## Q15/16:

## Will aim to minimise AIC:
Arima(ts(myts[1:314,], freq=12),order=c(0,0,0), seasonal = c(0,0,0)) ## Starting AIC 5026.22
tsdisplay(Arima(ts(myts[1:314,], freq=12),order=c(0,0,0), seasonal = c(0,0,0))$residuals, lag.max = 314) 
## Looking at the ACF graph here we can see there is major peaks every s (s=12) time periods. 
## Because of this and also since this is not seen in 
## the PACF it is possible to rule out using Q 
## for the moment. Since the major peaks (every s time periods) do not appear to be decreasing exponentially, 
## but in a linear fashion in the ACF graph we should set D=1. 

Arima(ts(myts[1:314,], freq=12),order=c(0,0,0), seasonal = c(0,1,0)) ##  AIC 3560.6
tsdisplay(Arima(ts(myts[1:314,], freq=12),order=c(0,0,0), seasonal = c(0,1,0))$residuals, lag.max = 114)

## There now appears to be no seasonality. There are major peaks at lag 1 in both the ACF and PACF. 
## There are smaller, but still notable coefficents at lag 2, 3, 4, 5 and 8 in the ACF graph.
## So I will try p=1.
Arima(ts(myts[1:314,], freq=12),order=c(1,0,0), seasonal = c(0,1,0)) ##  AIC 3508.38
tsdisplay(Arima(ts(myts[1:314,], freq=12),order=c(1,0,0), seasonal = c(0,1,0))$residuals, lag.max = 24)
## There is now a major peak at lag 12, the freq of the initial time series is 12, so I will set Q=1

Arima(ts(myts[1:314,], freq=12),order=c(1,0,0), seasonal = c(0,1,1)) ##  AIC 3499.82
tsdisplay(Arima(ts(myts[1:314,], freq=12),order=c(1,0,0), seasonal = c(0,1,1))$residuals, lag.max = 314)
## There now appears to be a notable peak at lag at k=1 in both the ACF and Pacf. I will set q=1.

Arima(ts(myts[1:314,], freq=12),order=c(1,0,1), seasonal = c(0,1,1)) ##  AIC 3479.88
tsdisplay(Arima(ts(myts[1:314,], freq=12),order=c(1,0,1), seasonal = c(0,1,1))$residuals, lag.max = 14)
## All of the lags except lag k=7 appear to now be negligible, the mean and var appear to be stationary and 
## since there is nothing to explain this peak at lag k=7 in both the PACF and ACF this is as far as I can go from visual inspection.

Arima(ts(myts[1:314,], freq=12),order=c(1,0,1), seasonal = c(0,1,1), include.drift = TRUE) ## AIC 3477.74
tsdisplay(Arima(ts(myts[1:314,], freq=12),order=c(1,0,1), seasonal = c(0,1,1), include.drift = TRUE)$residuals, lag.max = 14)


auto.arima(ts(myts[1:314,], freq=12))
##The model found by auto.arima
Arima(ts(myts, freq=12),order=c(2,1,1), seasonal = c(0,1,1)) 
tsdisplay(Arima(ts(myts[1:314,], freq=12),order=c(2,1,1), seasonal = c(0,1,1))$residuals)
