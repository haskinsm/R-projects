require(fma)
tsdisplay(arima.sim(n=10000, list(ma=c(3)),sd = sqrt(.1796)))
##Creating a 10000 sample time series, creating a moving average model such that phi1 = 3
##Proved that the MA model will work for any value of Phi, mean and var alwasy stationary (Means they dont depend on t)

tsdisplay(arima.sim(n=10000, list(ar=c(3)),sd = sqrt(.1796)))
##Ar model would not be stationary in mean and variance so this wont work
tsdisplay(arima.sim(n = 10000, list(ar = c(.8)),sd = sqrt(.1796))) ##this will 
##In ar model phi1 has to be between 1 and -1


tsdisplay(dowjones)
tsdisplay(diff(dowjones))
?Arima
tsdisplay(Arima(dowjones, order = c(0,0,0))$residuals) ##Since everything else is zero equation reduces to: yt = c + episilont
##Since called the residuals bit, just displays the episilon values and ignores c (Look at y axis)
##Can see it has a trend
tsdisplay(Arima(dowjones, order = c(0,1,0))$residuals) ## yt - yt-1 = xt = c + episilont ##C is the mean level
##Taking residuals so only getting episilont
## yt = yt-1 + c + +episilont      only have constant parameter c to estimate
## When you differntiate does not complicate at all or add more paramters to estimate

Arima(dowjones, order = c(0,0,0)) ##Coefficient is c there
## BIC stands for bayesian.... bla bla bla
Arima(dowjones, order = c(1,0,0))
Arima(dowjones, order = c(2,0,0)) ## Log likelihood is the sum of square error i think
## more explanatory variables sort of lowers the SSE
##But the more EVs the more complex the model, so introduce a penalty for complexity

##Note have to use capital A Arima function not arima
plot(forecast(Arima(dowjones,order=c(0,1,0),include.drift = TRUE),h=10)) ##means Phi0 is included in equationf ro yt
##yt = phi0 + yt-1 + episilont

plot(forecast(Arima(dowjones,order=c(0,1,0),include.drift = FALSE),h=10)) ## yt = yt-1 + episilont
Arima(dowjones,order=c(0,1,0),include.drift = FALSE) ##Notice no coeff
Arima(dowjones,order=c(0,1,0),include.drift = TRUE) ##Notice coeff
plot(forecast(Arima(dowjones,order=c(0,1,0),include.drift = TRUE),h=10)) ##positive slop of the prediction is cause of the drift

tsdisplay(Arima(dowjones, order = c(0,1,0), include.drift = TRUE)$residuals)  ##Equation that we fit is: yt = phi0 + yt-1 + episilont,
## derivative of this is constant phi0, so predictions increase by phi0
## so if phi0 greater than zero predictions will have positive slope



tsdisplay(Arima(dowjones, order=c(0,0,0))$residuals) ## Trend so set d=1
tsdisplay(Arima(dowjones, order=c(0,1,0))$residuals) ## Stil; looks to be trend
tsdisplay(Arima(dowjones, order=c(0,2,0))$residuals) ## Now looks like noise which is good
## The time plot of the residuals looks better now as it looks like noise, and the mean accross time
## seems to be well centered around 0 i.e. after 2nd order differencing the time series is appearing
## stationary in mean.
## Now looking at the ACF after the 2nd order differencing, I see a major peak at lag 1 then the
## following ones get negligeable. Although major peaks also appears at lags11 to 17, I decided to
## try the MA(1) :
tsdisplay(Arima(dowjones, order=c(0,2,1))$residuals)
  