##Linear regression stuff

require(fma)  
time = seq(1:length(dowjones)) ##Gives the model the same length as the dowJones times series

myLinearModelwithTime = lm(dowjones~time) ##Fits the linear model    explantory values come after the ~

windows(5,5)
plot(time, dowjones, ylim=c(min(dowjones)-1, max(dowjones) + 1))
par(new=TRUE, ann=FALSE)
plot(time, myLinearModelwithTime$fitted.values, type ="l", col="red", lwd=3,
     ylim=c(min(dowjones)-1, max(dowjones) + 1)) ##Fitted.values gives the predicted/forecasted values
##Red line is the predicted line calc. by linear regression

summary(myLinearModelwithTime)  ##From formula of linear regression a is the intercept etsimate,b is the time estimate
##Note degrees of freedom is n - 2 as we estimated both a and b. N is length(dowjones), so degrees of freedom = 78-2=76

tslm.beer = tslm(beer~ season) ##Automatically defines all the seasonal components
summary(tslm.beer)            
##Would have to define 11 of the 12 seasonal components as explanatory variables if didnt use the tslm function
plot(forecast(tslm.beer, h=20)) ##Light grey is 95% conf. interval, +- 1.96*standard deviation of epislon, otherwise known as the noise
?tslm

tslm.airpass = tslm(airpass ~ trend + season) ##Trend in airpass data so need explanatory variable for trend
summary(tslm.airpass) ##All the estimates are the Beta values which minimise the RSE 
plot(forecast(tslm.airpass, h=20))


require(fma)
##AR(1)
tsdisplay(arima.sim(n=63, list(ar= c(0.8897)), sd = sqrt(0.1796)))
##Simulated a perfect time series, 0.8897 is the coefficient phi 1., have defined the sd of episilon t.
##Cannot have phi1 greater than one
##Arima.sim creates a time series with perfect values that follows a perfect auto regressive model

##AR(2)
tsdisplay(arima.sim(n=63, list(ar= c(0.8897, -0.4858)), sd = sqrt(0.1796)))


##Actual Quiz5:
?bricksq  ##Quarterly data so freq == 4
bricksq
seasonplot(bricksq)
frequency(bricksq) ##Frequency == 4

?HoltWinters
HoltWinters(bricksq, optim.start = c(alpha=0.5, beta = 0.5, gamma = 0.5))$SSE ##SHW+
HoltWinters(bricksq, seasonal = "multiplicative", optim.start = c(alpha=0.10, beta = 0.5, gamma = 0.5))$SSE ##SHWx

##Originally had both optims starting at 0.5 all round and was indicating additive was the best
##but get lower SSE for multpilicative version when start the optim at 0.1 for alpha

bricksq ##Can see from this the Q4 1994 will be the very next forecast, so let n=1
predict(HoltWinters(bricksq, seasonal = "multiplicative", optim.start = c(alpha=0.10, beta = 0.5, gamma = 0.5) ), n.ahead=1)[1]

