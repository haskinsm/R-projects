##Linear regression stuff

require(fma)  
time = seq(1:length(dowjones)) ##Gives the model the same length as the dowJones times series

myLinearModelwithTime = lm(dowjones~time) ##Fits the linear model

windows(5,5)
plot(time, dowjones, ylim=c(min(dowjones)-1, max(dowjones) + 1))
par(new=TRUE, ann=FALSE)
plot(time, myLinearModelwithTime$fitted.values, type ="l", col="red", lwd=3,
     ylim=c(min(dowjones)-1, max(dowjones) + 1)) ##Fitted.values gives the predicted/forecasted values
##Red line is the predicted line calc. by linear regression
