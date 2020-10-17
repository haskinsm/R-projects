require("fma")
HoltWinters(dowjones,alpha=0.3, beta=0.3, gamma=FALSE)
plot(forecast(HoltWinters(dowjones,gamma=FALSE),h=20))
forecast(HoltWinters(dowjones,gamma=FALSE),h=20)
HoltWinters(dowjones,gamma=FALSE)$SSE

?HoltWinters
HoltWinters(beer,alpha=0.1,beta=0.5, gamma=0.8, seasonal ="additive")  #Output A is the last observation, b the last tren, s1 the last seasonal
forecast(HoltWinters(beer,alpha=0.1,beta=0.5, gamma=0.8, seasonal ="additive"))
plot(forecast(HoltWinters(beer, seasonal ="additive"), h=5*12)) #h=5*12 gives next 5 years
HoltWinters(beer, seasonal ="additive")
HoltWinters(dowjones, seasonal ="additive") ##Note: it will not worth for dowjones as no seasonal component
##To get it to work have to do:
HoltWinters(ts(beer,freq=12), seasonal ="additive") ##Must identify it as timeseries with frequency = x
plot(forecast(HoltWinters(beer, seasonal ="multiplicative"), h=5*12))
HoltWinters(beer, seasonal ="multiplicative")
##F1 = (a+b*1)*s1  Check using the following line:
forecast(HoltWinters(beer, seasonal ="multiplicative"), h=1)
HoltWinters(beer,seasonal ="multiplicative" )$SSE
HoltWinters(beer,seasonal ="additive" )$SSE
plot(HoltWinters(beer,seasonal ="additive" )$fitted)  ##xhat is forecast, 
plot(HoltWinters(beer,seasonal ="multiplicative" )$fitted)
