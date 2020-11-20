require(fma)
?arima.sim

##Ar(1) model, phi0 = 0 by default
arima.sim(n=63, list(ar = c(0.8897)), sd = sqrt(0.1796)) ##Phi1 here = .8897, sd of the noise = 0.1796 ##Could have meant root 0.1796 here

plot(forecast(arima.sim(n=63, list(ar = c(0.8897)), sd = sqrt(0.1796)), h=100))


plot(forecast(Arima(dowjones, order = c (1,0,0)), h = 1000)) ##Converging to a limit, shown in OneNote notes

plot(forecast(Arima(beer, order = c (1,0,0)), h = 1000)) ##Will always converge -> this behaviour is because of the Ar 1 model => proved this in notes
##All AR models of order 1, 2nd number in list is the integerated, 3rd number is the moving average, both not covered yet.


tslm.airpass = tslm(airpass~ trend)
tslm.airpass
?tslm
airpass
plot(airpass) ##Clear trend and yearly seasonality,
plot(forecast(tslm.airpass, h=20))
summary(tslm.airpass) ##Can tell how many explanatory variables there are by looking at coefficients section (Intercept doesnt count) *******************************Imp***********
##So 1 Explanatory Variable, the trend, in tslm.airpass

tslm.airpass2<-tslm(airpass~ trend + season)
summary(tslm.airpass2) #Can see there are 12 explanatroy variables. The reasoning for this is that there is no need to include an explanatory variable for Jan, 
##so have 11 Explanatory variables for the other months and 1 for trend

tslm.airpass3<-tslm(airpass~ season)
summary(tslm.airpass3) ##Can see there are 11 explanatroy variables. The reasoning for this is that there is no need to include an explanatory variable for Jan, 
##so have 11 Explanatory variables for the other months
