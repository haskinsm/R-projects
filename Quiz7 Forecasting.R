require(fma)
?arima.sim

##Ar(1) model, phi0 = 0 by default
arima.sim(n=63, list(ar = c(0.8897)), sd = sqrt(0.1796)) ##Phi1 here = .8897, sd of the noise = 0.1796 ##Could have meant root 0.1796 here

plot(forecast(arima.sim(n=63, list(ar = c(0.8897)), sd = sqrt(0.1796)), h=100))


plot(forecast(Arima(dowjones, order = c (1,0,0)), h = 1000)) ##Converging to a limit, shown in OneNote notes

plot(forecast(Arima(beer, order = c (1,0,0)), h = 1000)) ##Will always converge -> this behaviour is because of the Ar 1 model => proved this in notes
##All AR models of order 1, 2nd number in list is the integerated, 3rd number is the moving average, both not covered yet