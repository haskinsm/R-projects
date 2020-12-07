require(fma)

tsdisplay(Arima(dowjones, order=c(0,0,0))$residuals)## Want PACF and ACF generally within blue lines => negligible as the residuals is basically just the er
## error epsilon t so want this to be zero from hypothesis so want to pick a model that acheives this fairly well.
tsdisplay(Arima(dowjones, order=c(0,1,0))$residuals) ##Seee OneNotes notes in MA section

Arima(dowjones, order = c(1,1,0)) ## Can decide to minimize either AIC or BIC and then pick the model which has the lowest score for whichever u pick

## Be careful using below function
auto.arima(dowjones, trace=TRUE) ##Returns the best model to fit the timeseries
##Shows you all the combinations it has tried

plot(forecast(Arima(dowjones, order= c(1,1,0)), h=20, level = c(80,95))) ##Then use the best model for computing prediction


tsdisplay(arima.sim(n=100000,list(order = c(0,0,1),ma = c(0.8)), sd = sqrt(0.1796))) ##Ma= c(0.8) is setting phi1 as 0.8
## Takes a while as asking for a lot of values/sims
## This is a pure MA(1) model I think, Can tell by spike at lag 1 in ACF which is followed by zeros

Arima(dowjones, order=c(3,1,1))
