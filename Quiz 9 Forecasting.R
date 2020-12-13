require(fma)

tsdisplay(Arima(dowjones, order=c(0,0,0))$residuals)## Want PACF and ACF generally within blue lines => negligible as the residuals is basically just the er
## error epsilon t so want this to be zero from hypothesis so want to pick a model that acheives this fairly well.
tsdisplay(Arima(dowjones, order=c(0,1,0))$residuals) ##Seee OneNotes notes in MA section

Arima(dowjones, order = c(1,1,0)) ## Can decide to minimize either AIC or BIC and then pick the model which has the lowest score for whichever u pick

## Be careful using below function
auto.arima(dowjones, trace=TRUE) ##Returns the best model to fit the timeseries
##Shows you all the combinations it has tried

plot(forecast(Arima(dowjones, order= c(1,1,0)), h=20, level = c(80,95))) ##Then use the best model for computing prediction


tsdisplay(arima.sim(n=1000,list(order = c(0,0,1),ma = c(0.8)), sd = sqrt(0.1796))) ##Ma= c(0.8) is setting phi1 as 0.8
## Takes a while as asking for a lot of values/sims
## This is a pure MA(1) model I think, Can tell by spike at lag 1 in ACF which is followed by zeros
## The more observations the more accurate the model

Arima(dowjones, order=c(3,1,1))

tsdisplay(arima.sim(n=10000,list(order = c(0,0,2),ma = c(0.8,-0.2)), sd = sqrt(0.1796))) ## 0.8 is phi1 and -0.2 is phi2
## Mpving Average of order two will result in us seeing the first two lags in ACF being non-zero and after that have only zeroes
## Anything within the blue lines is negligible so can ignore

tsdisplay(beer, lag.max = 60) ## Can see seasonality in the ACF, repeats every 12 lags, can see 12 explains evrything from the PACF graph, i.e 24 or 36 has nothing left to explain and is negligible
## Can see s=12

## SEASONAL ARIMA(P,D,Q)
Arima(beer,order=c(0,0,0),seasonal=c(1,0,0)) ## P=1 seasonal = c(P,D,Q), order=c(p,d,q)
## Output: Series: beer 
##ARIMA(0,0,0)(1,0,0)[12] with non-zero mean 
## Can see from [12] that s=12, this is stored in memory from R, i.e. R knows the beer data has a freq or s=12 by default
## Expnanatory variable that has been introduced for the beer data is yt-12

?bricksq
## Quarterly data
## Seasonality and trend
tsdisplay(bricksq)
Arima(bricksq,order=c(0,0,0),seasonal=c(1,0,0))
## Can see s=4 from output, 
## so has explanatory variable yt-4
Arima(bricksq,order=c(0,1,0),seasonal=c(1,0,0))
## Diff but since do not include drift only one parameter to estimate, no constant estimated

Arima(bricksq,order=c(0,0,0),seasonal=c(1,0,0), include.drift=TRUE)
## Now have two parameters that are estimated, sar1 and drift
## Here tho including drift does not help, as if we have decided we want to minimize AIC this is not helping and has resulted in an increase in AIC

## Now deciding on a model to fit the data look at the residuals
tsdisplay(Arima(bricksq,order=c(0,0,0),seasonal=c(0,0,0), include.drift=FALSE)$residuals)
## Want to remove the trend so d=1 an do not include the drift by default
tsdisplay(Arima(bricksq,order=c(0,1,0),seasonal=c(0,0,0), include.drift=FALSE)$residuals)
## Want to fulfill our hyptoheses that all thee epislons have a mean of 0 and fixed var
## Can see very strong corelation at lags of 4 in ACF, look at PACF then and see only relavant at 4, gain nothing by 8 or 12 etc => negiligible
## Now will go about removing the seasonal comp, so set D=1 in next attempt

## Check AIC to see if going in right direction
Arima(bricksq,order=c(0,0,0),seasonal=c(0,0,0), include.drift=FALSE)
Arima(bricksq,order=c(0,1,0),seasonal=c(0,0,0), include.drift=FALSE)

tsdisplay(Arima(bricksq,order=c(0,1,0),seasonal=c(0,1,0), include.drift=FALSE)$residuals) ## Set D=1 to remove seasonal comp
Arima(bricksq,order=c(0,1,0),seasonal=c(0,1,0), include.drift=FALSE) ## AIC has decr so going in right direction
## Going to try a seasonal moving average model as have strong spike on the ACF and we some on the PACF

tsdisplay(Arima(bricksq,order=c(0,1,0),seasonal=c(0,1,1), include.drift=FALSE)$residuals)
Arima(bricksq,order=c(0,1,0),seasonal=c(0,1,1), include.drift=FALSE) ## AIC going down so on right track

auto.arima(bricksq)
##Notice auto.arima did not work very well, its best model had a greater AIC than the last one we tried, this is because it did not try our model combo
auto.arima(bricksq, trace=TRUE) ## can now see what models it tried





tsdisplay(Arima(bricksq,order=c(0,0,0),seasonal=c(1,0,0), include.drift=FALSE)$residuals) 


tsdisplay(Arima(airpass,order=c(0,0,0),seasonal=c(0,0,0))$residuals) ## constant c has been fitted
Arima(airpass,order=c(0,0,0),seasonal=c(0,0,0))

Arima(airpass,order=c(0,1,0),seasonal=c(0,0,0)) ## Now no c, as have not included drift
Arima(airpass,order=c(0,1,0),seasonal=c(0,0,0), include.drift =TRUE) ## Now can see constant c has been included
 
tsdisplay(Arima(airpass,order=c(0,1,0),seasonal=c(0,0,0))$residuals, lag.max=100)   
## Can see strong correlations in Acf AT 12, 24, 36 etc, so every 12 have a very strong correlation coeff
## Can see very strong corr at 12 in the PACF graph. This tells us that there is a very strng correlation between yt and yt-12 ***********************************
## and we do not need anyother explanatory variables to explain yt. i.e. nothing for yt-24 to explain
## Looks like a linear decrease in ACF so next move D=1 (or if expo decr P=1)

tsdisplay(Arima(airpass,order=c(0,1,0),seasonal=c(0,1,0))$residuals,lag.max=100) ## Residuals now have constan mean (=0) and var
## Dont have any explanation for the coeefficients we see at 20 as no seasonality there in the airpass data, so juts going to focus on what we see at lag 1
## Going to introduce yt-1 as an explanatory variable, do this by setting p=1
tsdisplay(Arima(airpass,order=c(1,1,0),seasonal=c(0,1,0))$residuals,lag.max=100)
Arima(airpass,order=c(1,1,0),seasonal=c(0,1,0)) ## Only 1 parameter needs to be estimated, phi (from introduction of p), as we use differencing so no constant c
## now check AICs..... looks like we have statinoary mean=0 and stationary var

auto.arima(airpass) ## Gives slightly better result than us => i.e. lower AIC
?auto.arima
? Arima ## Remember capital A Arima is better, as allows you to include drift if you want

##If the data/time series does not ahve a saved freq in r can tell R it has one by:
Arima(ts(airpass, freq=12),order=c(1,1,0),seasonal=c(0,1,0))

tsdisplay(mink, lag.max=100) ##Looks to be seasonality of 10 years, need to set this as follows
Arima(ts(mink, freq=10),order=c(0,0,0),seasonal=c(0,0,0))


tsdisplay(Arima(airpass,order=c(0,0,0),seasonal=c(0,0,0))$residuals) ## All we are seeing here is the episilon at t t+1 ... , have done nothing yet so they arent ~=0

tsdisplay(Arima(airpass,order=c(0,1,0),seasonal=c(1,0,0))$residuals) ## Notice this throws an error as R is failing to fit a stationary model for this time sereis analysis, not satisfied hyptohesis
## Perhaps phi1 coeff was greater/Less than + or -1 

tsdisplay(Arima(airpass,order=c(1,1,0),seasonal=c(10,1,0))$residuals) ## Way to high an order for little p here ## Wil work but this is not a good model at all


Arima(airpass, order=c(0,0,0), seasonal = c(0,1,0)) ## No parameters to estimate here
Arima(airpass, order=c(0,0,0), seasonal = c(0,1,1), include.drift=TRUE) ## Two paramters to estimaet
