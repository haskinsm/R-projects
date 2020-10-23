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

?pollution ##Has trend and seems to have seasonality but hard to tell
tsdisplay(pollution) ##Wont display image unless you make to plot window bigger
tsdisplay(diff( pollution )) ## diff to remove a trend  ##Displays first order differentiation of polution timeseries and its ACF & PACF
pollution 
diff( pollution ) ##The diff function takes the value for february and subtracts it from January
##Notice diff function missing value for Jan
frequency(pollution) ##=12 as monthly data
HoltWinters(pollution, seasonal = "additive") ##The function will let s=12
HoltWinters(pollution, seasonal = "multiplicative") ##Get error message, 'Optimization  failure'
##The functions HoltWinters uses for minimizing the SSE (setting the best values for alpha, beta, gamma), i.e. optimizing, fails
?HoltWinters ##Need to use optim.start, set initial values and R will change these to a better selection of values
HoltWinters(pollution, seasonal = "multiplicative", optim.start = c(alpha =0.3, beta = 0.1,
                                                                    gamma=0.1))
##Throws another error as is not able to move in the right direction of best values from that particular starting point
HoltWinters(pollution, seasonal = "multiplicative", optim.start = c(alpha =0.5, beta = 0.5,
                                                                    gamma=0.5))
##Best to start in the middle by setting them all to 0.5 and optimize the SSE from there and finds a better value of gamma, beta, alpha

tsdisplay(diff( airpass ), lag.max=100)   ##Displays more correlation coefficients
ggtsdisplay(pollution)

frequency(mink) ##=1 which indicates no stored data on freq.
plot(mink)
?mink
##If you think it has a period of 10 can recats using ts function
ts(mink, frequency = 10)
HoltWinters(ts(mink, frequency = 10)) 
##Need the dataset to have a timeseries when using HoltWinters, normally does
##Have a stored value for frequency
seasonplot(ts(mink, frequency = 10))
decompose(ts(mink, frequency = 10))
decompose(beer)

k=5
s=12
Ln = HoltWinters(beer, seasonal ="multiplicative")$coefficients[1]
bn = HoltWinters(beer, seasonal ="multiplicative")$coefficients[2]
snk = HoltWinters(beer, seasonal ="multiplicative")$coefficients[7]
(Ln+k*bn)*snk

##The above code should be equal to:
  predict(HoltWinters(beer, seasonal ="multiplicative"), n.ahead=5)[5]
  
require(fma)
?bank  
tsdisplay(bank$EOM) ##Trend, no seasonality so would have to say DES
tsdisplay(diff(bank$EOM)) ##After emoving trend still no clear seasonal components

HoltWinters(bank$EOM,seasonal ="multiplicative" )$SSE
HoltWinters(bank$EOM)$SSE
HoltWinters(bank$EOM, gamma=FALSE)$SSE ##DES
HoltWinters(bank$EOM, beta =FALSE, gamma=FALSE)$SSE ##SES
