install.packages("fma")
require("fma")
tsdisplay(beer)
seasonplot(beer)
plot(decompose(beer))
plot(decompose(airpass, type="multiplicative"))

HoltWinters(dowjones,alpha=0.5,beta=FALSE,gamma=FALSE)
HoltWinters(dowjones,alpha=0.5,beta=FALSE,gamma=FALSE)
plot(forecast(HoltWinters(dowjones,alpha=0.5,beta=FALSE,gamma=FALSE),h=20))
HoltWinters(dowjones,beta=FALSE,gamma=FALSE)
plot(forecast(HoltWinters(dowjones,beta=FALSE,gamma=FALSE),h=20))
HoltWinters(dowjones,alpha =0.5,beta=FALSE,gamma=FALSE)$SSE

A = c(205,200,220,225,210)
tsdisplay(A)
?Acf
Acf(A, 1, plot=FALSE)
