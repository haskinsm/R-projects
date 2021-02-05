oliveOil <- read.csv("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/olive.csv")
x = oliveOil[ ,3:10]
library(mclust)
?mclust
plot(Mclust(faithful))
## Get 4 options 
# 1: Want to optimize BIC for this version, so higher value better ( Note Different signs in lecture notes)
# 2:
fit = Mclust(faithful)
fit ##Tells you best model: is EEE, 3.    EEE tells us about like whether equal variance are assumed, are datapoints assumed to be ind etc. And 3 is the number of clusters

plot(Mclust(faithful))
# 2: Gives classification  plot
# 3: The uncertainty, can tell which are well seperated and obv belong to one cluster and which dont (i.e. hard to cluster)
# 4: Gives prob density contour lines

fit = Mclust(x, G=1:15)
fit 
fitk = kmeans(x, 9 , nstart =20)
fitk
adjustedRandIndex(fitk$cl, fit$cl)
adjustedRandIndex(fitk$cl, oliveOil$Region)
adjustedRandIndex(fit$cl, oliveOil$Region)
