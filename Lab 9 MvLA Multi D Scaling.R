## Olive Oil data set
library(MASS) ## for sammon and kruskals non metric method
install.packages(vegan) ## For procrustes analysis
library(vegan)

oliveOil <- read.csv("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/olive.csv")
dim(oliveOil)
oliveOil
oliveOil[ ,3:10]

# As there are 572 observations within the olive oil data set, we will work with a subset so that results can be
# easily visualised. However, we will ensure the subset selected has the same number from each of the three
# regions that can be used as labelling for the data.
# In the olive oil data, the first 323 observations come from the northern region of Italy, observations 324 to
# 421 come from the southern region of Italy, and the remaining observations came from Sardinia

N <- sample(1:323, size = 30) ## Takes 30 from each region which makes graphing easy
D <- sample(324:421, size = 30)
S <- sample(422:572, size = 30)
acidsamp <- oliveOil[c(N, D, S), 3:10]

# (Metric) Classical Dimensional Scaling
?cmdscale
loc = cmdscale(dist(acidsamp), k = 2, eig = TRUE) # A function that performs (metric) classical scaling is cmdscale.
loc
## Setiing k=2 means reducing the dataset to 2D so makes it easy to graph

x <- loc$points[,1]
y <- loc$points[,2]
cols <- c(rep(1,30),rep(2,30),rep(3,30))
plot(x, y, type = "n", xlab = "", ylab ="", main = "Classical")
text(x, y, oliveOil[c(N, D, S), 1], cex = 1, col = cols)
# Since we want to label each observation by its region of origin, the initial plot does not produce any points.
# This is achieved by setting the argument type="n". The text function then adds the appropriate region
# names to the plot.

# We can calculate the criterion discussed in class to select the optimal dimension ourselves by fitting a
# configuration within an n − 1 dimensional space and examining the eigenvalues:
loc_total <- cmdscale(dist(acidsamp), k = (nrow(acidsamp)-1), eig=TRUE) ## I think what hes doing here is comparing our method to the exact method (i.e. have explanatory varaibles for every data point so since this 
sum(abs(loc_total$eig[1:2])) / sum(abs(loc_total$eig)) # is 90 readings long will have (n-1) 89 explanatory variables (n-1 as remember with seasonality in forecasting dont need every one as base model can account for one so hence n-1)
# Some eigenvalues are negative – note the warning message that states this. Hence we have calculated a
# modified criterion by applying it to the absolute value of the eigenvalues. The cmdscale function returns a
# GOF vector which computes two measures for the goodness of fit of the configuration. See the help file for
# more details. These values should coincide very closely with your own calculation.


#Metric Least Squares and Kruskal’s Non-Metric Scaling
# Sammon’s metric least squares and Kruskal’s non-metric methods are available in the MASS package. Use the
# function sammon to perform Sammon’s metric least squares technique and isoMDS for Kruskal’s method.
?sammon
?isoMDS

loc2 = sammon(dist(acidsamp), k =2)
loc2
sammon(dist(acidsamp)) ## Seems to have k=2 by default. Either that or it has decided k=2 is the best. Fairly sure k=2 default
x2 <- loc2$points[,1]
y2 <- loc2$points[,2]
cols2 <- c(rep(1,30),rep(2,30),rep(3,30))
plot(x2, y2, type = "n", xlab = "", ylab ="", main = "Sammon")
text(x2, y2, oliveOil[c(N, D, S), 1], cex = 1, col = cols2)

loc3 = isoMDS(dist(acidsamp), k=2) ## Default is k=2
loc3
x3 <- loc3$points[,1]
y3 <- loc3$points[,2]
cols3 <- c(rep(1,30),rep(2,30),rep(3,30))
plot(x3, y3, type = "n", xlab = "", ylab ="", main = "Kruskals non-metric scaling")
text(x3, y3, oliveOil[c(N, D, S), 1], cex = 1, col = cols3)


# Procrustes analysis
#We can examine how well the three configurations match using the function procrustes within the package vegan

# To compare the results of the classical scaling object loc, the metric least squares scaling object loc2, and
# Kruskal’s scaling object loc3, try the following:
#library(vegan)
proc12 <- procrustes(loc$points,loc2$points)
proc23 <- procrustes(loc2$points,loc3$points)
proc13 <- procrustes(loc3$points,loc$points)

#Multiple plot types are available. How should you interpret the plots below?
plot(proc12)
plot(proc12, kind = 2)
plot(proc23)
plot(proc23, kind = 2)
plot(proc13)
plot(proc13, kind = 2)

## 1 and 3 are most similar??


# To standardize use scale function I think
locS = cmdscale(dist(scale(acidsamp)), k = 2, eig = TRUE) 
locS
xS <- locS$points[,1]
yS <- locS$points[,2]
colsS <- c(rep(1,30),rep(2,30),rep(3,30))
plot(xS, yS, type = "n", xlab = "", ylab ="", main = "Classical (Standardized)")
text(xS, yS, oliveOil[c(N, D, S), 1], cex = 1, col = colsS)


loc2S = sammon(dist(scale(acidsamp)), k =2)
loc2S
x2S <- loc2S$points[,1]
y2S <- loc2S$points[,2]
cols2S <- c(rep(1,30),rep(2,30),rep(3,30))
plot(x2S, y2S, type = "n", xlab = "", ylab ="", main = "Sammon (Standardized)")
text(x2S, y2S, oliveOil[c(N, D, S), 1], cex = 1, col = cols2S)

loc3S = isoMDS(dist(scale(acidsamp)), k=2) ## Default is k=2
loc3S
x3S <- loc3S$points[,1]
y3S <- loc3S$points[,2]
cols3S <- c(rep(1,30),rep(2,30),rep(3,30))
plot(x3S, y3S, type = "n", xlab = "", ylab ="", main = "Kruskals non-metric scaling (Standardized)")
text(x3S, y3S, oliveOil[c(N, D, S), 1], cex = 1, col = cols3S)
