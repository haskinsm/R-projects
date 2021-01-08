?faithful
dim(faithful)
library("flexclust") ##For Rand Index

##Wanted us to first perform a heiracrhel clustering but not bothered 

WSS = rep(0,10) # all 10 entries =0 atm
## we will edit the entries within this vector by replacing them with the total within group sum of squares value for
## the results of a k-means clustering when k = 1, . . . , 10.

## This is how to do it for k=2
WSS[2] <- sum(kmeans(faithful, centers = 2)$withinss)
WSS

## Now will use a for loop to values for k 1->10 to assign relevant sum of squares value
for(k in 1:10){
  fit  = kmeans(faithful, k, nstart = 50)
  WSS[k] = fit$tot.withinss
}
##fit$withinss
##fit$tot.withinss
WSS
plot(1:10, WSS, type = "l") ##Better to give title and label axis better
plot(1:10, WSS, type = "l", ylab = "within cluster sum of squares", main ="Faithful data", xlab = "Number of clusters (k)")

# You should have found that both your hierarchical analysis, and the k-means analysis, suggests that there are
# 2 distinct clusters within the data. To find out more about the k-means results for this particular value of k
# we enter the following commands

k <- 2
cl2 <- kmeans(faithful,centers = 2)
table(cl2$cluster)

#The above gives a table of cluster size (which is the same as would have been returned if the last line in the
# above was replaced with cl2$size).
cl2$size

cl2$centers ## info about the clusster centres?? YESSS

cl2$withinss ## Does this indciate how tightly the clusters are grouped?? Or this could measure how well the clusters are fitted? rly not sure R syas: Vector of within-cluster sum of squares, one component per cluster.


##The info gotten from cl2$centers is very useful as it tells you that cluster ones centre (or on average) has an eruptions reading of ~2 and 
# a waiting time of ~55. While cluster two has a higher eruptions reading of ~4 and a waiting time of ~80


plot(faithful, col = cl2$cluster)
points(cl2$centers, col=1:k, pch=8, cex=5)

# In the above command the points function adds points to a currently open plot. It is used here to display
# the centroids of the resulting 2 clusters from the k-means analysis.
# Additional information that will be of interest are the Average Distance from Cluster Centroid' and
# theDistance between Cluster Centroids’. When compared these values indicate the compactness of the
# clusters within the multidimensional data space.

?dist
dist(cl2$centers) ## Distance between centroids


## To get average distnace of cluster 1 points from cluster centroid 1:
g1 <- faithful[which(cl2$cluster==1),] # selects that subset of the data which was clustered into the first group. 
ng1 <- cl2$size[1] #The second command records the number of such data points
total1 <- sum(as.matrix(dist(rbind(g1, cl2$centers[1,])))[ng1+1,]) # adds the cluster centroid as an additional row in the subset of the data which forms the cluster, creates a distance matrix, and sums the elements in the final row
ave1 <- total1/ng1
ave1

## for cluster 2:
g2 <- faithful[which(cl2$cluster==2),]
ng2 <- cl2$size[2]
total2 <- sum(as.matrix(dist(rbind(g2, cl2$centers[2,])))[ng2+1,])
ave2 <- total2/ng2
ave2

## Can conclude that cluster 2 is more tightly grouped that cluster 1 as it has a lower value fro average distance to cluster centoids

hcl <- cutree(hclust(dist(faithful)), 2)
icl <- kmeans(faithful, centers = 2)
tab <- table(hcl, icl$cluster)
tab

randIndex(hcl, icl$cluster, correct = FALSE) ##(unadjusted) Rand index
randIndex(hcl, icl$cluster) ##adjusted rand index



## Olive Oil data set
oliveOil <- read.csv("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/olive.csv")
dim(oliveOil)
oliveOil[ ,3:10]
sumSq = rep(0,10)
sumSq
## Now will use a for loop to values for k 1->10 to assign relevant sum of squares value
for(i in 1:10){
  fit  = kmeans(oliveOil[ ,3:10], i, nstart = 50)
  sumSq[i] = fit$tot.withinss
}
sumSq
plot(1:10, sumSq, type = "l", ylab = "within cluster sum of squares", main ="Olive Oil data", xlab = "Number of clusters (k)")
## K=3 best or k=2 prob is k=3

k <- 3
cl3 <- kmeans(oliveOil[ ,3:10],centers = 3)
table(cl3$cluster)

cl3$size
cl3$centers ## info about the clusster centres?? YESSS
cl3$withinss #Vector of within-cluster sum of squares, one component per cluster.

plot(oliveOil[ ,3:10], col = cl3$cluster)
points(cl3$centers, col=1:k, pch=8, cex=5)
