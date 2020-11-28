WSS = rep(0,10)
for(k in 1:10){
  fit  = kmeans(faithful, k, nstart = 50)
  WSS[k] = fit$tot.withinss
}
##fit$withinss
##fit$tot.withinss
WSS
plot(1:10, WSS, type = "l") ##Better to give title and label axis better
plot(1:10, WSS, type = "l", ylab = "within cluster sum of squares", main ="Faithful data", xlab = "Number of clusters (k)")

pc2 = prcomp(faithful)
summary(pc2)
plot(predict(pc2))
pc2 = prcomp(faithful, scale. = TRUE) ##Standardized
summary(pc2)
plot(predict(pc2))
