library("flexclust")
oliveOil <- read.csv("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/olive.csv")
dim(oliveOil)

acids = data.matrix(oliveOil[, 3:10]) ##Create a new data matrix called acids that consists only of the final 8 columns of olive
acids

acids_disE <- dist(acids, method="euclidean") ##Using euclidean method of determining disimilarity.
acids_disE_mat <- as.matrix(acids_disE)

acids_disM <- dist(acids, method="maximum") ##Using Maximum method of determining disimilarity.
acids_disM_mat <- as.matrix(acids_disM)

clust1 <- hclust(acids_disE, method = "complete")
plot(clust1) 

clust2 = hclust(acids_disM, method = "complete")
plot(clust2)
?hclust
?dist

##Let’s assume that there are 4 clusters in the data. 
##(Does this seem plausible to you?) We can assign cluster membership using the cutree function:
clust1_label <- cutree(clust1, k=4) ##
clust2_label <- cutree(clust2, k=4)

## To compare the clusterings, we can create a contingency table using the table command.
##Do the results show good agreement, in your opinion?
table(clust1_label, clust2_label)

##To calculate the (unadjusted) Rand index, enter the following:
randIndex(clust1_label, clust2_label, correct = FALSE)

##By default, the randIndex function calculates the adjusted (uncorrected) Rand index:
randIndex(clust1_label, clust2_label)

clust3 = hclust(acids_disE, method = "average")
plot(clust3)
clust3_label = cutree(clust3, k=4) ##a 4 class clustering using Euclidean dissimilarity and average linkage

clust12_label = cutree(clust1, k=3) ## a 3 class clustering solution using Euclidean dissimilarity and complete linkage. 

randIndex(clust3_label, clust12_label, correct = FALSE) ##(unadjusted) Rand index
randIndex(clust3_label, clust12_label) ##adjusted rand index
table(clust3_label, clust12_label)
##Not sure why you would compare the 4 cluster solution to the 3 cluster solution
##Think it might be because that complete linkage is likely to suggest a smaller number of large clusters with roughly equal size
##Could also be to do with both are considered to be the best cut off points for their respective clusters


protein = read.table("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/protein.txt", header = TRUE)
dim(protein)
proteinM = data.matrix(protein[, 2:10])
protein[, 2:10]
prot_dis = dist(proteinM, method="euclidean")
plot(prot_dis)
clustPro = hclust(prot_dis, method = "complete")
plot(clustPro)


sim <- rnorm(10,0,1)
mean(sim)
sd(sim)
pos = rpois(1000, 1)
mean(pos)
sd(pos)
##To simulate from a Multivariate Normal distribution we can first load the package MASS
##(installed by default in R) and then use the function mvrnorm:
library(MASS)
mu <- c(7, 10)
sig <- matrix(c(2, 1, 1, 4), nrow = 2, ncol = 2)
multisim <- mvrnorm(100, mu, sig)
##The first line in the above specifies the mean vector of the distribution, whilst the second line specifies
##the covariance matrix. The final command then asks for a sample of size 100 from a Multivariate Normal
##distribution with this mean and covariance
plot(multisim)
##To produce graphs in a 2 × 2 array, instead of a single frame per plot, enter:
par(mfrow=c(2,2))
##To revert to 1x1 
par(mfrow=c(1,1))
hist(multisim[,1])

hist(multisim[, 2], freq = FALSE, breaks = 10)
curve(dnorm(x, mean=10, sd=2), add=TRUE)
curve( dnorm(x, mean = mean(multisim[,2]) , sd = sd(multisim[,2])), add = TRUE, lty = 2, col = 2)

boxplot(as.data.frame(multisim),notch=TRUE)



##Creating Functions below:
square <- function(x){
  x^2
}
square(3)
square(sig)


for(i in 1:10){
  if(i==1){
    print(paste("The first number is", i))
  }
  if(i>1){
    print( paste("The next number is", i))
  }
}

if(2>0){print("yes")}


toy1 <- -2
if(toy1 > 0){
  print("yes")
} else{
  print("no")
}
