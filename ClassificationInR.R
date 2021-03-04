## Classifcation in r:

# Decision Trees - packages rpart, party
# Random Forest - packages randomforest
# Naive Bayes Classifiers - packages e1071, mlr
# K-Nearest Neigbors Classifiers - function knn() package class
# Support Vector Machines - package e1071
# Logistic Regression - glm() function, package stats



## Heriarchel Clustering
# does not require the number of clusters to be specified
# Apprach1: Agglomerative -> 'Bottom up approach' starts by assuming each obs in its own cluster and pairs of clusters are merged based on similarity
    # Usually represented by a dendogram structure
# Approach2: Divisive -> Top-down approach: all observations start in one cluster and splits are performed recurisively based on disimilarity
    #


## MEASURING 'DISTANCE' or dissimliarity 
# Continuous vars: Euclidean dist, squared Eucl dist
# Binary vars: Jaccard distance
# Categorical converted to dummy variables: Dice distance (Gower)
# Mixed continuous and categorical: Gower distnace

# How to get distnace?
dist(x) # base r function dist() can do a few, incl Eucl.
( dist(x) )^2 ## Sqaured Eucl

install.packages("ade4")
library(ade4)
dist.binary(x, method = 1, diag = FALSE, upper = FALSE) # Jaccard

library(cluster)
daisy(x, metric = "gower") # Gower dist


## Normalizing Data. (Not the same as standardization)
# puts everything on same scale
# It is standard to normalize cont. data before you calc the distance beacuse of diff units
# Subtracting the min from every value and dividing the differences by the transformed data will lie within hte interval [0,1]

normalize <- function(x){
    return ( (x - min(x)) / (max(x) - min(x)) )
}
mydata$V1_norm <- normalize(mydata$V1)

## Stanadardizing Data
# puts everything on same scale and with equal sds
## the transformed data will not neccesarily lie within hte interval [0,1]
standardize <- function(x){
  return ( (x - mean(x)) / sd(x) )
}
mydata$V1_stand <- standardize(mydata$V1)
## the scale function in r standardizes i think

## Lect prefers normalizing.Although its a judgement call


## How to choose a linkage 
## See notes on OneNote


?hclust
?dist
my_iris = iris
# Complete Linkagae, Eucl dist
clusters1 <- hclust(dist(my_iris[,1:4]))

Cut1 <- cutree(clusters1, 3) ## Cut at whatever height will give 3 groups

table(Cut1, my_iris$Species) 
## Can see Setosa species was very well identified 
## Virginica was also somewhat well identified, all but 1 was put into cluster 2 but cluster 2 also included 23 of versiclor species
## Cluster 3 as nealry all versiclor


## k-Means clustering
## We specify number of clusters to be created
euc <- dist(my_iris[,1:4]) #Euclidean distance. get disimlarity matrix
kmeans1 <- kmeans(euc, 3) ## use kmeans function and specify you want 3 clsuters
table( kmeans1$cluster, my_iris$Species )

## K-Medoids clustering
gow <- daisy(my_iris[,1:4], metric= "gower")# gower distance, use the daisy functon
kmedoids1 <- pam(gow, 3) ## use pam to calculate Kmedoids and specify 3 clusters (Know this from iris data)
table( kmedoids1$clustering, my_iris$Species )


## If dont know how many clusters are present in the data to start use 'Elbow Method':
## Calc the total within cluster sum of squares for diff values of k (num of clusters) and pick the value of k where the bend or knee is

## Or the average silhouette approach 
## Measures the quality of each cluster-> how well each object lies within its cluster. A high average silhouette indicates good clustering
## The optimal values of k is the one that maxs the av silhouette 

## Or the gap statistic
## Cluster the observed data, varying the num of clusters and compute the corresponding within cluster variation .. etc see notes

data <- my_iris[,1:4]
# install.packages("factoextra")
library(factoextra)
## Below function does it all. 2nd arg specifies that doing this for kmeans
?fviz_nbclust
## This function will be finiicky with mixed data
fviz_nbclust(data, kmeans, method = "wss", diss=euc)
fviz_nbclust(data, kmeans, method = "silhouette", diss=euc)
fviz_nbclust(data, kmeans, method = "gap_stat", diss=euc)



## Post-Cluster Analysis 
#Silhouette width plot
sk2 <- silhouette(kmeans1$cl, euc)
plot(sk2) ## Note if plots a graph with no grey bits just try add the arg border=NA inside the plot function

# install.packages("clusterCrit")
library(clusterCrit)

#internal criteria
intldx <- intCriteria(as.matrix(my_iris[,1:4]), kmeans1$cluster, "all")
intldx

# also external criteria for comparing results from 2 cluster methods/clusters from 2 datasets
extldx <- extCriteria(part1,part2,"all") # Where part 1 and 2 are the reuslts of the two diff methods
extldx <- extCriteria(kmeans1$cluster,kmedoids1$cluster,"all")
extldx

# Look at a summary of our data by cluster
require(dplyr)
kmed_results <- iris %>% mutate(cluster = kmedoids1$clustering) %>% group_by(cluster)
kmed_results
kmed_summary <- kmed_results %>% do(the_summary = summary(kmed_results[1:4]) )
kmed_summary$the_summary
