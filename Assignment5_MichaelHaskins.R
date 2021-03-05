set.seed(123) ## Told to do this so results are reproducible 

########################################### PART 1
# Clustering Methods refer specifically to Unsupervised Learning methods, it is a machine 
# learning technique whereby we seek to establish/find 
# group structure in the dataset we are examining. Datapoints in the same cluster 
# should be more similar to each other than to datapoints in other clusters.
# We call clustering methods unsupervised methods as we do not know (or ignore) 
# any group structure in the data before we start the analysis

## There are a number of different clustering methods which will be discussed. The distance measure and linkage methods, 
# which can have a big effect on how clustering methods function will also be discussed.

# 1st we will talk about heirarchel clustering
  # This method is especiially useful when we do not know how many clusters (if any) are in the data. This an itertive algorithm and 
  # there are two approaches to performing it. The first being a bottom up, agglomerative approach, where each obs starts in its own cluster
  # and is merged based on similairty (I will later discuss the diff similarity measures that can be used). 
  # The second approach being a divisive or
  # top-down approach: all observations start in one cluster, and are split into smaller clusters recursively based on disimilarity.
  # Heir. clustering is usually displayed as a dendogram. Personally I do not find this the most visually appealing 
  # way of displaying any group structure in the
  # data but it does provide some good isnights into the disimlarity between groups and within groups. 
  # Perhaps the best way of using this method is to establish
  # the number of clusters in the data and then use this to perform another unsupervised learning method.
  # Dendograms are also particulary good at showcasing how the different linkage and dismilarity measures affect clustering. 
  # Linkage methods typically have 
  # more of an impact on tree structure than distance or dissimlarity measure (Not to say distance measure does not have an impact).


## 2nd K-Means clustering
  # K means requires you to specify the number of clusters before it begins. To ensure you pick the number of clusters thaat have the best internal 
  # similarity and external dissimilartiy you can use a while loop to find the best value of k and then plot the total within sum of squares and 
  # [pick the number of clusters where the 'joint is' (This is known as the elbow method.
  # There are other ways  to pick number of clusters such as the silhouette method and Gap statsic method

### 3rd Model Based clustering
  # You must be careful to ensure that the model you assume thee data to follow is accurate or a reasonable approximation.
  # The key differnce between this and the two previuously mentioned methods is it assigns points to groups based on probability.
  # If working with multivariate cont. data use mclust package, if multivar. categorical data use BayesLCA, if mixed data use clustMD


#### Distance Measures
  # There are a number of different distnace or dssimlarity measures. They aim to establish how similar two datapoints are to one another.
  ## Euclidean and qaured Euclidean distnace can be thought of as as the crow flies.
  ## Manahttan distnace which is always longer (or =) than Euclidean distnace can be thought of as following gridlines/roads in Manhattan. 
  ### Maximum distnace selects the maximum distance between the two datapoint. 
  
  # Choosing a distnace measure depends on what type of data you are working with. 
    # Generally speaking if you are working with Cont. vars then use Euclidean or Sqaured Euclidean distnace. 
    # If Binary variables use Jaccard distnace.
    # If Categorical coverted to dummmy vars then Dice distance(Gower)
    # If Mixed continuous and categorical data then use Gower distance. We use the daisy fucntion to calc Gower Dist 
    # and specify Gower as the dist metric 
  
  ## The rand index if i remmber correct is used to measure how well the clustering method fits the data. The unadjusted rand index is usually better in
  # practice as it excludes agreement by chance


##### Linkage Methods 
    # Complete Linkage finds the max possible distnace (With respect to the distnace measure being used) between points belonging to two diff clusters
    # Single linkage finds the min possible distnace between points belonging to two diff clysters
    # Mean Linkage Finds alls possible pairwise distances for points belongign to two diff clusters and then calculates their average
    # There is also Centroid linkage (must use squared Eucl. dist) and Minimax linkage
  
  # Common problems with linkage methods are Chaining. In order to merge two groups (Single linkage is particulary sescipatble to this)
    # we only need one pair of points to be close irrespective of the rest of the group. This can lead to poor internal similarity in a group.
    # meaning the group is not compact enough. For example Complete Linkage gets arounds this as joins clusters at max dissimilarity.
    
    # Complete Linkage causes crowding problems wehreby it may arise that points in one cluster are more similar to points in another cluster.
    
    # Average linkage tries to strike a balance. Aiming to ensure good internal similarity and that clusters are notably different from each other.



#### In conclusion
  # If you want to be able to see the probability that a datapoint belongs to a group and generate interesting graphics showcasing 
  # the CI of a group
  # you should use model based clustering, eg. LDA, QDA etc
  
  # If you decide to use Kmeans clustering you should establish what is the best number of clusters to use and then use that Kmeans result in your analysis
  
  # Heri. Clustering is useful in visulaizing how different/similar datapoints are and visualaizng the internal similarity of clusters.
  
  ## It is also very important that you pick an appropriate distnace measure for the type of data your working with.
  
  ## Generally I would recommend using Avergae Linkage as a linkage method as it ensures good internal similarity and good disiimlairty between groups
  # and avoids the problems of chaining associated with single linkage and crowding associated with complete linkage
  ## But again this will vary based on what you want
  
  ## For most unsupervised learning methods it is good to normalize or standardize the data, this is not always neccesary and is a judgement call.
  # (Although this can be hard to do with mixed data)





#########################################  PART 2
my_iris = iris
?hclust
## Whether the algorithm performs the Heri. clustering using the agglomerative approach or divisive approach makes no difference to the results.
## The divisive method is more computationally efficient tho.
## But since the iris dataset is small with only 150 obs there is no need to change from the default agglomerative approach.
dim(iris)

# Av. Linkagae, Eucl dist
  # I want to use Avergae Linkage to ensure that there is good inteernal similartiy within my groups and dissimlartiy between my groups
  # Av. Linkage also avoids (mitigates) the problems of chaining associated with single linkage and crowding associated with complete linkage
  
  # I used Euclidean distance as the data I am working with is Continuous data. It can be thought of as an 'as the crow flies' measure of 
  # disimilarity between two datapoints. I belive this is a fair method of measuring disimilarity and it is also compatible with Avergae Linkage
clusters1 <- hclust(dist(my_iris[,1:4]), method = "average")
?iris
# I did not see any need to normalize or standardize the data. Although it is normal to standardize or normalize when dealing with
# continuous variables the iris dataset varaibels are continuous but they are all in the same units. It is also important to note that 
# altough Sepal.Length is the biggest varaibel it is not drastically bigger than the others

## Now will visualize the dendogram and then decide at which height to cut (i.e. how many clusters in the data)
plot(clusters1) + abline(h = 1.94, lty=2, col=2) ## This code adds a red line showing the cut off point

## From Looking at the dendogram there appears to be 2 dominat groups. The internal similarity on the group on the right isnt the best.
# We can tell this by the height they are joined at. For this reason I am going to cut at a height which gives me 3 clusters.
# This should ensure good internal similarity within groups. Groups 2 & 3 might have some poiints that are quite similar but the groups should
# hopefully be quite dissimlar in general.

Cut1 <- cutree(clusters1, 3) ## Cut at whatever height will give 3 groups

## Now see the agrreement between the findings and the knwon species classifcation of the iris dataset
table(Cut1, my_iris$Species) 
## The simple table function is perfect for mcomparing classifcation agreement here as we have the same numbeer of groups, so there is no need to use the 
## Rand or adjusted Rand index.

### From this result we can see that the Setosa species is perfectly classified in group 1.
## The Virginica species is all in group 2 but there is also 14 iris of the virginica species in this cluster.
 ## This points to some of the virginica species being quite similar to the versicolor species.
## Group 3 only consists of the virginica species

## In general I would be very happy with this classification and it points to the
## correct linkage and dissimilarity measures being used





############################# PART 3
# mclust is used for model based clustering for multivar. cont. data
library(mclust)

## We can use mclust on the irs data since the data is categorical and it would be logical to assume that it has a normal distribution

model1<- Mclust( my_iris[,1:4] )
summary(model1) # Note log likelihood, number of obs, degrees of freedom, & BIC value (BICs are only good for comparing, magnitude means nothing)

## Now see how well the data was clustered.
table(model1$classification, my_iris$Species)
## It is no surprise that Mclust grouped the versicolor and virginica species together as they were found to be quite similar in the previously 
## performed heri. clustering

plot(model1, what = c("classification"))
## This is shows the classifications and and compares diff variables in the dataset to one another.
## This is very useful in visualizing the differences between the groups.
## For example we can see that group 1 has lower readings for petal length & width than group 2.

plot(model1, what = "BIC")
## Shows the BIC for diff number of clusters for diff models. 
## Mclust will automatically pick the model and number of clusters (Must be more than 1 cluster) combination that best fits the data
## This is measured by BIC. The smallest BIC represetns the best fit for the data


## Post Cluster Analysis
## Whats the quality of your answer?
plot(model1, what = "uncertainty")
## This shows confidence intervals between groups 

plot(model1, what = "density")
## This is a rly cool feature of using Mixture models as apose to other unsupervised methods like Kmeans. You can see/determine the prob
# that each datapoint belongs to a cluster and see its var (the width).
## It is no surpise that cluster two which contains two different species is the biggest for nearly every variable,
## meaning it has the highest variance. The density lines are most spread out for the sepal variables. This is unsurpirizng as 
## the sepal variables have a bigger magnitude than the petal vars. It might be interesting to see how this changes if the data was 
## normalized or standardized

?uncerPlot
uncerPlot(model1$z) ## This works better for other data 
## Not very useful at all on this dataset

summary(model1, parameters=TRUE) ## Gives a much more detailed summary and lists for example the means of group 1 and 2 for diff vars

summary(model1, parameters=TRUE)$mean
## From this we can see that group 1(Setosa species) has a mean reading of 5 and 3.428 for sepal length & width, 1.46 & 0.25 for petal length & width
## Group 2(Versicolor and vertoga) has a mean reading of 6.26 & 2.87 for sepal lenth and width, 4.91 & 1.68 for petal length & width

## Group two has higher readings on average for every variable other than speal width





################################### Part 4
?purrr
#install.packages("purrr")
library(purrr)
library(cluster)
library(dplyr)

?pam
gow <- daisy(my_iris[,1:4], metric= "gower")# gower distance, use the daisy functon
Xmedoids <- pam(gow, 3)  ## 3 clusters
Xmedoids
table(Xmedoids$clustering, my_iris$Species)

## K-means attempts to minimize the total squared error, while k-medoids minimizes the sum of dissimilarities 
# between points labeled to be in 
# a cluster and a point designated as the center of that cluster. In contrast to the k-means algorithm, 
# k-medoids chooses datapoints as centers


## Code Given ***************************(UNALTERED, ALTERED CODE IS BELOW THIS CODE BLOCK)*******************************:
  set.seed(123)
  
  # function to compute total within-cluster sum of square 
  wss <- function(k) {
    kmeans(my_iris[,1:4], k, nstart = 10 )$tot.withinss
  }
  
  # Compute and plot wss for k = 1 to k = 15
  k.values <- 1:15
  typeof(k.values)
  ## Keep getting errors here because dtype is "closure" ## Error is beacuse DF was not being set to iris one
  wss_values <- map_dbl( k.values, wss)
  
  plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
  
  # function to compute average silhouette for k clusters
  avg_sil <- function(k) {
    km.res <- kmeans(my_iris[,1:4], centers = k, nstart = 25)
    ss <- silhouette(km.res$cluster, dist(my_iris[,1:4]))
    mean(ss[, 3])
  }
  
  # Compute and plot wss for k = 2 to k = 15
  k.values <- 2:15
  
  # extract avg silhouette for 2-15 clusters
  avg_sil_values <- map_dbl(k.values, avg_sil)
  
  plot(k.values, avg_sil_values,
       type = "b", pch = 19, frame = FALSE, 
       xlab = "Number of clusters K",
       ylab = "Average Silhouettes")
  
  
  ## Will not be able to calcuate the gap statistic as using Gower Dissimilarity measure as working with mixed data
  ## K medoids uses actual points whose av. dissimilarity to all the objects in the cluster is minimal. So Gower would not work
  # compute gap statistic
  set.seed(123)
  ?clusGap
  gap_stat <- clusGap(my_iris[,1:4], FUN = kmeans, nstart = 25,
                      K.max = 10, B = 50)
  ## It could also be beacuse clusGap only uses a default disimilarity measure and you cant change it
  # Print the result
  print(gap_stat, method = "firstmax")
  

  
 
  
  
   
  ### The below code works and achieves everything that was requested 
  
  
  ## Code Given:
  set.seed(123)
  
  gow <- daisy(my_iris[,1:4], metric= "gower")# gower distance, use the daisy functon
  
  ## fviz_nbclust(my_iris[,1:4], pam, method = "wss", k.max = 15)
  fviz_nbclust(my_iris[,1:4], pam, method = "wss", diss = gow, k.max = 15) 
  ## The above function is used to caclutate total withins of a  kmedoids clustering, the diss = gow where gow is the 
  ## distance matrix of the iris dataset using Gower dissimilarity method
  
  fviz_nbclust(my_iris[,1:4], pam, method = "silhouette", diss = gow, k.max = 15) 
  ## The above function is used to calculate silhouette of a kmedoids clustering
   ?fviz_nbclust
   ?fviz_gap_stat()
  ## Below function calculates the gap statistic using gower as the dsiimilarit measure
  fviz_nbclust(my_iris[,1:4], pam, method = "gap_stat", diss = gow, k.max = 15) 
  
  ## Will not be able to calcuate the gap statistic as using Gower Dissimilarity measure as working with mixed data
  ## K medoids uses actual points whose av. dissimilarity to all the objects in the cluster is minimal. So Gower would not work
  # compute gap statistic
 #  We can calculate the gap statistic for each number of clusters using the clusGap() function from the cluster package along with 
  # a plot of clusters vs. gap statistic using the fviz_gap_stat() function:
  #set.seed(123)
  #?clusGap
  #gap_stat <- clusGap(my_iris[,1:4], FUN = kmeans, nstart = 25,
  #                    K.max = 10, B = 50)
  # Print the result
  # print(gap_stat, method = "firstmax")
  
  
  
  
  
  
  
  
  ## Code Given (ALTERED, Will try do it so it looks similar to the example)*******************************:
  ## Not able to do WSS altough i was able to do it with fviz_nbclust function which was shown above 
  ## Was able to do average silhouette tho
  
  set.seed(123)
  gow <- daisy(my_iris[,1:4], metric= "gower")# gower distance, use the daisy functon
  
  # function to compute total within-cluster sum of square 
  wss <- function(k) {
    kmeans(my_iris[,1:4], k, nstart = 10 )$tot.withinss
  }
  
  # Compute and plot wss for k = 1 to k = 15
  k.values <- 1:15
  typeof(k.values)
  ?as.vector
  
  # extract wss for 2-15 clusters
  ?map_dbl
  #wss_values <- map_dbl( k.values, wss) ## Keep getting errors here because dtype is "closure" ## Error is beacuse DF was not being set to iris one
  wss_values <- map_dbl( k.values, wss)
  
  plot(k.values, wss_values,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
  
  # function to compute average silhouette for k clusters
  avg_sil <- function(k) {
    kmd.res <- pam(gow, k)
    ss <- silhouette(kmd.res$cluster, dist(gow))
    mean(ss[, 3])
  }
  
  # Compute and plot wss for k = 2 to k = 15
  k.values <- 2:15
  
  # extract avg silhouette for 2-15 clusters
  avg_sil_values <- map_dbl(k.values, avg_sil)
  
  plot(k.values, avg_sil_values,
       type = "b", pch = 19, frame = FALSE, 
       xlab = "Number of clusters K",
       ylab = "Average Silhouettes")
  
  
  ## Will not be able to calcuate the gap statistic as using Gower Dissimilarity measure as working with mixed data
  ## K medoids uses actual points whose av. dissimilarity to all the objects in the cluster is minimal. So Gower would not work
  # compute gap statistic
  set.seed(123)
  ?clusGap
  gap_stat <- clusGap(my_iris[,1:4], FUN = kmeans, nstart = 25,
                      K.max = 10, B = 50)
  ## It could also be beacuse clusGap only uses a default disimilarity measure and you cant change it
  # Print the result
  print(gap_stat, method = "firstmax")


