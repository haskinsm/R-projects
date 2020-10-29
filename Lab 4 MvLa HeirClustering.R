oliveOil <- read.csv("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/olive.csv")
dim(oliveOil) ##572 observations of 10 variables,
names(oliveOil)
head(oliveOil) ##First two variables are categorical data, 
## the final 8 variables in the data set consist of the percentage composition of 8 fatty acids in the oil.

acids = data.matrix(oliveOil[, 3:10]) ##Create a new data matrix called acids that consists only of the final 8 columns of olive
acids


## The dist function can be used to create a dissimilarity matrix. Remember to 
## check its help file. This function returns an object of class dist. Although 
## this class is useful when used with some other functions, it is convenient to
## convert this object to a matrix (i.e., object of class matrix) format if we 
## want to check a particular entries.
acids_dis <- dist(acids, method="euclidean") ##Using euclidean method of determining disimilarity.
acids_dis_mat <- as.matrix(acids_dis)

acids_dis_mat[1, 5] ##Checks disimilarity between observations
acids_dis_mat[c(1:5, 331:334), c(1:5, 331:334)]

acids_dis_mat[1, 10] ## I THINK, Comparing the Euclidean dissimilarity between observations 1 and 10.

acids_disM <- dist(acids, method="manhattan") ##Using Manhattan method of determining disimilarity.
acids_disM_mat <- as.matrix(acids_disM)

acids_disM_mat[1, 10] ## I THINK, Comparing the Manhattan dissimilarity between observations 1 and 10.

##Compare the dissimilarity between the first five observations from the Sardinia
## region, (specifically from Inland Sardinia) with the first five observations
## from the North region, (specifically, Umbria). Are oils from the same region 
##more similar? Use any dissimilarity measure you like
A = oliveOil[oliveOil$Area == "Umbria", ]
A[1:5,]
B = oliveOil[oliveOil$Area == "Inland Sardinia", ]
B[1:5,]
C = rbind(A[1:5, 3:9],B[1:5, 3:9])
C
disM = dist(C, method ="manhattan")
disM_mat = as.matrix(disM)
disM_mat
## Can easily see that oils from the same region are more similar, due to lower disimilarity (or distance) values
acids_disM_mat[c(422:426,324:328), c(422:426,324:328)] ##Yields similar reuslt to above code


clust1 <- hclust(acids_dis, method = "average")
plot(clust1) 

clust2 = hclust(acids_disM, method = "single")
plot(clust2)
##Clear to see the linkage method matters more

head(clust1$merge)
head(clust1$height)
##The clust1$merge explains the ordering in which observations were joined 
##into groups. clust1$height describes the dissimilarity (with respect to 
## linkage method) between groups as they were clustered.

meanHeight = mean(clust1$height)
sdHeight = sd(clust1$height)
RecCutOffHeight = meanHeight + 3*sdHeight
RecCutOffHeight ##Correct

##Code below adds the recommended cut off point
plot(clust1)
abline(h = 306.5752, lty=2, col=2)
##The abline command adds a line to an already existing plot. 
##The arguments lty and col specify line type and color of the line respectively.


##Use the cutree function to split the data into a specific cluster structure. This function takes the hclust
##object and either a given cut off height for the dendogram or a
##pre-specified number of clusters as its arguments:
acids_label1 <- cutree(clust1, k=10)
acids_label2 <- cutree(clust1, h=306.5752)

which(acids_label1 == 1)
 ##Find the points assigned to a given cluster
## The argument acidlabel1 == 1 is a logical statement that checks each element
## of the vector and returns whether or not its value equals 1 (i.e., TRUE 
## or FALSE). The which function returns those elements within a
## vector that satisfy the property of its argument.

palette(rainbow(10))
plot(acids[,1], acids[,2], col = acids_label1)
pairs(acids, col = acids_label1)

##Standardize the data
acid_sd <- apply(acids, 2 ,sd)
acid_sd
## The first argument to apply is the matrix to apply the operation over. 
## The second argument specifies that the operation is performed over 
## successive columns (if we wanted the operation performed over successive
##rows we would replace the 2 with a 1). The final argument specifies the 
## operation to be performed.

acid_mean <- apply(acids, 2 ,mean)
acid_mean


#In order to divide each column in acids by its standard deviation we can
## use the function sweep. This function returns an alteration to the matrix 
## acids in which the relevant summary statistic will have been “swept” out.
standard_acids <- sweep(acids, 2, acid_sd, "/")
head(standard_acids)


## Use the sweep function to create a centered version of acids whereby 
## each column has mean 0
##To do this: (observation - meanObs / sdObs) for each obs
normalize_acids = sweep(sweep(acids, 2, acid_mean,"-"), 2, acid_sd, "/") ##Note i have created variables acid_mean above
normalize_acids
acid_scale <- scale(acids, center = TRUE, scale = TRUE)
acid_scale ##Gives same result as above normalize_acids

norm_acids_disM <- dist(normalize_acids, method="manhattan") ##Using Manhattan method of determining disimilarity.
norm_acids_disM_mat <- as.matrix(norm_acids_disM)
clust3 = hclust(norm_acids_disM, method = "average")
plot(clust3)

faithful ##dataset
ffMat = data.matrix(faithful[])
ff_disM = dist(ffMat, method ="manhattan")
ff_disM_mat = as.matrix(ff_disM)
clust4 = hclust(ff_disM, method ="average")
plot(clust4)

ff_scale = scale(ffMat, center =TRUE, scale =TRUE)
f_disM = dist(ff_scale, method = "manhattan")
f_disM_mat = as.matrix(f_disM)
clust5 = hclust(f_disM, method = "average")
plot(clust5) ##Height of the normalized tree is way way less
