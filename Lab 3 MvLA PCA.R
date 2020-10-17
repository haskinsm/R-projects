?iris
dim(iris)   ## 150x5
names(iris)
head(iris, n=5)

fisher1 <- prcomp(iris[,1:4]) ##prcomp is the R function for PCA
##Note set it to ignore the final column 5 
##of the dataset as it is a categorical variable and should be excluded from PCA
fisher1

A = data.matrix(iris[,1:4])
A
B = cov(A)
B
C = eigen(B)
C    ## Can be seen here that prcomp(iris[,1:4]) = C which is the eigen decomposition of the covariance
     ## matrix of the data

summary(fisher1) ##Tells you the importance of the components
summary(C) ##Yields nothing useful

round(fisher1$rotation, 2)
round(fisher1$sdev, 2)
##fisher1$rotation and fisher1$sdev respectively call the rotations/PCs
##and standard deviations/square root of eigenvalues from fisher1. 
##The round function rounds its first argument to the number of decimal places 
##given by its second argument.

screeplot(fisher1, type="lines")  ##For seeing the proportion of variance each PC accounts for.
?screeplot

fisher_var_explain <- (fisher1$sdev^2) / (sum(fisher1$sdev^2))
plot(fisher_var_explain, type = "b", main = "Fisher's Iris Data",
     xlab = "No. of components", ylab = "Proportion of variance explained", xaxt = "n")
axis(1, at = 1:4)
##No right answer, but in this case I would say two PC's (so PC1 & PC2).

##Recall that each new PC is just a linear combination of the original data. 
##To see how the values of each observation map on to the PCs, 
##use the predict function:
newiris <- predict(fisher1)
head(newiris, n = 5)
##The predict function is another generic function (like print, summary, and plot) 
##that predicts the results of  a model based on inputted data and a previously
##fitted model object. In this case, predict again recognizes that fisher1 is the 
##output of the prcomp function (i.e., its class is "prcomp") and calculates 
##the value of each observation based on the estimated PC loadings.

newiris[10,] ##I think these are the coefficents by which you multiply the PC linear combinations
##I.e. multiply PC1 vector by its value here of -2.6727558

##The indented code below is just checking the above line, newiris[10,],is right
  sepLMean = mean(A[,1])
  sepWMean = mean(A[,2])
  petLMean = mean(A[,3])
  petWMean = mean(A[,4])
  
  obsLessMeans = c(A[10,1] - sepLMean, A[10,2] - sepWMean, A[10,3] - petLMean, A[10,4] - petWMean)
  data.matrix(obsLessMeans)
  
  ##PCValuesForObs10 = round(fisher1$rotation, 2) %*% data.matrix(obsLessMeans) 
  PCValuesForObs10 = t(data.matrix(obsLessMeans)) %*% round(fisher1$rotation, 2) 
  ##Here t() transposes the matrix. Have to do this so multiplying the right things.
  PCValuesForObs10

##Visualize results
plot(iris[, 1], iris[, 2], col=iris[, 5])
legend(6.5, 4.5, legend = levels(iris[, 5]), col = c(1, 2, 3), pch = 1)

##Gives like graphs comparing the varaibles agaisnt one other one at a time
pairs(iris[,1:4],col=iris[,5])
##V.cool
pairs(newiris,col=iris[,5])
##Where newiris = predict(prcomp(iris[,1:4]))

plot(newiris[,1], newiris[,2], type="n", xlab="PC1", ylab="PC2")
text(newiris[,1], newiris[,2], labels=substr(iris[,5],1,2), col=as.integer(iris[,5]))

eigen(cor(iris[,1:4]))
##Lines either side of this are equal
prcomp(iris[, 1:4], scale=TRUE)  


oliveOil <- read.csv("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/olive.csv")
dim(oliveOil)
oliveOil
oliveOilPCA = prcomp(oliveOil[,3:9])
oliveOilPCA
?prcomp
oliveOilPCAStandardized = prcomp(oliveOil[,3:9], scale =TRUE) ##I think this how to standardize
oliveOilPCAStandardized

##Below lines both get variance of the variables
diag(cov(oliveOil[,3:9]))
c(var(oliveOil[,3]), var(oliveOil[,4]), var(oliveOil[,5]), var(oliveOil[,6]), var(oliveOil[,7]), var(oliveOil[,8]), var(oliveOil[,9]))
