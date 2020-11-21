library(MASS)
require(class) ##For knn -> K nearest neighbours

mu = c(0,0) ##Mean vector (0,0)
sig = matrix(c(10, 3, 3, 2), nrow = 2, ncol = 2) ##Sigma, which stands for cov matrix
simA = mvrnorm(900, mu, sig)

muB = c(5,3)
sigB =  matrix(c(12, 2, 2, 15), nrow = 2, ncol = 2)
simB = mvrnorm(900, muB, sigB)

## We will analyse both data sets together, using the rbind function to create the data set simT
simT <- rbind(simA,simB)
## rbind combines its arguments row-wise (i.e., simA on top of simB) so that simT is now a matrix with 1,800
 ## rows and 2 columns.

##Next we will add a column to simT that indicates the class membership of each observation, so that we have
##a record of whether each observation (i.e., each row in the data) in simT came from simA, or from simB.
class_ind <- rep(c(1, 2), each = 900)
simT <- cbind(simT, class_ind)
simT
## The first line of code creates a vector class_ind that repeats the term 1 900 times, and then 2 a further 900
 ## times. The rep function replicates the values in its first argument a set number of times. There are different
 ## ways to specify the function, e.g., rep(c(1, 2), c(900, 900)) or c(rep(1, 900), rep(2, 900).
 ## The second line of code adds the vector class_ind as a third column to the simT dataset. The function
 ## cbind combines the data column-wise (i.e., side to side), in much the same way rbind combines data by rows.

## To visualise the data set simT enter the following:
  plot(simT[,1], simT[,2], col = as.factor(simT[, 3]))
  ## We use the as.factor function to distinguish that the class membership is a categorical variable, and not
  ## numerical, although strictly speaking it isn’t necessary to use this here.
  
  
 ## The function knn takes as its arguments a training data set and a testing data set, which we are now going
 ## to create. We will use a split whereby 1/3 of the data will be used for training, 1/3 of the data will be used
##  for testing, and 1/3 for validation. To create the test and training sub-sets enter the following:
  
  index_train <- c(1:300, 1:300 + 900)
  index_test <- c(1:300 + 300, 1:300 + 900 + 300)
  index_valid <- c(1:300 + 600, 1:300 + 900 + 600)
  
  train <- simT[index_train, 1:2]
  test <- simT[index_test, 1:2]
  valid <- simT[index_valid, 1:2]
  
##  The first line of code creates a vector index_train that consists of the ordered numbers 1, 2, . . . , 300,
##  901, 902, . . . , 1200. We use index_train to select the first third of the data in simTthat come from simA and
##  from simB, with an equal number of points coming from each group. The fourth line creates a matrix train
##  that consists of the un-labelled training data. Similar code divides up the remaining data into the test and
##  valid datasets.
  
##  Now we have the training and the test data, we are able to run knn. We begin with a value of k = 3:
  result <- knn(train, test, cl = simT[index_train, 3], k=3)
  result
## The argument cl=simT[indexindex_train, 3] supplies the function with the true classification for the
## training data set train. These data are used to classify the data points in test. The output in result are
##  the assigned classifications.
  
## Let’s look at the misclassification rate for k = 3. (Note that because we’re analysing simulated data, your
## results may differ slightly from the values shown here. But they should be broadly similar.)
class_agree <- table(result, simT[index_test,3])
class_agree
sum_agree <- sum(diag(class_agree))
sum_agree
(nrow(test) - sum_agree) / nrow(test)
## The inner command table(result, simT[index_test,3]) creates a table comparing the classification
## assigned to the test data by the k-nearest neighbour classifier against the true classification of the data.
## The diag function selects the diagonal elements of the table, which is the number of points where knearest neighbour classification agrees with the true classification. The sum command simply adds these
## values together, and the remainder of the code ensures that the output that is returned is the percentage
## misclassification. 

## To see how the k-nearest neighbour classification performs as a function of k we can write a for loop:
kmax <- 50
k <- 1:kmax
p <- rep(0, kmax)
ntest <- nrow(test)
k_summary <- cbind(k, p)
colnames(k_summary) <- c("k","% misclassified")
for(i in 1:kmax){
  result <- knn(train, test, cl = simT[index_train, 3], k = i)
  class_agree <- table(result, simT[index_test,3])
  sum_agree <- sum(diag(class_agree))
  k_summary[i, 2] <- (ntest - sum_agree) / ntest
}
k_summary[1:10, ]
plot(k_summary[1:10,],  type = "b") 
?knn

?knn.cv
knn.cv(train, valid, k = 3)
dim(train)
dim(valid)

?train
