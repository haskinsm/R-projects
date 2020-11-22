library(MASS)
salmon = read.table("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/salmon.txt", header = TRUE)
salmon
plot(salmon[,-1], col = as.factor(salmon[,1]))

library(ellipse)
plot(salmon[,c(2,3)], col = as.factor(salmon[, 1]), xlim=c(50,190), ylim=c(290,530))
lines(ellipse(cov(salmon[c(1:50), c(2, 3)]), centre = colMeans(salmon[c(1:50), c(2, 3)]),level = c(0.5)))
lines(ellipse(cov(salmon[c(51:100), c(2, 3)]), centre = colMeans(salmon[c(51:100), c(2, 3)]), level = 0.5), col = 2)

## Linear Discriminant Analysis (LDA)

##Before we use LDA, we need to split the data into training, and test sets. 
##(Why don’t we need a validation set?) For this example we will try an 80:20 split.

strain <- salmon[c(1:40, 51:90), ]
stest <- salmon[c(41:50, 91:100),]

## We can then train our classifier:

lsol <- lda(strain[, c(2, 3)], grouping = strain[,1])
lsol$prior
lsol$means
## lsol$prior provides the prior probability of group membership that is used in the analysis, 
## which by default is taken to be the class proportions in the training data. The second section
## provides the estimated group means for each of the two groups. You should be able to verify both
## of these calculations.

## Note that the pooled covariance matrix used to perform LDA is not provided. You can find this
## manually by calculating the following:
## Σ= (N1−1)Σ1+(N2−1)Σ2 / N1+N2−2.
## In the above Σ is the estimated common covariance matrix, Σi is the estimated covariance matrix 
## for specific group i, whilst Ni is the number of data points in group i.

## To estimate the covariance matrices for both subsets of salmon data, enter the following:
  
alaska_salmon <- strain[strain == "Alaska", c(2,3)]
canada_salmon <- strain[strain == "Canada", c(2,3)]
n_alaska <- length(alaska_salmon)
n_canada <- length(canada_salmon)
single_cov_num <- ((n_alaska - 1) * cov (alaska_salmon) + (n_canada - 1) * cov(canada_salmon) )
single_cov <- single_cov_num / ( length(strain[, 1]) - 2)
single_cov

## Remember from lectures that the classification rule for LDA is:
## log(P(k|x)/P(l|x))=log(πk/πl)+log(f(x|k)/f(x|l)).

## The multivariate normal assumption with common covariance then leads to the following:
## log(f(x|k)/f(x|l))=xTΣ^−1(μk−μl) −1/2(μTkΣ−1μk−μTlΣ−1μl)
## ⇒log(f(x|k)f(x|l))={x−12(μk+μl)}TΣ−1(μk−μl).  ##Not formatted correctly here

## The term 1/2(μk+μl) gives the average of the group means, and so x−1/2(μk+μl) gives the 
## difference of the observation to this value. Assuming the prior probabilities are equal, 
## (x−1/2(μk+μl))TΣ^−1(μk−μl) determines the classification by whether it is positive or negative.
## (In this case, a positive value indicates membership to Group k).


## As well as providing information about prior probalilities and group means, calling lsol 
## directly provides information regarding the coefficients of linear discriminants 
## (use lsol$scaling to call this directly):
lsol
lsol$scaling

## These are a (scaled) version of Σ−1(μk−μl), and hence can be used for classifying a new observation.
## Note that it is the second class, here Canada, that is associated with group k in the output.
?lda
##The scaling value of the lda object gives the loadings (also called the slopes, coefficients, or weights) 
##of each variable on each discriminant function.

predict(lsol, c(120, 380)) ## Determine the classification for an observation with a Freshwater recording of 120 and a Marine recording of 380

predict(lsol, stest[, c(2, 3)]) ##To automatically predict the test data set enter:
predict(lsol, stest[, c(2, 3)])$class ##Gets how the points were classified using knn
stest[, c(1)] ##Gets the actual classifications
class_agree = table(stest[, c(1)], predict(lsol, stest[, c(2, 3)])$class)
sum_agree <- sum(diag(class_agree))
sum_agree
Perc_misclass = (nrow(stest) - sum_agree) / nrow(stest) ##0% misclassification
Perc_misclass



## Cross-Validation
## Rather than splitting the data into training and test sets (or training, test, 
## and validation sets when different models are being considered), an alternative technique 
## for measuring the performance of the model is to perform cross-validation. 
## For the lda function this is achieved by incorporating the argument CV=TRUE:
  
lsol_cv <- lda(salmon[,c(2,3)], grouping = salmon[, 1], CV = TRUE)
lsol_cv
  
## In order to visualise the performance of LDA under cross-validation we can produce a plot 
## of the following form:
 plot(salmon[, c(2, 3)], col = as.factor(salmon[, 1]), pch = as.numeric(lsol_cv$class))
## The above command plots the two numeric variables of the salmon data with colouring being 
 ## determined by true classification and symbols being determined by the resulting classification 
 ## of `leave-one-out’ LDA. How many misclassified points do you notice?  6 I think
 
 ## Quadratic Discriminant Analysis
 ## The function qda within the package MASS performs quadratic discriminant analysis. 
 ## Remember the difference between QDA and LDA is that the former permits each group 
 ## distribution to have its own covariance matrix, whilst the latter assumes a common 
 ## covariance matrix for all group distributions. The usage of qda the same as lda:
 
 qsol <- qda(strain[, c(2,3)], grouping = strain[, 1])
 qsol
 predict(qsol, stest[, c(2, 3)])
## Again you will notice in an 80:20 training:testing split we have achieved 100% correct 
 ## classification. The output returned from by qsol provides details of the prior probability 
 ## of group membership (again determined by the proportion of data points classified in that 
 ## group by default), and the mean vectors for each group. To find the covariances for the two 
 ## groups enter the following:
 cov (alaska_salmon) 
 cov (canada_salmon)
 
 ## assess the performance of QDA for the salmon data set under cross-validation and produce a plot of your results.
 qsolCV <- qda(salmon[, c(2,3)], grouping = salmon[, 1], CV = TRUE) ##CV = True => Cross Validation
 qsolCV
 salmon[, c(2, 3)]
 class_agree = table(salmon[, c(1)], qsolCV$class) ##Seems like you dont need to use predict function with CV
 sum_agree <- sum(diag(class_agree))
 sum_agree
 Perc_misclass = (nrow(salmon) - sum_agree) / nrow(salmon) ##0% misclassification
 Perc_misclass ## 8% misclass
 plot(salmon[, c(2, 3)], col = as.factor(salmon[, 1]), pch = as.numeric(qsolCV$class))
 

##Compare the performance of LDA and QDA under a 50:25:25 training:validation:testing split of 
 ## the salmon data set. This means you should use 50% of the data to train both models, 25% of 
 ## the data to assess which model appears to be the better classifier, and a further 25% of the
 ## data to more accurately assess the true classification rate of the better model.
 Sal_train = salmon[c(1:25, 51:75), ] ## 25 + 25 = 50
 Sal_test = salmon[c(26:38, 76:87), ] ## Cant split remaining 50 evenly-> test(13Alaskan, 12Canadian) 
 Sal_acc = salmon[c(39:50, 88:100), ] ## acc(12Alaskan, 13Canadian) 
 
 lda_sol = lda(Sal_train[, c(2, 3)], grouping = Sal_train[,1])
 predict(lda_sol, Sal_test[, c(2, 3)])$class
 class_agree = table(Sal_test[, c(1)], predict(lda_sol, Sal_test[, c(2, 3)])$class)
 sum_agree <- sum(diag(class_agree))
 sum_agree
 Perc_misclass = (nrow(Sal_test) - sum_agree) / nrow(Sal_test) ##0% misclassification
 Perc_misclass ##0%

 qda_sol = qda(Sal_train[, c(2,3)], grouping = Sal_train[,1])
 predict(qda_sol, Sal_test[, c(2,3)])$class
 class_agree = table(Sal_test[, c(1)], predict(qda_sol, Sal_test[, c(2, 3)])$class)
 sum_agree <- sum(diag(class_agree))
 sum_agree
 Perc_misclass = (nrow(Sal_test) - sum_agree) / nrow(Sal_test) ##0% misclassification
 Perc_misclass ##4%
 ##Im being lazy here copy and pasting so be careful to run right bits first
 
 ##So lda appears to be better as it misclassifies 0% of the points.
 predict(lda_sol, Sal_acc[, c(2, 3)])$class
 class_agree = table(Sal_acc[, c(1)], predict(lda_sol, Sal_acc[, c(2, 3)])$class)
 sum_agree <- sum(diag(class_agree))
 sum_agree
 Perc_misclass = (nrow(Sal_acc) - sum_agree) / nrow(Sal_acc) ##0% misclassification
 Perc_misclass ##0%
 