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
