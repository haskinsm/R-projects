######################################## PART 1 #######################

## After placing data file in working directory read in the data
cancerData = read.csv(file = "breast_cancer.dat", sep=",", na.strings = "NA", header = TRUE) 
## The data is comma delimited and has headers

dim(cancerData)
summary(cancerData) ## From this can see there are issues with bare nuclei field

## The first field of the data is a unique ID number. The next 9 fields are mapped
## to a 1-10 scale. The last field 'class_tumor' is like binary data with 2 representing 
## a beingn tumor and 4 for malignant. 

## Now need to clean the data. 
## There are some missing values in the data and want to make the previously
## mentioned changes to the class_tumor (classification) field.

# In my global environement I can see that bare_nuclei is the only field that contains 
# blanks ('?')
levels( cancerData$bare_nuclei )
## Can see the '?'
## Need to remove data with missing info:
?which
#cleanData <- which( cancerData != "?", arr.ind = TRUE)
# cleanData <- data.frame(cleanData)
cleanData <- subset(cancerData, (bare_nuclei != '?'))
## Now make sure bare_nuclei field is an integer
cleanData$bare_nuclei <- as.integer(as.character(cleanData$bare_nuclei))
dim(cleanData)
## Can see now that missing data has been removed 
levels( cleanData$bare_nuclei ) ## No longer able to use levels as now dealing with a numeric field
summary(cleanData) ## Data now looks good

## Now change Classifcation field to 0 and 1 as previously discussed
cleanData$class_tumor[cleanData$class_tumor == 2] <- 'Benign'
cleanData$class_tumor[cleanData$class_tumor == 4] <- 'Malignant'
summary(cleanData$class_tumor) ## Data now looks good, aslo checked in environment

## Data now cleaned:
cleanData
## Will now set the classification field as a factor. Cannot be zero or one. Need to be names 
cleanData$class_tumor <- as.factor(cleanData$class_tumor) 



library(caret)
library(dplyr)
set.seed(123) ## Told to do this so results are reproducible 
# Split data into 60% training, 20% validation and 20% test
ind <- sample(3, nrow(cleanData), replace = TRUE, prob=c(0.6,0.2,0.2))
train.dat <- cleanData[ind==1, ]
val.dat <- cleanData[ind==2, ]
test.dat <- cleanData[ind==3, ]
## Data is now split. Since the data is split based on probability, val.dat and test.dat don not have exactly the same number of observations


?train
# cv <- train(class_tumor ~ clump_thick + unif_cell_size + unif_cell_shape + marg_adhesion + epithelial_size + bare_nuclei + bland_chromatin + normal_nucleoli + mitoses, data = train.dat, method = "lm") ## Model is price
## I set the classification field as a factor as was getting warning that: '
#   You are trying to do regression and your outcome only has two possible values Are you trying to do classification? If so, use a 2 level factor as your outcome column.'
# cv #RMSE 0.1980072
# preds <- predict(cv, test.dat)
# RMSE(preds, test.dat$price) ##Normally expectRMSE to be worse than the RMSE for our training data
# RMSE = 

#create predictor matrix and outcome vector for training, validation, and test data
train.X <- as.matrix(within(train.dat, rm(class_tumor, id_number)))
val.X <- as.matrix(within(val.dat, rm(class_tumor, id_number)))
test.X <- as.matrix(within(test.dat, rm(class_tumor, id_number)))
train.y <- train.dat$class_tumor
val.y <- val.dat$class_tumor
test.y <- test.dat$class_tumor

library(glmnet)
#cross-validate to tune lambda for ridge and lasso
cvridge <- cv.glmnet(train.X, train.y, family="binomial", alpha=0, nlambda=20, type.measure="auc") ## Aplha = 0 for ridge, 1 for lasso
cvlasso <- cv.glmnet(train.X, train.y, family="binomial", alpha=1, nlambda=20, type.measure="auc")

#fit models with final lambda
ridgemod <- glmnet(train.X, train.y, family="binomial", alpha = 0, lambda = cvridge$lambda.1se) ## $lambda.1se => Least complex model thats within one standard error of the min
lassomod <- glmnet(train.X, train.y, family="binomial", alpha = 1, lambda = cvlasso$lambda.1se)

#pre-standardise the predictor matrix for elastic net 
#(glmnet wrapped in caret gets fussy if you try to preprocess within the command)

train.stdX <-scale(train.X)

library(caret)
# Set training control
?trainControl
?train
train_control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5,
                              search = "random",
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              verboseIter = TRUE) ## Want 5 folds repeated 5 times here
## repeats arg: For repeated k-fold cross-validation only: the number of complete sets of folds to compute
# number: Either the number of folds or number of resampling iterations
## number must be greater than 1

## elsatic grid code below has following error: 
# Error: At least one of the class levels is not a valid R variable name; This will cause errors when class probabilities are
# generated because the variables names will be converted to  X0, X1 . Please use factor levels that can be used as valid R variable names  
# (see ?make.names for help).

## Will try to fix this error like so:
#train.stdX  %>% 
  #mutate(churn = factor(churn, 
                      #levels = make.names(levels(churn))))
## That failed. ## https://stackoverflow.com/questions/44084735/classification-usage-of-factor-levels

# Train the model
elastic_grid <- train(train.stdX, train.y,
                      method = "glmnet",
                      tuneLength = 25,
                      trControl = train_control,
                      metric= "ROC",
                      family = "binomial",
                      standardize = FALSE)

#fit the model with best lambda and alpha
elasticmod <- glmnet(train.X, train.y, family="binomial", alpha = elastic_grid$bestTune$alpha, lambda = elastic_grid$bestTune$lambda)





#################################### PART 2 ##########################
#Looking at Coefs for final models
Intercepts <- cbind(ridgemod$a0,lassomod$a0,elasticmod$a0)
Coefs <- cbind(ridgemod$beta,lassomod$beta, elasticmod$beta)
Betas <-rbind(Intercepts, Coefs)
rownames(Betas)[1] = "(Intercept)"
colnames(Betas) = c("Ridge", "Lasso", "Elastic Net")
Betas ## Show the beta coeff fors each model

#Methods for choosing cutoffs in validation data 
#(only ridge model shown here as example. Same for the others)

#Method using visualisation of ROC curves
fit.ridge <- predict(ridgemod, val.X, type="response")

  library(ROCR)
  ROCRfit.ridge = prediction(fit.ridge, val.y)
  ROCRperf.tr.ridge = performance(ROCRfit.ridge, "tpr", "fpr")
  
  plot(ROCRperf.tr.ridge, colorize=TRUE, main="Ridge")
  
  #Method using pROC
  library(pROC)
  ridge.roc <- roc(val.y, as.vector(fit.ridge) )
  coords(ridge.roc, "best", best.method="youden", transpose=TRUE)
  
  #Method using visualisation over Grid
  # accuracy here used as metric, could use something else
  cutoffs <- seq(min(fit.ridge),max(fit.ridge),(max(fit.ridge)-min(fit.ridge))/100)
  accuracy <- NULL
  
  for (i in seq(along = cutoffs)){
    prediction <- ifelse(fit.ridge >= cutoffs[i], "Malignant", "Benign") #Predicting for cut-off
    accuracy <- c(accuracy,length(which(val.y ==prediction))/length(prediction)*100)
  }
  
  plot(cutoffs, accuracy, pch =19,type='l',col= "steelblue",
       main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")


#Method for comparing performance metrics of various final models in test data 
#(here using pROC "youden" to choose threshold)

#fit lasso and elastic models
fit.lasso <- predict(lassomod, val.X, type="response")

  library(ROCR)
  ROCRfit.lasso = prediction(fit.lasso, val.y)
  ROCRperf.tr.lasso = performance(ROCRfit.lasso, "tpr", "fpr")
  
  plot(ROCRperf.tr.lasso, colorize=TRUE, main="Lasso")
  
  #Method using pROC
  library(pROC)
  lasso.roc <- roc(val.y, as.vector(fit.lasso) )
  coords(lasso.roc, "best", best.method="youden", transpose=TRUE)
  
  #Method using visualisation over Grid
  # accuracy here used as metric, could use something else
  cutoffs <- seq(min(fit.lasso),max(fit.lasso),(max(fit.lasso)-min(fit.lasso))/100)
  accuracy <- NULL
  
  for (i in seq(along = cutoffs)){
    prediction <- ifelse(fit.lasso >= cutoffs[i], "Malignant", "Benign") #Predicting for cut-off
    accuracy <- c(accuracy,length(which(val.y ==prediction))/length(prediction)*100)
  }
  
  plot(cutoffs, accuracy, pch =19,type='l',col= "steelblue",
       main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")
  
  
fit.elastic <- predict(elasticmod, val.X, type="response")

  library(ROCR)
  ROCRfit.elastic = prediction(fit.elastic, val.y)
  ROCRperf.tr.elastic = performance(ROCRfit.elastic, "tpr", "fpr")
  
  plot(ROCRperf.tr.elastic, colorize=TRUE, main="Elastic")
  
  #Method using pROC
  library(pROC)
  elastic.roc <- roc(val.y, as.vector(fit.elastic) )
  coords(elastic.roc, "best", best.method="youden", transpose=TRUE)
  
  #Method using visualisation over Grid
  # accuracy here used as metric, could use something else
  cutoffs <- seq(min(fit.elastic),max(fit.elastic),(max(fit.elastic)-min(fit.elastic))/100)
  accuracy <- NULL
  
  for (i in seq(along = cutoffs)){
    prediction <- ifelse(fit.elastic >= cutoffs[i], "Malignant", "Benign") #Predicting for cut-off
    accuracy <- c(accuracy,length(which(val.y ==prediction))/length(prediction)*100)
  }
  
  plot(cutoffs, accuracy, pch =19,type='l',col= "steelblue",
       main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")
# ?rocplot ## Not sure how to do an area under the curve ROC plot

#get thresholds
thresh.r <- coords(roc(val.y, as.vector(fit.ridge)), "best", best.method="youden", transpose=TRUE, ret="threshold")
thresh.l <- coords(roc(val.y, as.vector(fit.lasso)), "best", best.method="youden", transpose=TRUE, ret="threshold")
thresh.e <- coords(roc(val.y, as.vector(fit.elastic)), "best", best.method="youden", transpose=TRUE, ret="threshold")

#predict classifications in test data
final.r <- predict(ridgemod, test.X, type="response")
final.l <- predict(lassomod, test.X, type="response")
final.e <- predict(elasticmod, test.X, type="response")



#use caret to see various measures of performance
#install.packages("e1071")
library(e1071)
class.ridge <- as.factor(ifelse(final.r <= thresh.r, "Benign", "Malignant"))
confusionMatrix(class.ridge, test.y, positive = "Benign") ## Can see accuracy and sensitivity and other stuff

class.lasso <- as.factor(ifelse(final.l <= thresh.l, "Benign", "Malignant"))
confusionMatrix(class.lasso, test.y, positive = "Benign")

class.elastic <- as.factor(ifelse(final.e <= thresh.e, "Benign", "Malignant"))
confusionMatrix(class.elastic, test.y, positive = "Benign")


## Now calculate BIC which is a good for explanation
library(leaps)
plot(regsubsets(class_tumor ~., data = cleanData, method = "exhaustive", nbest = 1))
## Can see from this which variables are best at modelling our data






####### Nothing below this point works :(
# ridgemod$AIC
library(stats4)
# BIC(ridgemod)
# Not possible to get AIC or BIC
# cvridge


## Pearson residuals:
?resid
resid(ridgemod, type="pearson")
## Deviance Residuals
resid(elastic_grid)


library(arm)
## Binnned residual plot:
# After dividing the data into categories (bins) based on their fitted values, 
# the average residual versus the average fitted vaue for each bin
?binnedplot
# ?fitted
binnedplot(ridgemod,
           residuals(class.ridge, type = "response"),
           nclass = NULL,
           xlab = "Expected Values",
           ylab = "Average residual",
           main = "Binned residual plot",
           cex.pts = 0.8,
           col.pts = 1,
           col.int = "gray")

#install.packages("mlbench")
library(mlbench)
library(caret)
# collect resamples
?resamples
results <- resamples(list(Lasso=fit.lasso, Ridge=fit.ridge))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

