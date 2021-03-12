## GLMs -> Generalized Linear Models


## Quite a bit of theory in the notes

## To make predictions you need to choose a threshold for probabilities. I.e. if the prob is less than 0.4 assign to group 0.

## The ROC curve -> Reciver Operator Characteristic
# Curve is used to plot the True Positive Rate vs the 
# False postive rate as you vary the threshold for assigning observations t0 a given class

## Will always end at (1,1). The threshold at this point will be 0. This means we will always classify these observations as falling into class 1

## The area under ROC is called Area Under the Curve (AUC). This gives the rate of successful classification by the logistic model.



# Make some logit model 
#install.packages("catdata") ## Used to get heart dis dataset
library(catdata)
data(heart)
my_heart_dis = data.frame(heart)
?glm
mymodel = glm(y ~ obesity, data=my_heart_dis, family=binomial) ## Binary Data, either belongs in one group or the other
## Note that y is a field in the heart dataset. Here y is the result (or classification) and obesity is my inputs
## Since we're modelling Binomial just going to get probabilitys
## NOTE: If y's ouput is 0 and 1's, R will assume 0 is failure and 1 is success. If y consists of a factor with different levels its
## oing to assume the first factor is the failure and all the others are successes**************************
mymodel


# predict some probabilities from my model 
mypredict = predict(mymodel, type = "response")
mypredict

# install.packages("ROCR")
library(ROCR)
# ROC and Perfromance function
ROCRpred = prediction(mypredict, my_heart_dis$y)
ROCRpred

ROCRperf = performance(ROCRpred, "tpr", "fpr") 
# tpr:True positive rate. P(Yhat = + | Y = +). Estimated as: TP/P.
# fpr:False positive rate. P(Yhat = + | Y = -). Estimated as: FP/N.
?performance
ROCRperf

# Plot ROC curve
plot(ROCRperf)
plot(ROCRperf, colorize = TRUE)
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
## I think the numbers on the line are the diff prob thresholds 


## Performance measures
library(caret)
?confusionMatrix
confusionMatrix(mypredict, my_heart_dis$y, postive = "pos") ## No idea how to do this and she didnt explain. Uterrly Useless. 


