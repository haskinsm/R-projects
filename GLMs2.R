#install.packages("arm")

# Residuals in GLMs
# We still want residuals to be "random" around zero and we don't
# want to see any trends in them because they would suggest that
# there are some effects that are not accounted for in the model, but
# we don't assume that they are normal and/or i.i.d

## Residuals in GLMS
# Pearson residuals: observations with a Pearson residual exceeding
# two in absolute value may be worth a closer look.
?resid
resid(object, type="pearson")

# Deviance residuals: Observations with a deviance residual in excess of
# two may indicate lack of fit.
resid(object) #(default residuals in residuals.glm)



library(arm)
## Binnned residual plot:
# After dividing the data into categories (bins) based on their fitted values, 
# the average residual versus the average fitted vaue for each bin
binnedplot(fitted(bin_model2),
           residuals(bin_model2, type = "response"),
           nclass = NULL,
           xlab = "Expected Values",
           ylab = "Average residual",
           main = "Binned residual plot",
           cex.pts = 0.8,
           col.pts = 1,
           col.int = "gray")
## See interpretaion of this plot in one note GLMs2

binnedplot(d$bmi,
           residuals(bin_model2, type = "response"),
           nclass = NULL,
           xlab = "bmi",
           ylab = "Average residual",
           main = "Binned residual plot",
           cex.pts = 0.8,
           col.pts = 1,
           col.int = "gray")



#install.packages("ISLR")
library(ISLR)
data("Hitters")

# The step() function in base R automates stepwise variable selection using AIC.
display(step(lm(Salary ~ ., data =  train(Salary~., data = train, method = "lm")), trace = F, direction = "forward"))

# OLSRR package has a lot of options
# All possible subsets, best subset, stepwise…
# https://cran.rproject.org/web/packages/olsrr/vignettes/variable_selection.html

# regsubsets() function in the leaps package performs exhaustive search
# of the model space using the leaps algorithm for variable selection.
library(leaps)
plot(regsubsets(Salary ~ ., data = Hitters, method = "exhaustive", nbest = 1)) ## Regsubsets graph
## I'm fairly sure that Salary ~ ., includes all variables as predictors/explanatory variables to predict/model salary

# Caret package
# Useful for comparing models with cross-validation
# By default caret uses 25 bootstrap samples rather than folds.
# Some data points will be left out of each bootstrap sample; caret uses those as the test set for estimating out-of-sample predictive error.
library(caret)
library(dplyr)
set.seed(123)
# train(Salary~., data = train, method = "lm")
#train(Salary~., data = Hitters, method = "lm")

data("Sacramento")
my.dat <- Sacramento
# Add ID variable
my.dat$ID <- seq(1:nrow(my.dat))

# Split data into training and test
ind <- sample(2, nrow(my.dat), replace = TRUE, prob=c(0.7,0.3))
train.dat <- my.dat[ind==1, ]
test.dat <- my.dat[ind==2, ]

# or using dplyr
train.dat <- my.dat %>% sample_frac(.7)
val.dat <- dplyr::anti_join(train.dat, test.dat, by = 'ID')

# or using Caret
ind <- createDataPartition(my.dat$price, p=0.7, list = FALSE)
train.dat <- my.dat[ind, ]
val.dat <- my.dat[-ind, ]

cv <- train(price ~ beds + baths + sqft + type, data = train.dat, method = "lm") ## Model is price
cv #RMSE 79506
preds <- predict(cv, test.dat)
RMSE(preds, test.dat$price) ##Normally expectRMSE to be worse than the RMSE for our training data
# RMSE = 93000





################## Now Lasso and Ridge stuff ##########################


#read in credit data, remove missing values, and add ID variable
dat <- read.csv("credit.csv", na.strings="")
dat <- na.omit(dat)
dat$ID <- seq.int(nrow(dat))
#ensure outcome is a factor
#note that order here is "no" "yes", so a logistic regression model will predict "yes"
dat$Loan_Status <-as.factor(dat$Loan_Status)

library(glmnet)
#set up data in format glmnet can use (must have dummy vars for categorical predictors)
#removed intercept term when making xfactors
xfactors <- model.matrix(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed+Credit_History+Property_Area, data=dat)[, -1] 
## Have -1 here as dont want intercept term, as Glmnet creates its own intercept 
xnonfactors<- subset(dat, select = c("ApplicantIncome", "CoapplicantIncome", "LoanAmount", "Loan_Amount_Term")) ## subset nonfactors/ non categorica, i.e. the cont vars
prepared.dat <- cbind(xnonfactors, xfactors, dat$Loan_Status, dat$ID)
names(prepared.dat)[15:16] <- c("Loan_Status", "ID")

#subset data to create training, valdiation, and test data
library(dplyr)
train.dat <- prepared.dat %>% sample_frac(.6)
rem.dat <- dplyr::anti_join(prepared.dat, train.dat, by = 'ID')
test.dat <- rem.dat %>% sample_frac(.5)
val.dat <- dplyr::anti_join(rem.dat, test.dat, by = 'ID')

#create predictor matrix and outcome vector for training, validation, and test data
train.X <- as.matrix(within(train.dat, rm(Loan_Status, ID)))
val.X <- as.matrix(within(val.dat, rm(Loan_Status, ID)))
test.X <- as.matrix(within(test.dat, rm(Loan_Status, ID)))
train.y <- train.dat$Loan_Status
val.y <- val.dat$Loan_Status
test.y <- test.dat$Loan_Status

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
train_control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 5,
                              search = "random",
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              verboseIter = TRUE) ## Want 5 folds repeated 5 times here

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

#Looking at Coefs for final models
Intercepts <- cbind(ridgemod$a0,lassomod$a0,elasticmod$a0)
Coefs <- cbind(ridgemod$beta,lassomod$beta, elasticmod$beta)
Betas <-rbind(Intercepts, Coefs)
rownames(Betas)[1] = "(Intercept)"
colnames(Betas) = c("Ridge", "Lasso", "Elastic Net")
Betas

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
  prediction <- ifelse(fit.ridge >= cutoffs[i], "Y", "N") #Predicting for cut-off
  accuracy <- c(accuracy,length(which(val.y ==prediction))/length(prediction)*100)
}

plot(cutoffs, accuracy, pch =19,type='l',col= "steelblue",
     main ="Logistic Regression", xlab="Cutoff Level", ylab = "Accuracy %")


#Method for comparing performance metrics of various final models in test data 
#(here using pROC "youden" to choose threshold)

#fit lasso and elastic models
fit.lasso <- predict(lassomod, val.X, type="response")
fit.elastic <- predict(elasticmod, val.X, type="response")

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
class.ridge <- as.factor(ifelse(final.r <= thresh.r, "N", "Y"))
confusionMatrix(class.ridge, test.y, positive = "Y") ## Can see accuracy and sensitivity and other stuff

class.lasso <- as.factor(ifelse(final.l <= thresh.l, "N", "Y"))
confusionMatrix(class.lasso, test.y, positive = "Y")

class.elastic <- as.factor(ifelse(final.e <= thresh.e, "N", "Y"))
confusionMatrix(class.elastic, test.y, positive = "Y")
