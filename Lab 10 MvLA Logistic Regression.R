?read.table 
library(MASS)
pulse = read.table("pulse.txt", header = TRUE)

# The glm function performs logistic regression. This function is a much used feature of R for when we are
# interested in fitting what is known as a generalized linear model.

lreg <- glm(RestingPulse ~ Smokes + Weight, data = pulse, family = binomial(logit))
summary(lreg)
# In the above code, the first argument specifies a linear model for predicting RestingPulse from the Smokes
# values and the Weight values. The second argument specifies that the pulse data is to be analysed. The
# final argument specifies that the linear model is applied to the logit of the probability for RestingPulse.

# Now load the MASS package and load the data set birthwt. There is a help file for this data set that can also be called.
# Unfortunately there are a few changes that need to be made to this data set before applying it within a regression.
# For example, we should omit the actual birth weight data and ensure that certain variables are treated as
# categorical (factor), rather than numerical:
birthwt
newbirthdata <- birthwt[,-10]
newbirthdata
is.factor(newbirthdata$race)
newbirthdata$race <- as.factor(newbirthdata$race)
is.factor(newbirthdata$race)
for(i in 1:length(newbirthdata$ptl)){
  if(newbirthdata$ptl[i]>1)
  {
    newbirthdata$ptl[i] <- 1
  }
}
newbirthdata$ptl <- as.factor(newbirthdata$ptl)
for(i in 1:length(newbirthdata$ftv)){
  if(newbirthdata$ftv[i]>2){
    newbirthdata$ftv[i] <- 2
  }
}
newbirthdata$ftv <- as.factor(newbirthdata$ftv)
# This code has ensured all variables that should be treated as categorical (factors) are indeed being treated as
# such. To ensure the results are the same as given in lectures we have also changed the ptl values so that
# they are indicators of whether or not there was a previous premature labour (rather than how many), and
# placed all ftv values that were greater than or equal to 2 into the same factor.

# To perform a logistic regression we can now enter the following:
lreg2 <- glm(low ~ ., data = newbirthdata, family = binomial(logit))
lreg2
summary(lreg2)
# In the above the expression low ~ . is used as short hand to specify a linear model featuring all other
# covariates within the data set.


#Including Interactions
# Returning to the pulse data set, the interaction between smoking and weight can be included within the
# model by entering the following:
lreg3 <- glm(RestingPulse ~ Smokes + Weight + Smokes:Weight,
             data = pulse, family = binomial(logit))
lreg3
summary(lreg3)
# If there are many covariates and all interactions are required, then an easy way to specify the model is to use
# code resembling response~.*. as the first argument in glm.

lreg4 = glm(low ~ lwt + smoke + ht + age + age:smoke, data = newbirthdata, family = binomial(logit))
lreg4
summary(lreg4)



# Prediction & Classification
# To use the output from a logistic regression fit, we can again use its predict function. Try the following:
predict(lreg, pulse[, c(2, 3)])
# You should notice that the results are neither 0 or 1 valued, nor are they even probabilities. This is because
# the results are given on the logit scale. To instead return probabilities we can enter the following:
predict(lreg,pulse[,c(2,3)],type="response")

##Can predict the classification of someone who doesnt smoke and weighs 140lbs
predict(lreg,data.frame(Smokes="No",Weight=140),type="response")
## Can predict this by hand
## Formula P(Low) = exp(alpa + coeffB*smokes + coeffC*weight) / (1 + exp(alpa + coeffB*smokes + coeffC*weight) )
summary(lreg)
predictionValue = summary(lreg)$coefficients[1] + summary(lreg)$coefficients[2]*(0) + summary(lreg)$coefficients[3]*(140)
PLow = exp(predictionValue) / (1 + exp(predictionValue))
PLow ## Same as above predict(lreg,data.frame(Smokes="No",Weight=140),type="response")

# To use the logistic regression fit as a classifier we simply select those data points which have a probability greater than 0.5:
which(as.vector(predict(lreg,pulse[,c(2,3)],type = "response")) > 0.5)

res1 <- round(as.vector(predict(lreg,pulse[,c(2,3)],type="response")))
res1
plot(as.numeric(pulse[,2])+rnorm(length(pulse[,2]),0,0.1),pulse[,3],
     col=pulse[,1],pch=res1,xlab="Jittered Smoking Indicator",ylab="Weight")
class <- c("Correct Low","Correct High","False Low","False High")
legend(x=1.3, y=200, legend=class, col=c(2,1,1,2), pch=c(1,0,1,0))


# Model Comparison
# As far as using the deviance information is concerned, this is best for comparing a nested sequence of models
# to determine if the additional parameters are justified by the data.

# For example, consider the pulse data with no interaction term lreg and the model with an interaction term
# lreg3. The return from the following output suggests that there is no additional benefit of including the
# interaction (a non-significant value is returned).
1-pchisq(lreg$deviance-lreg3$deviance,lreg$df.residual - lreg3$df.residual)

# Create a new logistic model lreg0 that only takes weight as a coefficient and determine if the deviance
# criteria suggests moving to the more advanced model that includes both smoke and weight information:
lreg0 = glm(RestingPulse ~ Weight, data = pulse, family = binomial(logit))
summary(lreg0)
1-pchisq(lreg0$deviance-lreg$deviance,lreg0$df.residual - lreg$df.residual) #suggests moving to the more advanced model that includes both smoke and weight information
