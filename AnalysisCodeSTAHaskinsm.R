######## 24/05/21, Michael Haskins, 18323076, 3rd year MSISS

## Read in already cleaned and manipulated dataset
dataset = read.csv(file = "cleanProjectData.csv", sep=",", header = TRUE) 
dim(dataset) 
# Create a data frame of the dataset
df = data.frame(dataset)
dim(df)
# Change all variables to a factor that are not already factors (except failures and absences as they are count variables)
df$Pedu <- factor(df$Pedu)
df$traveltime <- factor(df$traveltime)
df$studytime <- factor(df$studytime)
df$Dalc <- factor(df$Dalc)
df$Walc <- factor(df$Walc)
df$health <- factor(df$health)
#levels(df$Dalc)

# Order the FinalGrade field in the order Low-Grade, Average-Grade, High-Grade
df$FinalGrade <- ordered(df$FinalGrade, levels = c("Low-Grade", "Average-Grade", "High-Grade"))



######################### Code used for exploratory analysis section ######################

###### Basic breakdown of FinalGrade by Pedu (Parent's Education Level)
table(dataset$Pedu, dataset$FinalGrade)


library(ggplot2)

###### Will now create a graph to breakdown the final grade by the level of parent's education
gradeGraph <- ggplot(df, aes(x=Pedu, fill = Pedu)) + labs(title="Breakdown of Final Grade by level of Parent's Education", subtitle = "(649 obs)", y="Total count", x="")
gradeGraph <- gradeGraph + geom_bar()  ## Specify the plot as being a Barchart
gradeGraph <- gradeGraph + facet_wrap("FinalGrade") ## This will create bands of final grade. i.e. a band for Low-Grade, then one for Average-Grade etc split up by the Pedu levels/groups
gradeGraph <- gradeGraph + scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) ## Add colour to the Pedu classifications. Will try keep groups colour constant throughout report
gradeGraph <- gradeGraph + theme(legend.position = "right") ## Place legend to the right 
gradeGraph <- gradeGraph + geom_text(stat='count', aes(label = ..count..), vjust = 0) # Add count labels above each column to esnure easy interpretation 
gradeGraph <- gradeGraph + scale_x_discrete(labels = NULL, breaks = NULL) + scale_y_continuous(labels = NULL, breaks = NULL, limits = c(0,148)) 
# Above line: Removes all axis labels and breaks, and alter the y axis limits to increase visiblity for reader.
plot(gradeGraph) #Plot the created ggplot barchart


###### Will now create a graph to breakdown the final grade by school
schoolGraph <- ggplot(df, aes(x=school, fill = school)) + labs(title="Breakdown of Final Grade by school", subtitle = "(649 obs)", y="Total count", x="")
schoolGraph <- schoolGraph + geom_bar()  ## Specify the plot as being a Barchart
schoolGraph <- schoolGraph + facet_wrap("FinalGrade") ## This will create bands of final grade. i.e. a band for Low-Grade, then one for Average-Grade etc split up by the School 
schoolGraph <- schoolGraph + scale_fill_manual(values = c("darkviolet", "yellow")) ## Add colour to the School classifications. Will try keep groups colour constant throughout report
schoolGraph <- schoolGraph + theme(legend.position = "right") ## Place legend to the right 
schoolGraph <- schoolGraph + geom_text(stat='count', aes(label = ..count..), vjust = 0) # Add count labels above each column to esnure easy interpretation 
schoolGraph <- schoolGraph + scale_x_discrete(labels = NULL, breaks = NULL) + scale_y_continuous(labels = NULL, breaks = NULL, limits = c(0,250)) 
# Above line: Removes all axis labels and breaks, and alter the y axis limits to increase visiblity for reader.
plot(schoolGraph) #Plot the created ggplot barchart


##### Breakdown of Pedu by school
table(df$school, df$Pedu)



################### Correlation heatmap 

set.seed(123) # To ensure reproducability
# Src #https://stackoverflow.com/questions/52554336/plot-the-equivalent-of-correlation-matrix-for-factors-categorical-data-and-mi
# This code was used in my eploratory analysis

library(tidyverse)
# install.packages("lsr")
library(lsr)
library(dplyr)

#### Make sure MASS project is not loaded (As select function overlaps with desired dplyr select) #############################################################
detach("package:MASS", unload=TRUE) ## To ensure the select function of MASS does not override select function of dplyr

# function to get chi square p value and Cramers V
f = function(x,y) {
  tbl = df %>% select(x,y) %>% table()
  chisqPval = round(chisq.test(tbl)$p.value, 2)
  cramV = round(cramersV(tbl), 2) ## Round to two decimal places
  data.frame(x, y, chisqPval, cramV) }

# create unique combinations of column names
# sorting will help getting a better plot (upper triangular)
dfComb = data.frame(t(combn(sort(names(df)), 2)), stringsAsFactors = F) 

# apply function to each variable combination
dfRes = map2_df(dfComb$X1, dfComb$X2, f)

# plot results
dfRes %>%
  ggplot(aes(x,y,fill=chisqPval))+
  geom_tile()+
  geom_text(aes(x,y,label=cramV))+
  scale_fill_gradient(low="red", high="yellow")+
  theme_classic() + 
  scale_x_discrete(guide = guide_axis(angle = -45)) +## Rotate x labels 45 degrees so theyre more legible 
  labs(title="Correlation Heatmap of School Performance dataset", subtitle = "Calculated using Cramers V (indicates how strongly categorical variables are associated)", 
       y="Variables", x="Variables", caption = "Cramers V ranges from 0 (low corr) to 1 (high corr)")

######################################### End of Exploratory Analysis ##############################




######################################### Prediction Models ########################################

########################################## Ordinal Logistic Regression ##############

########## Need to change ordinal variable FinalGrade from 'Low-Grade', 'Average-Grade' and 'High-Grade' to 1, 2, 3 (Still an ordinal variable)
dfR = df # Create new df for this 
levels(dfR$FinalGrade)[levels(dfR$FinalGrade) == 'Low-Grade'] <- '1'
levels(dfR$FinalGrade)[levels(dfR$FinalGrade) == 'Average-Grade'] <- '2'
levels(dfR$FinalGrade)[levels(dfR$FinalGrade) == 'High-Grade'] <- '3'


# src: https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/

#install.packages("MASS")
library(MASS) #Reload MASS package

######## Randomly split data into 70% training, 30% test
set.seed(123) ## Set seed to ensure reproducability.
ind <- sample(2, nrow(dfR), replace = TRUE, prob=c(0.7, 0.3))
trainDf <- dfR[ind==1, ]
testDf <- dfR[ind==2, ]
## Data is now split. Since the data is split based on probability, may not have exactly a 70/30 split.


# Create Ordinal Logistic Regression model 
m <- polr(FinalGrade ~ . , data= trainDf, Hess = TRUE)
# create a model using all variables as explanatory variables and use the training dataset to train the model
# specify Hess=TRUE to have the model return the observed information matrix from 
# optimization (called the Hessian) which is used to get standard errors and is required for the summary function.

# Now get summary of the model
summary(m)
# Can see the estimates for the two intercepts, which are sometimes called cutpoints. 
# The intercepts indicate where the latent variable is cut to make the three groups that we observe in our data.
# Note that this latent variable is continuous. In general, these are not used in the interpretation of the results. 
# The cutpoints are closely related to thresholds, which are reported by other statistical packages
# Can see the AIC = 791.671


##### Get P-Values
# Coeff table
coeffTable <- coef(summary(m))
# calculate and store p-values
p <- pnorm(abs(coeffTable[, "t value"]), lower.tail = FALSE) * 2
## combined table
coeffTable <- cbind(coeffTable, "p value" = p)
coeffTable ## Shows coeff and p-values


##### Get CI's 
ci <- confint(m) # default method gives profiled CIs -> obtained by profiling the likelihood function
ci 
# If the 95% CI does not cross 0, the parameter estimate is statistically significant.
# The estimates in the output are given in units of ordered logits, or ordered log odds
## Better to use above ci than below (commented out) line as cannot assume normailty for all vars. 
# confint.default(m) ## Not preferred: CIs assuming normality -> obtained by using the standard errors and assuming a normal distribution.

## For ease of interpretation will convert the coefficinets into odds ratios 
# To get the OR and confidence intervals, exponentiate the estimates and confidence intervals.
###### odds ratios
exp(coef(m))
## OR and CI
exp(cbind(OR = coef(m), ci))
# These coefficients are called proportional odds ratios and we would interpret these pretty much as we would odds ratios from a binary logistic regression.


######## Now test the porportional odds assumption of an ordinal logistic regression
#src: https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
#install.packages("Hmisc")
library(Hmisc)
# I will use a graph to test the porportional odds assumption 
# Create function to estimate the values to be graphed
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}
(s <- with(trainDf, summary(as.numeric(FinalGrade) ~ sex + school + Pstatus + 
                              traveltime + studytime + failures + schoolsup + famsup + 
                              higher + internet + Dalc + Walc + health + absences + Pedu,
                            fun=sf)))
# The table above displays the (linear) predicted values we would get if we regressed our dependent
# variable on our predictor variables one at a time, without the parallel slopes assumption.

# We can evaluate the parallel slopes assumption by running a series of binary logistic regressions with 
# varying cutpoints on the dependent variable and checking the equality of coefficients across cutpoints. 
# We thus relax the parallel slopes assumption to checks its tenability. To accomplish this, we transform the original, ordinal, 
# dependent variable into a new, binary, dependent variable which is equal to zero if the original, ordinal dependent variable (here FinalGrade) 
# is less than some value a, and 1 if the ordinal variable is greater than or equal to a.

# This is done for k-1 levels of the ordinal variable and is executed by the as.numeric(apply) >= a coding below. The first line of code estimates the 
# effect of Pedu on achieving “Low-Grade”(1) versus “Average-Grade”(2) or “High-Grade”(3). 
# The second line of code estimates the effect of pared on achieving“Low-Grade”(1) or “Average-Grade”(2) versus “High-Grade”(3).
glm(I(as.numeric(FinalGrade) >= 2) ~ Pedu, family="binomial", data = trainDf)
glm(I(as.numeric(FinalGrade) >= 3) ~ Pedu, family="binomial", data = trainDf)
# We can use the values in this table to help us assess whether the proportional odds assumption is reasonable for our model
# For example, when Pedu is equal to 1 the difference between the predicted value for FinalGrade greater than or equal to two and 
# FinalGrade greater than or equal to three is roughly 2.3 ( 0.3847 – (-1.9010) = 2.29 ). 
# For Pedu equal to “2” the difference in predicted values for FinalGrade greater than or equal to two and FinalGrade greater than or equal to 
# three is also roughly 1.7 ((0.3847 + 0.3231) - (-1.9 + 0.8661) = 1.7427). 
# This suggests that the parallel slopes assumption may not hold for Pedu, indicating that the effect of Pedu (Parents' education level) 
# is different for the transition from "Low-Grade" to "Average-Grade" and "Average-Grade" to "High-Grade"

## Do this for each parameter:
s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
s ## If the assumption is to stand for an explanatory variable the readings in 'Y>=3' column must be equal for its section

# In the below graph If the proportional odds assumption holds, for each predictor variable, distance between the symbols for
# each set of categories of the dependent variable, should remain similar.
plot(s, which=1:3, pch=1:3, main="", cex.lab=0.7, xlab='logit', xlim=c(-3,0), ann = FALSE) # yaxt="n" -> No y axis labels
title(main='Verification of the parallel slopes assumption (of Ordinal Regr.)', 
      sub='The assumption holds if for each predictor \n variable the distance between the symbols \n for each set of categories is similar.',
      cex.sub = 0.75, font.sub = 4, adj=1) #adj=1 stes the subtitle to be algned as far right as possible

# which =1:3 as decision var has 3 levels
# Set x range as being between -3 to 0 to ensure graph was close up
# I think I will have to remove category labels of individual variables to increase readability
# Will add subtitle to ensure graph is standalone 
# Cant make y axis labels appear any nicer unfortunately. 


##################### Now obtain the predicted probabilities:
############################################### Abandoned Block of code ############################
### Create new data frame contain all possible combinations
#newdat <- data.frame(
  #school = rep(1:2, 1600), # 1="GP", 2="MS"
  #sex = rep(1:2, 1600), #1='M;, 2='F'
  #Pstatus = rep(1:2, 1600), # "A"=1, "T"=2
  #traveltime = rep(1:5, 640),
  #studytime = rep(1:5, 640),
  #failures = rep(1:3, length.out = 3200),
  #schoolsup = rep(1:2, 1600), #1=Yes, 2=No
  #famsup = rep(1:2, 1600),#1=Yes, 2=No
  #higher = rep(1:2, 1600),#1=Yes, 2=No
  #internet = rep(1:2, 1600),#1=Yes, 2=No
  #Dalc = rep(1:5, 640),
  #Walc = rep(1:5, 640),
  #health = rep(1:5, 640),
  #absences = rep(1:32, 100), #3200 rows
  #Pedu= rep(1:3, length.out = 3200)
#)
#newdat <- cbind(newdat, predict(m, newdat, type = "probs"))


#install.packages("reshape2")
#library(reshape2)

#lnewdat <- melt(newdat, id.vars = c("school", "sex", "Pstatus", "traveltime", "studytime", "failures", "schoolsup", 
 #                                   "famsup", "higher", "internet", "Dalc", "Walc", "health", "absences", "Pedu" ),
  #              variable.name = "Level", value.name="Probability")

#ggplot(lnewdat, aes(x = internet, y = Probability, colour = Level)) +
 # geom_line() + facet_grid(Pedu ~ higher, labeller="label_both")

######################################### End of abadnoned block #################################


############## Predict the (FinalGrade) classification of the test dataset
predictTest = predict(m, testDf)

#### Compute confusion matrix
confMatrix = table(testDf$FinalGrade, predictTest)
## Add row and col names
confMatrix <- rbind(as.numeric(names(confMatrix)), confMatrix)
rownames(confMatrix) <- c("Real (rows): Low-Grade", "             Avergae-Grade", "             High-Grade")
colnames(confMatrix) <- c("Predicted (Cols): Low-Grade", "Avergae-Grade", "High-Grade")
confMatrix

#### Calc Misclassification %
correctClassifications = confMatrix[1] + confMatrix[5] + confMatrix[9]
total = dim(testDf)[1]
misClassificationPerc = round(((total - correctClassifications)/ total)*100, 2) 
misClassificationPerc ##  39.29% -> Very high. 

###### Abandoned 
### Misclassification % for each level
#numLowGrades = length(testDf$FinalGrade[testDf$FinalGrade == "1"])
#correctLowGrades = 
###### End abandoned 

############ Plotting the probability an observation belongs to different bands of FinalGrade based on levels of Pedu and higher
#install.packages("effects")
library("effects")
Effect(focal.predictors = "Pedu",m)
plot(Effect(focal.predictors = "higher",m))
plot(Effect(focal.predictors = "Pedu",m))
plot(Effect(focal.predictors = c("Pedu", "higher"),m), main = "Effect of the levels of Pedu and higher on FinalGrade outcome")
# Can draw a number of interesting insights . 


############################## VUS: Volume Under the ROC Surface (Suitable for Ordinal Logistic regression models)
#install.packages("VUROCS")
library("VUROCS")
?VUS
# This function computes the volume under the ROC surface (VUS) for a vector of 
# realisations y (i.e. realised categories) and a vector of predictions fx (i.e. values of the a ranking function f) 
volumeUnderVUS = VUS(testDf$FinalGrade, predictTest)$val
volumeUnderVUS # is 0.1102031. This is very low nd indicates the model is not very good at predicting the finalGrade of students.

## There is no point in plotting this as it would be a 3D ROC curve, which is very hard to interpret for readers. 
## I also have not been able to find a package for plotting a 3D ROC curve that I can get to work.



## Some prediction probabilities
myPredict = predict(m, type = "probs", newdata = testDf)
myPredict
# If I have time I might write a double for loop to ascertain the best thresholds, but not sure if this will be useful or worthwhile 





############################################### Try Model with only variables that were deemed to be statsitically signifcant from CIs##########################
ci 
# If the 95% CI does not cross 0, the parameter estimate is statistically significant.
## All of the below variables are deemed to be statistically signifcant, except Pedu, but as it was very close to be 
# statistically signifcant I included it. 
m2 <- polr(FinalGrade ~ school + higher + absences + Pedu + failures , data= trainDf, Hess = TRUE)
m2
summary(m2) ## AIC is 798, which is higher than model 'm', which has an AIC of 791.671 so this model is worse at modelling the original data
#summary(m)
############## Predict the (FinalGrade) classification of the test dataset using model 2
predictTestm2 = predict(m2, testDf)

#### Compute confusion matrix
confMatrixm2 = table(testDf$FinalGrade, predictTestm2)
confMatrixm2

#### Calc Misclassification %
correctClassificationsm2 = confMatrixm2[1] + confMatrixm2[5] + confMatrixm2[9]
total = dim(testDf)[1]
misClassificationPercm2 = round(((total - correctClassificationsm2)/ total)*100, 2) 
misClassificationPercm2 ##  43.37% -> Very high.Higher than model m, so this model is worse at predicting the outcome of FinalGrade  



















