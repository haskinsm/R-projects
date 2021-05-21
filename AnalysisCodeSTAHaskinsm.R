## Read in already cleaned and manipulated dataset
dataset = read.csv(file = "cleanProjectData.csv", sep=",", header = TRUE) 
dim(dataset) 
# Create a data frame of the dataset
df = data.frame(dataset)
dim(df)
# Change all variables to a factor that are not already factors (except failures and absences)
df$Pedu <- factor(df$Pedu)
df$traveltime <- factor(df$traveltime)
df$studytime <- factor(df$studytime)
df$Dalc <- factor(df$Dalc)
df$Walc <- factor(df$Walc)
df$health <- factor(df$health)
#levels(df$Dalc)

# Order the FinalGrade field in the order Low-Grade, Average-Grade, High-Grade
df$FinalGrade <- ordered(df$FinalGrade, levels = c("Low-Grade", "Average-Grade", "High-Grade"))

table(dataset$Pedu, dataset$FinalGrade)


library(ggplot2)

## Will now createa graph to breakdown the final grade by the level of parent's education
gradeGraph <- ggplot(df, aes(x=Pedu, fill = Pedu)) + labs(title="Breakdown of Final Grade by level of Parent's Education", subtitle = "(649 obs)", y="Total count", x="")
gradeGraph <- gradeGraph + geom_bar()  ## Specify the plot as being a Barchart
gradeGraph <- gradeGraph + facet_wrap("FinalGrade") ## This will create bands of final grade. i.e. a band for Low-Grade, then one for Average-Grade etc split up by the Pedu variable
gradeGraph <- gradeGraph + scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) ## Add colour to the Pedu classifications 
gradeGraph <- gradeGraph + theme(legend.position = "right") ## Place legend to the right 
gradeGraph <- gradeGraph + geom_text(stat='count', aes(label = ..count..), vjust = 0) # Add count labels above each column
gradeGraph <- gradeGraph + scale_x_discrete(labels = NULL, breaks = NULL) + scale_y_continuous(labels = NULL, breaks = NULL, limits = c(0,148)) 
# Above line: Removes all axis labels and breaks, and alter the y axis limits to increase visiblity.
plot(gradeGraph) #Plot the created ggplot barchart


###################################### Correlation heatmap ##################################################
set.seed(123)
# Src #https://stackoverflow.com/questions/52554336/plot-the-equivalent-of-correlation-matrix-for-factors-categorical-data-and-mi
# This code was used in my eploratory analysis

library(tidyverse)
# install.packages("lsr")
library(lsr)
library(dplyr)
#### Make sure MASS project is not loaded (As select function overlaps with desired dplyr select) #############################################################

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


########################################## Ordinal Logistic Regression ##############################
set.seed(123) ## Set seed to ensure reproducability. 
########## Need to change ordinal variable FinalGrade from 'Low-Grade', 'Average-Grade' and 'High-Grade' to 1, 2, 3
dfR = df#data.frame(dataset)
levels(dfR$FinalGrade)[levels(dfR$FinalGrade) == 'Low-Grade'] <- '1'
levels(dfR$FinalGrade)[levels(dfR$FinalGrade) == 'Average-Grade'] <- '2'
levels(dfR$FinalGrade)[levels(dfR$FinalGrade) == 'High-Grade'] <- '3'

# src: https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
#install.packages("MASS")
library(MASS)

# Randomly split data into 70% training, 30% test
ind <- sample(2, nrow(dfR), replace = TRUE, prob=c(0.7, 0.3))
trainDf <- dfR[ind==1, ]
testDf <- dfR[ind==2, ]
## Data is now split. Since the data is split based on probability, may not have exactly a 70/30 split


# Create Ordinal Logistic Regression model 
m <- polr(FinalGrade ~ . , data= trainDf, Hess = TRUE)
# specify Hess=TRUE to have the model return the observed information matrix from 
# optimization (called the Hessian) which is used to get standard errors.

# Now get summary of the model
summary(m)
# Can see the estimates for the two intercepts, which are sometimes called cutpoints. 
# The intercepts indicate where the latent variable is cut to make the three groups that we observe in our data.
# Note that this latent variable is continuous. In general, these are not used in the interpretation of the results. 
# The cutpoints are closely related to thresholds, which are reported by other statistical packages


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
## Better to use above ci than below line as cannot assume normailty for all vars. 
# confint.default(m) ## Not preferred: CIs assuming normality -> obtained by using the standard errors and assuming a normal distribution.

## For ease of interpretation will convert the coefficinets into odds ratios 
# To get the OR and confidence intervals, we just exponentiate the estimates and confidence intervals.
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
plot(s, which=1:3, pch=1:3, main="", xlab='logit', xlim=c(-3,0), ann = FALSE) #xlim=range(s[,3:4]))
title(main='Verification of the parallel slopes assumption (of Ordinal Regr.)', 
      sub='The assumption holds if for each predictor \n variable the distance between the symbols \n for each set of categories is similar.',
      cex.sub = 0.75, font.sub = 4, adj=1) #adj=1 stes the subtitle to be algned as far right as possible
# which =1:3 as decision var has 3 levels
# Set x range as being between -3 to 0 to ensure graph was close up
# I think I will have to remove category labels of individual variables to increase readability
# Will add subtitle to ensure graph is standalone 


##################### First make predictions:
# predict some probabilities from my model 
myPredict = predict(m, type = "response")
myPredict

# install.packages("ROCR")
library(ROCR)





