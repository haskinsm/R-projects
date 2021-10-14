
set.seed(123)
getwd() ##Check directory

########## Read in Data ##############
Data <- read.csv("Data Analytics 4thYr/Data/DT-Credit.csv", header=TRUE, sep= ";") ##Saved in a folder called 'Data'in this files directory


######### Check and manipulate dataset########
str(Data) # str() is used for compactly displaying the internal structure of a R object
Data <- Data[,-1] # Remove 1st column of data
str(Data)

################## Now fix Data, specifying what is factors 
str(Data)
cols <- c(1, 3:9, 11:21, 23:31)
Data[cols] <- lapply(Data[cols],factor) ##lapply applys function to everything in cols -> here it turns everything into factors 
attach(Data)
str(Data)
# Change the response to Yes/No answers. 
Target=ifelse(RESPONSE==1,'Yes','No')
Data <- data.frame(Data,Target)
str(Data)
Data=Data[,-31] #Remove Column 31 which was the old binary responses
attach(Data)
################################## I'm not a big fan of attach. It only stores a copy of the data so any changes are not refelcted 
################################## Be very careful about what variables are masked 
################################## Call detash(<data>) when altered the dataset data 


#######   Install necessary packages
#install.packages("rpart")
library("rpart")
#install.packages ("partykit") 
library("partykit") 

DT_Model_1<-rpart(Target~., data=Data, control=rpart.control(minsplit=60, minbucket=30, maxdepth=4 ))
# 'Target' is our response variable in this dataset. ~. signifies that everything else shuld be used as an independent variable a predictor

# minsplit: the minimum number of observations that must exist in a node for a new split 
# minbucket: the minimum number of observations in any terminal node 
# Maxdepth: Maximum depth for any node, with the root node counted as depth 0. 

plot(as.party(DT_Model_1))
print(DT_Model_1)
####################################### If asked to explain the outcome of the DTModel this is exactly what you need to write in an exam #####################
###########################################################################################################################################
#Explanation of table:
# 1) root 1000 300 Yes (0.3000000 0.7000000)  
# 2) CHK_ACCT=0,1 543 240 Yes (0.4419890 0.5580110)  
# 8) SAV_ACCT=0,1,2 196 74 No (0.6224490 0.3775510) *
# Node 1) shows in whole dataset there are 1000 records.
# In absence of any predictor the optimal decision is
# "1 or Yes". This decision gives us 70% accuracy, and for
# 300 records, which their actual value for the "Target"
# variable are "0 or No" are misclassified.
# Node 2) shows 543 records have "CHK_ACCT=0,1", that for
# 55.8% of them the "Target" variable is "1 or Yes".     
# As 55.8% is the majority, the decision is "1 or Yes".
# This decision results 240 misclassified records,
# which their actual value for the "Target"variable
# are "0 or No". 
# Node 8) with "SAV_ACCT=0,1,2", out of 196 records, for
# 62.2% of them the "Target" variable equals "0 or No".
# Based on the majority rule, there decision is therefore
# "0 or No" that results 74 or 37.7% misclassification.
# The asterisk indicates the terminal node. 


# We now relax the control parameters to get the largest table.
# We are going to "prune" this tree later.
DT_Model_2<-rpart(Target~., data=Data)
plot(as.party(DT_Model_2)) #This a very overfitted model as no control parameters -> very complex -> too many splits and terminal nodes 
print(DT_Model_2)

# The following line gives us the fitted tree's 
# Complexity Parameter (CP) table.
# Look where you see the least error. 
print(DT_Model_2$cptable) 

# In this table:
# CP stands for the Complexity Parameter. See how it is
# calculated in the note in Blackboard.
# rel error, is the relative error. This is similar to the 
# 1-(R-square) in regression analysis.  **********
# xerror is the sample mean of the model error using 10-fold
# cross validation (the default in rpart).
# Finally, xstd is the SD of the xerror.

####################################################################################################################################
#************* An increasing r^2 value indicates that your predictors are starting to predict something about the dependent variable
# R^2 talks about the level (or %) of variation in the dependetn variable that is explained by the predictors (the independent variables). 
# Variation of the dependent variable is constant. 
####################################################################################################################################


#################################### Improve the tree with regard to complexity ###########################################
# Visually can see fromabove output that the least xerror occurs in the 3rd line and this is associated with the complexity variable of 0.018
# The line below picks up the least error tree 

opt <- which.min(DT_Model_2$cptable [, "xerror"]) 
opt
# Pruning the tree to the least xerror
CP <- DT_Model_2$cptable [opt,"CP"]
CP
DT_Model_pruned <- prune(DT_Model_2, cp=CP) # Prune the tree to the level that we got for min cp above which was 0.01833...
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)
################# This model is not as overfitted or complexand gives a better prediction


################################### Performance of the model ###########################################################
# Now let's analyse the performance of the model.
Pred_DT_Model_pruned <- predict(DT_Model_pruned, data = Data ,type = "prob")
Pred_DT_Model_pruned
# Tell us the prob that each datapoint belongs in each target class

# assuming the cut-off probability 50%
Pred_DT_Model_pruned_YN <- ifelse(Pred_DT_Model_pruned[,2] > 0.50, "Yes", "No")
Pred_DT_Model_pruned_YN

# saving predicted vector as factor 
Pred <- as.factor(Pred_DT_Model_pruned_YN)

# ordering the vectors
Predicted <- ordered(Pred, levels = c("Yes", "No"))
Actual <- ordered(Data$Target,levels = c("Yes", "No"))
table(Predicted,Actual)
# making confusion matrix
#install.packages("caret")
#install.packages("lattice")
#install.packages("ggplot2")
#install.packages("e1071")
library("lattice")
library("ggplot2")
library("caret")
library("e1071")

CM <- confusionMatrix(table(Predicted,Actual))
CM
##################################### ******************** May be asked any of these in Exam #################################
########################## Be very very careful that the actual is in the columns and the predicted is in the rows
########################## when using these frmulas. Check this by adding up the actual number of yesses and seeing wether actual is in cols or rows
#	CM description
# For the Table below:
#             Actual ---In cOlumns    --- Predicted  in rows
# Predicted	 Yes	No
# Yes 	      A 	B
# No      	  C	  D
# The formulas used are:
# Accuracy = (A+D)/(A+B+C+D)
# random accuracy = ((B+D)(C+D)+(A+C)(A+B))/(A+B+C+D)^2
# kappa = (Accuracy - random accuracy)/(1-random accuracy)
# Sensitivity = A/(A+C)
# Specificity = D/(B+D)
# Prevalence = (A+C)/(A+B+C+D)
# Detection Rate = A/(A+B+C+D)
# Detection Prevalence = (A+B)/(A+B+C+D)
# Balanced Accuracy = (sensitivity+specificity)/2
# Precision = A/(A+B)
# Recall = A/(A+C)
# PPV = (sensitivity * prevalence)/((sensitivity*prevalence) + ((1-specificity)*(1-prevalence)))
# NPV = (specificity * (1-prevalence))/(((1-sensitivity)*prevalence) + ((specificity)*(1-prevalence)))

###*****************************Enough here to answer questions on specificity and sensitivity in Exam ####################



################################## Random Forest ############################################
# Min 51 Recorded lecture 13/10
# Used to help avoid overfitting 
# Install the package for Random Forest 

#install.packages ("randomForest") 
library(randomForest)
#Read Data
set.seed(123)
Data <- read.csv("Data Analytics 4thYr/Data/DT-Credit.csv", header=TRUE, sep= ";") 
str(Data) 
Data <- Data[,-1]
str(Data)
cols <- c(1, 3:9, 11:21, 23:31)
Data[cols] <- lapply(Data[cols],factor)
attach(Data)
str(Data)
# Run the Model 
RF <- randomForest(RESPONSE~.,data=Data) 
# RESPONSE is the target/response variable in this dataset
# ~. use everything else as predictors
# number of vars tried at each split is 5 
# The out of the bag estimate error is 24%, this is the overall misclassification rate
# Be v careful about predicted vs actual 
# ***********************************************************************************************
# In this confusion matrix, unlike the previous, the actual figures are in the rows and the classified are in the columns. 
# Can figure this out by adding up the actual number of yesses to figure out if the actual figures are the rows or columns
# Becareful using formulas (a + B) etc up above then 
#*************************************************************************************************
# See the result 
print(RF) 
# gives confusion matrix.
# default num trees is 500

#See importance of each predictor 
importance(RF) 
# Gives mean decreasse in GINI index. The gini index measures impurtiy. Would like to 
# have 100% purity as this means there are no misclassifications. 
# Expect every predictor (if its worthwile to add) to reduce the level of impurtiy => result in less misclassifications
# Want the max decrease in mean GINI index as this results in the biggest reduction in misclassifications. 

# Plot the importance 
varImpPlot(RF) 
#See the error vs. number of trees 
plot(RF,xlim=c(1,200)) # Plot up to 200 trees
legend("topright",legend=c("Misclassified as 1", "Misclassified as 0", "Overall misclassification rate"),pch="-", col=c("red","green", "black"))
# Can see overall misclassification rate reduces with more trees. 




##########################  For exam
# Will likely be given a dataset and may have to make minor changes to the code and then interpret the answers
##########













