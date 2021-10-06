### Decision (Classification) Trees ###################
# 22/09/2021

########## Read in Data ##############
##setwd("~/TCD/TCD 2021-22/STU44003") ##Presume this is code for him
getwd() ##Check directory
Data <- read.csv("Data Analytics 4thYr/Data/DT-Credit.csv", header=TRUE, sep= ";") ##Saved in a folder called 'Data'in this files directory


######### Check and manipulate dataset########
str(Data)  # str() is used for compactly displaying the internal structure of a R object
Data <- Data[,-1] # Remove 1st column of data
str(Data) 
attach(Data) #used to access the variables present in the data framework without calling the data frame. *Remember to use detach(<Data>) when finished


########## Buidling Decision Tree Model #################
#install.packages("rpart")
library(rpart)
## NOTE: INCORRECT as variables entered in wrong. Need to specify what variables are factors and NOT continuous, otherwise R will assume everything is continuous.****************************8
## Fix is below. 
DT_Model <-rpart(RESPONSE~., data=Data, control=rpart.control(minsplit=60, minbucket=30, maxdepth=4 )) 
# RESPONSE is Var in Data its the response var, ~. uses all other vars in Data as explanatory vars 
# minsplit: the minimum number of observations that must exist in a node for a new split. 
# minbucket: the minimum number of observations in any terminal node -> terminal node is the end node I think (the bucket) -> Tells you the classification decison or prob distribution for classification 
# Maxdepth: Maximum depth for any node, with the root node counted as depth 0.  
DT_Model
mean(RESPONSE)

########## Visualization of Decision Tree ############
#install.packages ("partykit") 
library("partykit") 
plot(as.party(DT_Model)) 
# Can see chl_account was the first explantory var picked for model
## Note R did not realize that some vars were factors and not continuous variables, not numerical ***********************************
## It also did not realize the response var was not continuous but binary. 


################## Now fix Data, specifying what is factors 
str(Data) 
cols <- c(1, 3:9, 11:21, 23:31)
Data[cols] <- lapply(Data[cols],factor)  ##lapply applys function to everything in cols -> here it turns everything into factors 
str(Data)


######## Now create a Decison Tree model using Correctly altered Data ######### 
DT_Model <-rpart(RESPONSE~., data=Data, control=rpart.control(minsplit=60, minbucket=30, maxdepth=4 )) 
print(DT_Model)
plot(as.party(DT_Model)) 
### Can now see its much easier to navigate the tree and makes much more sense (Dark grey is yes, can see percentages of response var (People who paid back the loan who got it)) 


####### Change the response to Y/N answers and rerun the decision tree model function. ##############
Target=ifelse(RESPONSE==1,'Y','N')
Data <- data.frame(Data,Target)
str(Data)
Data1=Data[,-31] 
DT_Model1<-rpart(Target~., data=Data1, control=rpart.control(minsplit=60, minbucket=30, maxdepth=4 ))
plot(as.party(DT_Model1))
print(DT_Model1)

# Change the control parameters and see the change in the output (minsplit=60, minbucket=30, maxdepth=8). 
