################### Use Regression Tree when target varible is continuous ###################################
#############################################################################################################

rm(list=ls())  ##Empty envrironment -> removes all objects present in the workspace
#setwd("XXX")
getwd() ##Check directory



Data <- read.csv("Data Analytics 4thYr/Data/RT.csv", header=TRUE, sep=";")
Data
#install.packages ("partykit")
#install.packages ("party")
library("partykit")
library("rpart")
library("party")
attach(Data) #used to access the variables present in the data framework without calling the data frame. 
#*Remember to use detach(<Data>) when finished
RTModel <- rpart(Weight ~ Height, method="anova", 
                 control=rpart.control(minsplit=5, minbucket=2, maxdepth=4))
#Anova is to with that SSE or whatever stuff
Fig1 <- plot(as.party(RTModel)) 
# Can see that as height increases weight increases (from increasing mean of weight of boxplots as height increase)
print(RTModel)
# Can see the spliting condition, number of observations, deviance, mean y val (Weight in this case) for each node or bucket on each line
# Table shows same info that you can see in the graph and also the deviance.
# The deviance is simply the difference between the prediction and actual observed figure fsquared for each node. 
# Each line talks about one fo the nodes, the astrix denotes if theyre terminal nodes
summary(RTModel)
# At very top Gives something about complexity: Gives number of splits, relative error (Prob is 1-R^2, as think R^2 measures the accuaracy so the error should be 1-R^2)
# , cross validation error, and the standard deviation of this error
# This is a tool to control the size of the tree
# A rule of thumb to control the complexity is to stop while the cross validation error (xerror) less the standard deviation of this error (xstd)
# Is still less than the relative error. Here this indicates that perhaps one split is enough 
# Lower down gives the mean of dependent variable at each node and mean squared error (MSE) of estimation at each node
rsq.rpart(RTModel)
# As the change in the relative error slows it is an indication that the number of pslits is enough, so here might be a good idea to stop after two splits
detach(Data)  ##*********Remember to deatch Data so as not to mask future variables when use attach function again ************************




###############################  2nd Dataset ################################

Data2 <- read.csv("Data Analytics 4thYr/Data/Fuel.csv", header=TRUE, sep=";")
Data2
attach(Data2)
str(Data2) ## Can see
########## ************ Remember to change to factors ***************************************
Country <- as.factor(Country)  #Note country are codes i.e. 1, 2,3,4 ...etc
Year <- as.factor(Year)  #Year obsv should be treated as factor 
RTModel2 <- rpart(mpg ~., data=Data2, method="anova", 
                  control=rpart.control(minsplit=6, minbucket=5, maxdepth=3))
Fig2 <- plot(as.party(RTModel2)) 
print(RTModel2)
summary(RTModel2)
rsq.rpart(RTModel2)
# Talks about something useful here. 
detach(Data2)  ##*********Remember to deatch Data so as not to mask future variables when use attach function again ************************


