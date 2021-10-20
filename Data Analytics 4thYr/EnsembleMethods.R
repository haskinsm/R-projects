### used to avoid overfitting and to get a better classification result or a regression tree with a lower level of error

# Contents:
  # data imputation 
  # classification Tree
  # Regression Tree
  # bagging 
  # adaboost 

###################### Read in and Manipulate Data ##################################
Data <- read.csv("Data Analytics 4thYr/Data/Movie_classification.csv",header=T)
summary(Data)
# Missings (NA's) are observed in "Time Taken".
# We replace them with  the mean of "Time Taken".
Data$Time_taken[is.na(Data$Time_taken)]<- mean(Data$Time_taken, na.rm=TRUE) #get mean Where data available and put it where there are missing figures
# This is called issing data imputation (will go through in more detail after reading week)
# Not the ideal method, but One of the best methods is to replace the missing figures with the mean of the existing figures 

#set response as factor
Data$Start_Tech_Oscar <- as.factor(Data$Start_Tech_Oscar) #Can see this var is a binary var so should be set as a factor



###################### Create Train/Test split ######################################
#install.packages("caTools")
library("caTools")
set.seed(123)
split <- sample.split(Data, SplitRatio=0.8)
train <- subset(Data, split==TRUE)
test <- subset(Data, split==FALSE)

trainw <- train
testw <- test

#install.packages("rpart")
#install.packages("rpart.plot")
library("rpart")
library("rpart.plot")


################## Classification Tree ###############################
CT <- rpart(Start_Tech_Oscar~., data=trainw, method='class', 
            control=rpart.control(maxdepth=3))
CT
rpart.plot(CT, box.palette = "RdBu", digits = 3)

testw$pred <- predict(CT, testw, type="class")   ## Prediction normally implies classification tree in Bahmans world
CM <- table(testw$Start_Tech_Oscar, testw$pred)
CM
59/107 ## Correct classifications 19+40 ==> 59
## Correct rate of predictions is 55%


################ Regression Tree ###################################
# Notice now using the continuous variable collection as target var fro regression tree
RT <- rpart(Collection~.,data=train,control=rpart.control(maxdepth=3))
RT
rpart.plot(RT, box.palette = "RdBu", digits = -3)
testw$est <- predict(RT, testw, type="vector") ## Estimate normally implies regression tree
MSE <- mean((testw$est-testw$Collection)^2)
MSE ## Used to compare later, on its own means nothing 
# The mean squared error is good measure of perfromance here
# The difference between the estimation and actual figures 



################ Bagging Classification Tree ############################################
# Bagging simply means we develop several models and then these several models would gives us different classification answers and
# majority classification gives us the classification

# Bagging simply stands for bootstrapping and aggregating 
# So we collect some random samples from my training dataset and then I develop my model based on these random samples 
# from the training dataset, these are called the bootstrap samples
# Bootsrapping simply means here sampling with replacement (Take a sample put it back, repeat)
# Take another bootstrap sample from training dataset and do this many times, to get several classification models, so for a new 
# record you have an answer for the class it belongs to. Will get many classifications for this new datapoint, 
# the bagging works on the majority vote, so which ever class that most 
# of the classification models classify it as is the answer. 

# From the first classification model where we do have some misclassified items -> dont get any lessons on how to improve
# perfromance in second model in BAGGING. 

#install.packages("randomForest")
library(randomForest)

BaggingCT <- randomForest(Start_Tech_Oscar~., data=train, mtry=18)
testw$baggingct <- predict(BaggingCT, test)
CMBagCT <- table(testw$Start_Tech_Oscar, testw$baggingct)
CMBagCT 
73/107 # See that the correct estimates has increased to 68%. This is better than using just one classification tree 


############### Bagging Regression Tree #########################################
BaggingRT <- randomForest(Collection~.,data=train, mtry=18)
testw$baggingrt <- predict(BaggingRT, test) 
MSEbag <- mean((testw$baggingrt-testw$Collection)^2) ##Get the mean of all the MSE of all the different regression 
# trees that have been created using different sampels of the training dataset that has been gotten by bootstrapping
MSEbag
MSE #Can see that the bagging regression tree achieves a lower ME for our model 



############### Random Forest Classification Tree (CT) ###########################
RFCT <- randomForest(Start_Tech_Oscar~., data=train, ntree=500)
testw$rfct <- predict(RFCT, test)
CMRFCT <- table(testw$Start_Tech_Oscar, testw$rfct)
CMBagCT 
73/107  # Gives same result as bagging
# bagging uses all your independent vars for bootstrapping, but random forest would take a matrix and not necessarily 
# all of your independent varibales would be included


################ Random Forest Regression Tree (RT) ##############################
RF <- randomForest(Collection~.,data=train, ntree=500)
testw$rf <- predict(RF, test)
MSERF <- mean((testw$rf-testw$Collection)^2)
MSERF
MSE
MSEbag  # Here can see bagging has given the lowest MSE 


############### AdaBoost (CT) ###################################################
# Similar to bagging 
# I have a testing dataset, take some bootstrap samples, make classifciation tree models based on these samples, 

# From the first classification model where we do have some misclassified items -> dont get any lessons on how to improve
# perfromance in second model in BAGGING. Dont use this information

# This is exactly what AdaBoost (Adaptive boosting) tries to overcome and learn from misclassifications. 
# Give all observations in my bootstrap samples the same weight. Have a testing dataset and training dataset, have 10000 samples, 
# 8000 training dataset, go to collect 2000 bootstrap sampples and give everything equal weight (1/n). All samples that are used to train the dataset have equal weight
# then use these samples to develop my classification tree. Then run with test dataset to see which ones are misclassified

# Whatever observations where misclassified in the last model I would give them higher weight and I 
# would adapt my model somehow to prevent misclassifction of those records. If these observationsa are misclassified again the overall 
# misclassification rate of my classification model would be higher (As these have now the higher importance in my dataset). 

# I think you repeat the process then
# Ultimately get to some point where my model would not be adapted anymore -> the precision % wont increase anymore so the algorithm stops -> then have the best model
# Apply this model to your testing dataset

# AdaBoost goes to learn something from previous model and uses lesson learned (which obs are misclassified) to imporve next classification model


# install.packages("adabag")
library(adabag)

adaboostCT <- boosting(Start_Tech_Oscar~., data=train,method='class',
                       control=rpart.control(maxdepth=6), boos=TRUE) 
pred_adaCT <- predict(adaboostCT, testw)
CMBoostCT  <- table(pred_adaCT$class, testw$Start_Tech_Oscar)
CMBoostCT 
78/107 # 33+45    Improved classification %
tree1 <- adaboostCT$trees[[1]]
tree1
rpart.plot(tree1, box.palette = "RdBu", digits = -3, roundint=FALSE)

