############################################### PART 1

## After placing data file in working directory read in the data
cancerData = read.csv(file = "breast_cancer.dat", sep=",", na.strings = "NA", header = TRUE) 
## The data is comma delimited and has headers

dim(cancerData)
summary(cancerData) ## From this can see there are issues with bare nuclei field

## The first field of the data is a unique ID number. The next 9 fields are mapped
## to a 1-10 scale. The last field 'class_tumor' is like binary data with 2 representing 
## a beingn tumor and 4 for malignant. It might be wise to change these to the more conventional
## 0 and 1 for benign and malignant. Want to predict if a mass is malignant or not so makes sense 
## to have 1 representing a malignant mass.


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
  cleanData$class_tumor[cleanData$class_tumor == 2] <- 0
  cleanData$class_tumor[cleanData$class_tumor == 4] <- 1
  summary(cleanData$class_tumor) ## Data now looks good, aslo checked in environment
  
  
set.seed(123) ## Told to do this so results are reproducible 
## Will now randomly assign observations to a training and test set in the ratio 4:1

numRows = dim(cleanData)[1] ## Number of obs
numTrainingData = floor(numRows*0.80) ## Num of obs that will be in training data

## Creates a sample of which observations should be assigned to the training or test data
trainingInd <- sample(seq_len(nrow(cleanData)), size = numTrainingData)

## Create training and test data based on randomly sleected data above
trainingData <- cleanData[trainingInd, ]
testData <- cleanData[-trainingInd, ] ## - indicates the remaining data


#?glm
## Now create a genralized linear model for the dataset. With the goal of classifying malignant tumours
## Note: class_tumor ~ . , would get everything is a dataset but wouldnt make sense to include the ID number here so dont do that
mymodel = glm(class_tumor ~ clump_thick + unif_cell_size + unif_cell_shape + marg_adhesion + epithelial_size + bare_nuclei + bland_chromatin + normal_nucleoli + mitoses, data= trainingData, family=binomial)
## Trying to predict if a tumor is Malignant (=0) or not (=1) so def family = binomial
  
## Check model
mymodel ## Looks good
summary(mymodel)
summary(mymodel)$call ## Not sure if this is what is meant by use the summary function
## to print your glm fit to screen.

plot(mymodel) ## Not compeltely sure what this does, 
# but results in a lot of graphs being displayed





########################################## PART 2
## First make predictions:
# predict some probabilities from my model 
mypredict = predict(mymodel, type = "response")
mypredict

# install.packages("ROCR")
library(ROCR)
# Now create a prediction instance for the training set   
ROCRpred = prediction(mypredict, trainingData$class_tumor)
ROCRpred

## Now assess the performance of predictions based on diff thresholds. 
ROCRperf = performance(ROCRpred, "tpr", "fpr") 
# tpr:True positive rate. P(Yhat = + | Y = +). Estimated as: TP/P.
# fpr:False positive rate. P(Yhat = + | Y = -). Estimated as: FP/N.
#?performance
ROCRperf ## Stores the false postive rate, true postive rate and the cutoff (Which i think is the threshold)

# Plot ROC curve
# ?plot
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), xlim = c(0,0.05), ylim = c(0.8,1), main = "ROC curve of predicting Malignant tumors")
# The above graph makes use of xlim and ylim to zoom in so to say

## Messing with the aspect ratio just made the plot look worse: plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), asp = 0.25)
## I think the numbers on the line are the diff prob thresholds 

## The assignment/probability threshold I believe would be best is 0.2. I believe this as it maximizes 
## the true positive rate (very close to being 100% (97.4%)), while also minimizing the false positive rate (circa 2.4%) 
## Due to the nature of what we are predicting here it is best to maximize the true postive rate. Once the true postive rate 
## is maxed we can focus on minimizing the false postive rate. 

## By picking a threshold of assignment of 0.2 this means that if the porbability the tumor
## being malignant is 0.2 or greater the tumor will be classified as Malign. If the prob is less than
## 0.2 the tumor will be classified as Benign.




###### Now apply model to test data

# Wrong: testModel = glm(class_tumor ~ clump_thick + unif_cell_size + unif_cell_shape + marg_adhesion + epithelial_size + bare_nuclei + bland_chromatin + normal_nucleoli + mitoses, data= testData, family=binomial)

## First make predictions:
# predict some probabilities from my model 
testPredict = predict(mymodel, type = "response", newdata = testData)
testPredict
## 0.2 was the selected threshold
## if gretaer than or equal to 0.2 assign to 1 (Malignant)
## if less than 0.2 assign to 0 (Benign)
testPredict[testPredict >= 0.2] = 1
testPredict[testPredict < 0.2] = 0
testPredict

#classification <- testPredict >= 0.2
#classification
#classification[classification == FALSE] <- 0 ## prob could have doen this all in one line

#trueClass = testData$class_tumor > 0
## Performance measures
#?table
dim(testData)
table(testData$class_tumor, testPredict) 
## From this table we can see that of the 137 samples in the test dataset
# 79 Benign samples were correctly classified and 5 were (incorrectly) classified as a false postive.
# 51 Malignant tumors were correclty classified as being malignant and 2 were (incorectly) 
# classified as being false negatives.

## True postive (Malign correclty classified) rate: = 51/(51+2) = 96.23% 
## False postive (Benign incorrectly classified) rate = 5/(79+5) = 5.95%
# True negative (Benign correctly classified) rate: = 79/84 = 94.05%
## False negative (Malign incorrectly classified) rate: = 2/53 = 3.77%%


## In conclusion I would be quite happy with the performance of this model. 
## The % of true positive classifications is very high at 96.23%. In a perfect world 
## this would be 100%, but 96% correct is still a very good result. The false negative rate
## of 3.77% is not ideal as this means people could go without treatment and possibly die.
# I would be less 
## concerened with the True negatuve rate although it is still somewhat important that it is high.
## 





library(caret)
?confusionMatrix
# confusionMatrix(testData$class_tumor, testPredict >= 0.2, postive = "pos")  
## Wont work: Error: `data` and `reference` should be factors with the same levels.
confusionMatrix(testData$class_tumor, testPredict, postive = "pos")  
## Samer error persists. 
