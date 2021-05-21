## Link to source of chosen dataset: https://www.kaggle.com/larsen0966/student-performance-data-set                                    

## My chosen dataset was gathered with the aim of measuring student performance in two secondary schools in Portugal. 
# The dataset was modelled under binary/five-level classification and regression tasks. There are 650 observations and 
# 33 variables in this dataset. 2 of the variables relate to exam performance in the first and second semester and another 
# variable relates to the final year grade.

# In the data cleaning stage of this project several of these variables were removed as I deemed them to be inconsequential to my research question:
# Research  Q:  Does the level of parents’ education affect their child’s performance in school?

## The reduced dataset
dataset = read.csv(file = "ReducedStudentPerformanceDataset.csv", sep=",", header = TRUE) 
dim(dataset) 
# In the reduced dataset there are 17 variables. In contains the same number of observations as the main dataset (649)





############################# Data Cleaning #####################################

## I will now alter the dataset further before I begin the exploratory analysis. 

## 1st I will create a new 3-level classification variable called parent's education. This will replace the variables 
# Medu and Fedu (Mother's and Father's Education). 1 will represent both parents having only a primary education or less, 
# 2 will represent only one having a secondary education or higher, 3 will represent both having a secondary education or higher. 

## Add new column called Pedu 
dataset$Pedu <- 0

## I will now subset the data into 3 subsets

# The 1st will have both Parents having an education lower than secondary school. Pedu will be set to 1 for this group
subsetPedu1 = subset( dataset, (Medu == '0' | Medu == '1' | Medu == '2') & (Fedu == '0' | Fedu == '1' | Fedu == '2') )
subsetPedu1$Pedu <- 1 ## Set Pedu to 1

# The 2nd will have only one of the parents having a secondary education or higher. Pedu will be set to 2 for this group
subsetPedu2 = subset( dataset, ( ((Medu == '0' | Medu == '1' | Medu == '2') & (Fedu == '3' | Fedu == '4')) | ((Fedu == '0' | Fedu == '1' | Fedu == '2') & (Medu == '3' | Medu == '4')) ) )
subsetPedu2$Pedu <- 2

# The 3rd will have both parents having a secondary education or higher. Pedu will be set to 3 for this group
subsetPedu3 = subset( dataset, ( (Fedu == '3' | Fedu == '4') & (Medu == '3' | Medu == '4') ) ) 
subsetPedu3$Pedu <- 3

# Now combine the subsetted dataframes using rbind
myDataset = rbind(subsetPedu1, subsetPedu2, subsetPedu3) ## This will combine the dataframes by row. May need to shuffle the dataset later on

## I will now delete the Medu and Fedu fields as they have been replaced by the Pedu field
myDataset$Medu <- NULL
myDataset$Fedu <- NULL


## Now I will change the grade variable 'G3' from a 0-20 classifcation to a 3-level classifcation field with a grade of below 7 being a low-grade,
# 7 - 13 being an average-grade and 14 - 20 being a high-grade.

## Need to convert G3 column to Factors so I can use the levels function
myDataset$G3 <- factor(myDataset$G3)

levels(myDataset$G3)[1:8] <- "Low-Grade" ## This corresponds to grades of 0, 1, 5 and 6 (0:6). (There are no 2,3,4 grades)
levels(myDataset$G3)[2:5] <- "Average-Grade" ## This corresponds to grades between 7 and 13
levels(myDataset$G3)[3:7] <- "High-Grade" ## This corresponds to grades between 14 and 20 (There are no 20 grades)


## Now I will shuffle the dataset, so the data is mixed up and not in order of Pedu
set.seed(123)
rows <- sample(nrow(myDataset))
cleanData <- myDataset[rows, ]

## Renaming the G3 field to FinalGrade which is more insightful 
names(cleanData)[15] <- "FinalGrade"


## Save the cleaned Data as a csv file in my local directory 
write.table(cleanData, file = "cleanProjectData.csv",row.names = FALSE, sep = ",")
## Row.names = False as don't want index





######################################## Exploratory Analysis ################################
dim(cleanData) #649 obs and 16 variables
head(cleanData) ## Check 1st 5 entries
summary(cleanData) ## Summary of the data
summary(subsetPedu1) ## Summary of the data where parents education level = 1

### Get count of students who do not want to go to education then breakdown into what their Pedu classifciation is
summary(cleanData[cleanData$higher == "no", ])
countOfPedu = cleanData[cleanData$higher == "no", ]$Pedu
table(countOfPedu)

## Breaksdown what Pedu classification students who have no access to internet at home have 
table(cleanData[cleanData$internet == "no", ]$Pedu)

## Breakdown of parents education level and the grade of the students
table(cleanData$Pedu, cleanData$FinalGrade)


########  Create a correlation heatmap #########
# Src #https://stackoverflow.com/questions/52554336/plot-the-equivalent-of-correlation-matrix-for-factors-categorical-data-and-mi

#library(ggplot2)
library(tidyverse)
# install.packages("lsr")
library(lsr)
library(dplyr)
#### Make sure MASS project is not loaded ######################

# function to get chi square p value and Cramers V
f = function(x,y) {
  tbl = cleanData %>% select(x,y) %>% table()
  chisqPval = round(chisq.test(tbl)$p.value, 2)
  cramV = round(cramersV(tbl), 2) ## Round to two decimal places
  data.frame(x, y, chisqPval, cramV) }

# create unique combinations of column names
# sorting will help getting a better plot (upper triangular)
cleanDataComb = data.frame(t(combn(sort(names(cleanData)), 2)), stringsAsFactors = F) 

# apply function to each variable combination
cleanDataRes = map2_df(cleanDataComb$X1, cleanDataComb$X2, f)

# plot results
cleanDataRes %>%
  ggplot(aes(x,y,fill=chisqPval))+
  geom_tile()+
  geom_text(aes(x,y,label=cramV))+
  scale_fill_gradient(low="red", high="yellow")+
  theme_classic() + 
  scale_x_discrete(guide = guide_axis(angle = -45)) +## Rotate x labels 45 degrees so theyre more legible 
  labs(title="Correlation Heatmap of School Performance dataset", subtitle = "Calculated using Cramers V (indicates how strongly categorical variables are associated)", 
        y="Variables", x="Variables", caption = "Cramers V ranges from 0 (low corr) to 1 (high corr)")


### Another attempt at creating a corraltion heatmap:
# This attempt is not as good as the first 
# install.packages("ggcorrplot")
library(ggcorrplot)

model.matrix(~0+., data=cleanData) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)





















