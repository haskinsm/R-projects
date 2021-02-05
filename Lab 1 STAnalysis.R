x <- rnorm(100)
hist(x)

employee <- c('John Doe','Peter Gynn','Jolie Hope')
salary <- c(21000, 23400, 26800)

startdate <- as.Date(c('2010-11-1','2008-3-25','2007-3-14')) ###Note how to set data
startdate

employ.data <- data.frame(employee, salary, startdate)
employ.data

str(employ.data) #to see the structure of the data frame.
## In the environment window, click employ.data itself to open a view of the data itself in a tab beside your R Script
colnames(employ.data) = c("employee","salary","startdate") 

# Download the csv (comma separated values) file Lab0.csv from Blackboard into your R working directory. 
# In the environment window, click Import Dataset and select From Text(readr)
# Browse to the location of Lab0.csv and click "Import"
# In the environment window, click the blue arrow beside Lab0 to see details of the data frame
# In the environment window, click Lab0 itself to open a view of the data itself in a tab beside your R Script
# Alternatively, you can read in the dataset using code.
# In the environment window, type and run the following two lines of code. What is the difference between the two data frames Lab0_true and Lab0_false?
  
Lab0<-read.csv(file="Lab0.csv", header=TRUE)
# Lab0_false<-read.csv(file="Lab0.csv", header=FALSE) ##Headers so this is worng

summary(Lab0$AGE) # to see a selection of summary statistics for the variable AGE
table(Lab0$Gender) # to see the frequencies of each gender category
table.gender <- table(Lab0$Gender) # to create a table in memory named table.gender
prop.table(table.gender) #to see the proportions of each gender category.
table(Lab0$EDUC, Lab0$Gender) #to see a two-way cross-tabulation of the frequencies of variables EDUC and Gender
ftable(Lab0$EDUC, Lab0$Gender, Lab0$Job.class) # to see a three-way cross-tabulation of the frequencies of variables Educ, Gender and Job.class.

boxplot(Lab0$AGE) #to see a basic boxplot of the variable AGE
boxplot(Lab0$AGE~Lab0$Gender) #to see a basic boxplot of the variable AGE in each level of the variable Gender. 
plot(Lab0$EARN, Lab0$AGE) # to see a basic scatterplot of EARN and AGE.
Lab0$EARN_1000 = Lab0$EARN/1000 # to create a new variable equal to EARN divided by 1000.
plot(Lab0$EARN_1000, Lab0$AGE) 


