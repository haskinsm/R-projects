## set up inputs
set.seed(3)
S <- 10
n <- 15
trimmean <- function(Y){mean(Y,0.2)}
myRate <- 4
sigma <- sqrt(5/3)

##Normal data:
out <- replicate(S, rexp(n, rate = myRate))
outsampmean <- apply(out,2, mean)
outtrimmean <- apply(out,2, trimmean)
outmedian <- apply(out,2, median)
summary.sim <- data.frame(mean=outsampmean,trim=outtrimmean, median=outmedian)

#take a look
head(round(summary.sim, 4))

#rsimsum
library(rsimsum)
#create id variable for replicates
library(tidyverse)
summary.sim <- tibble::rowid_to_column(summary.sim, "replicate")
summary.sim$replicate <- factor(summary.sim$replicate)
#change data from wide to long
summary.long <- summary.sim %>% pivot_longer(!replicate, names_to = "meanmeth", values_to = "b")

#use rsimsum
results <- simsum(summary.long, estvarname="b",methodvar="meanmeth", true=myRate, x=TRUE)
results
summary(results)

#visualise results
autoplot(results, type = "est")
autoplot(results, type = "est_ba")
autoplot(results, type = "est_ridge")




############################## Part 2 ########################
# Now , comment on your conclusions regarding the results for the three estimators for the mean of the population.


# Unlike the normal distribution, in an exponential distribution we would not expect the mean to be equal to the median and trimmean. 
# The average 

# The bias is the difference between the true value and what it estimated. From the bias in point estimate we can see that all
# the  3 estimators are not exactly equal to the true value. They are very close to the true value however. 

# The gain in precsion relative to the mean uses the mean as the reference as its the first estimator when alphabetically sorted. 
# Can see compared to the mean that the Median was less accurate and the trim mean was also less accurate. 

# The mean squared error was lowest for the mean. It was only slighlt higher for themedian and trimmean estimaotrs. 

# From the 2nd graph we can see that the mean vs trim are the most similar (Has the tighested CI band in the graph).

# From the 3rd graph we can see trimmean and mean look the most similar 

# In conclusion the results, like we would expect, are not as similar as those of a normal distribution. The mean, median and trim are not 
# equal




########################### Part 3 ##########################

# I do not think that simulation studies would be benefical to the analysis of my dataset. My variables are all categorical so I 
# do not see any benefit in simulating the data. I am not short on observations and have over 500 currently. If I were to simulate this dataset 
# it would take a long time and I dont think my laptop is up for it. 

# I do not think I will have time to do this and should already have enough to write about. I have plans to do an oridnal regression
# and some other stuff already

