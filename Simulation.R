# Example below:
  # compare 3 estimators for the mean of a dist based on i.i.d. draws Y1,..,Yn
  # sample mean T(1)
  # sample 20% trimmed mean T(2)
  # sample median T(3)
# Set up inputs
set.seed(3)
S <- 1000 ## 1000 replicates
n <- 15
trimmean <- function(Y){mean(Y,0.2)} ##
mu <- 4 ##mean
sigma <- sqrt(5/3)


## Normal Data
out <- replicate(S, rnorm(n, mu, sigma)) #15 rows by 1000 columsn in this case
outsampmean <- apply(out, 2, mean) ## so apply stattistics by column
outtrimmean <- apply(out, 2, trimmean)
outmedian <- apply(out,2,median)
summary.sim <- data.frame(mean=outsampmean,trim=outtrimmean,median=outmedian)

# Take a look
head(round(summary.sim,4))


## How to asess
# install.packages("rsimsum")
library("rsimsum")
#creaetea id var for replicates
library(tidyverse)
summary.sim <- tibble::rowid_to_column(summary.sim, "replicate")
summary.sim$replicate <- factor(summary.sim$replicate)
# change data from wide to long
summary.long <- summary.sim %>% pivot_longer(!replicate, names_to="meanmeth", values_to="b")

# use rsimsum
results <- simsum(summary.long, estvarname="b",methodvar = "meanmeth", true=mu, x=TRUE)
results
summary(results)



autoplot(results, type="est_ridge")
autoplot(results, type="est")
autoplot(results, type="est_ba")
