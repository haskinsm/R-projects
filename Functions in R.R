x <- c(4,3,-4,3)
sum(x)
exp(x)
median(x)
table(x)

abs(x)
x <- append(x, 2) ## ADD TOE ND OF VECTOR
cat(x) ##pRINT AGRS
diff(x)
y <- x
identical(x,y)
jitter(x) ## Add a small amount of noise to a numeric vector

ls() ### List objects of current environement
range(x)
rep(1,5)  
rev(x) ## Reverse
seq(1,10,0.4) #Sequence (1->10, spaced by 0.4)
sign(x)
sort(x)
order(x)
tolower("Hello")
toupper("Hello")

unique(x) ## Remove duplicate entries
vector(mode="logical",5)
rbind(x,y)
cbind(x,y)
?matrix
X <- matrix(x, nrow=6, ncol=2)
Y = cbind(X, c(1,1,5,1,1))

## Function to apply a function to the rows or columns of a matrix many times:
X 
Y
?apply
apply(X, MARGIN = 2, mean ) ## MARGIN=2 will get the mean of the columns
apply(Y, MARGIN = 1, mean ) ## MARGIN=1 will get the mean of the rows


## Can write functions:
myfunc <- function (arrg1,arg2,arg3){
  ## do something with arg1....
  ## return a value
}

a.simple.func <- function(arg1 = 0, arg2 = 0, arg3 = 1){ ##Can set the defaut values to sensible values, arg3 must be 1 to avoid div by zero error
  ## Warnings on errors
  if( arg3 == 0 ) stop( "arg3 gives division by zero" ) ## stops execution of the current expression and executes an error action
  # get ratios
  y1 <- arg1 / arg3
  y2 <- arg2 / arg3
  # make a return list
  ret <- list( ratios = c( y1,y2 ), denom = arg3)
  # return the list
  return( ret )
}
a.simple.func(4,6,2) ##Rerurns a list, the first item beng the ratios, the 2nd the denom
a.simple.func(4,6,2)$ratios
a.simple.func(4,6,2)$denom

# Write a function to take a vector and file name, plot a histogram to file in pdf
a.plot.func <- function( x = NULL, filename = NULL)
{
  if( is.null(x) | is.null( filename ) )
    stop("nn Give me decent arguments!")
  #plot the histogram
  hist( x )
  #print to file
  dev.copy2pdf( file = filename )
}

summary(X)
sd(X)
var(X)
range(x)
max(x)
min(x)
IQR(x)

table(X) # A frequency table of y can be obtained by table(y)
prop.table(X) # To get proportions in each category
100*prop.table( table(X) )# tO GET PROPORTIONS



sstats <- iris
summary(sstats)
# install.packages("psych")
library("psych") ## Need it for describeBy
describeBy(sstats$Sepal.Length, sstats$Species) # Descriptive statistics by group
species_tab <- table(sstats$Species)
prop.table(species_tab) ## Gives you proportion of each species


plot(sstats$Sepal.Length,sstats$Petal.Length,
     main="Petal and Sepal Length ",
     xlab="Sepal", ylab="Petal",
     col=sstats$Species )
legend('bottomright',
       legend = levels(sstats$Species),
       col = 1:3, cex = 0.8, pch = 1)



plot(sstats$Petal.Length,sstats$Sepal.Length,
     main="Sepal Vs Petal Length ",
     xlab="Petal Length", ylab="Sepal",
     col=1, pch=1,
     ylim =c(
       min(sstats$Sepal.Length,sstats$Sepal.Width),
       max(sstats$Sepal.Length,sstats$Sepal.Width)
     )
)
points(sstats$Petal.Length, sstats$Sepal.Width,
       col=2, pch=2
)
legend('bottomright', legend = c("Length", "Width"),
       col = 1:2, cex = 0.8, pch = 1:2
) 



hist(sstats$Sepal.Length)
hist(sstats$Sepal.Length, main="Histogram of Sepal Length", xlab="Sepal Length", col="blue",
     freq=FALSE)


boxplot(sstats$Sepal.Length~sstats$Species)
boxplot(sstats$Sepal.Length~sstats$Species, main="Boxplots of Sepal Length by Species",
        xlab="Species", ylab="Sepal Length", names=c("Setosa", "Versicolor", "Virginica"), col =1:3)


barplot(species_tab)
pie(species_tab)


par(mfrow=c(2,2)) ## Allows you to display 4 graphs (2x2)
hist(sstats$Sepal.Length)
hist(sstats$Sepal.Width)
barplot(species_tab)
pie(species_tab)

library(ggplot2)
ggplot(data=iris, aes(x=Sepal.Length, fill=Species)) +
  geom_density(alpha=0.5) +
  xlim(3.9,8.5) +
  theme_minimal()
