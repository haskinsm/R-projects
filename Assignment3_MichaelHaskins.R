?rnorm
?matrix

## ************************************************** Part 1
#a <- rnorm( matrix(nrow=1000, ncol = 100) )  ## Rnorm gives normal dist as default so dont need to specify mean and sd
#myMatrix <- matrix(a, nrow=1000, ncol = 100)
## Better code is:  a <- matrix( rnormn( 1000*100 ), nrow=1000, ncol=100 )
myMatrix <- matrix( rnorm( 1000*100 ), nrow=1000, ncol=100 )
myMatrix
dim(myMatrix)

## Part A:
apply(myMatrix, MARGIN = 1, mean ) ## MARGIN=1 will get the mean of the 1000 rows

## Part B:
apply(myMatrix, MARGIN = 2, mean ) ## MARGIN=1 will get the mean of the 100 columns

## Part C:
myMatrix
## temp = myMatrix ^2 ## To square each entry
apply(myMatrix^2, MARGIN=1, sum) ## Gives the sum of squared row entries

## PART D:
#temp = apply(myMatrix, MARGIN=1, exp)
#apply(temp+1, MARGIN=1, sum)
apply(myMatrix, MARGIN = 1, function(x) sum(exp(x)+1))  ## User defined functions using apply are doen like this********************************************
?apply


## ****************************************** PART 2
myMatrix
# smallMatrix <- matrix( rnorm( matrix(nrow=10, ncol=10) ), nrow=10,ncol=10)
# Better code:
smallMatrix <- matrix( rnorm( 10*10 ), nrow=10, ncol=10 )
smallMatrix
# Below applys the user defined function to columns (MARGIN=2):
apply(smallMatrix, MARGIN = 2, function(y) sum(exp(y)+1))  ## User defined functions using apply are doen like this********************************************

## Inputvector can be a matrix here
myFunc <- function( inputVector, MARGIN = 1 ){ ## Default value for Margin is 1, i.e. get mean over columns/rows
  
  inputVector
  y1 <- apply( inputVector, MARGIN, mean)
  y2 <- apply( inputVector, MARGIN, sd)
  
  ## Add names to vector entries
  if( MARGIN == 1){   
    names(y1) <- paste("Row", 1:length(y1), "means")
    names(y2) <- paste("Row", 1:length(y2), "sds")
  } else {
    names(y1) <- paste("Col", 1:length(y1), "means")
    names(y2) <- paste("Col", 1:length(y2), "sds")
  }
  ## would have bee bteer to this instead:
  # if( marg == 1 ) strstart <- "Row" else strstart <- "Col"
 #  names(y1) <- paste( strstart, 1:length(y1), "Mean")
 #  names(y2) <- paste( strstart, 1:length(y2), "St dev")
  
  
  ret <- list( means = y1, sds = y2)
  
  return(ret)
}
myFunc(myMatrix, 1)  ## For rows
myFunc(myMatrix, 2)  ### for cols

myFunc(smallMatrix, 1)
myFunc(smallMatrix, 2)

x <- myFunc(inputVector = smallMatrix)  ## Calls the default version, so margin =1 => rows
x

?apply
?names
1:8
paste("Row", 1:8, "Mean")
paste("Row", 1:length(y2), "St Dev")




### ************************** ******** PART 3 (Not the best answer)

a = hist( myFunc(myMatrix, 1)$mean, main ="Row means", breaks = 12,  xlab="Distribution", col="lightblue", border = "pink", xlim = c(-0.30,0.35), freq = FALSE) ## freq=FALSE gets density
c = hist( myFunc(myMatrix, 1)$mean, main ="Row means", breaks = 12,  xlab="Distribution", col="lightblue", border = "pink", xlim = c(-0.30,0.35), freq = TRUE) ## freq= TRUE gets frequency
## Set the xlim to include all the data, xlim = c(-0.25,0.25) would encompass most of the data too but misses the tails
?hist
fatMatrix <- matrix( rnorm( 1000*1000 ), nrow=1000, ncol=1000)
fatMatrix
b = hist( myFunc(fatMatrix, 1)$mean, main ="Row means", breaks = 12,  xlab="Distribution", col="lightblue", border = "pink", xlim = c(-0.30,0.35) )
b
# freq	 logical; if TRUE, the histogram graphic is a representation of frequencies, the counts component of the result; 
# if FALSE, probability densities, component density, are plotted (so that the histogram has a total area of one). 
# Defaults to TRUE if and only if breaks are equidistant (and probability is not specified).

## Comments on HIST B & C:
b$counts
b$density
b$mids

hist( myFunc(myMatrix, 1)$mean, main ="Row means", breaks = 12,  xlab="Distribution", col="lightblue", border = "pink", xlim = c(-0.30,0.35), freq = TRUE)
hist( myFunc(fatMatrix, 1)$mean, main ="Row means", breaks = 12,  xlab="Distribution", col="blue", border = "pink", xlim = c(-0.30,0.35), add = TRUE ) ## Here add =TRUE overlays it onto previous histogram
## From this we can easily see that the range of the 'fatMatrix' hist is much less than the range of the 'myMatrix' hist
## More of its data is centred around 0 as would be expected giving rnorm generated a bigger matrix for it so approximates a normal dist better (I think)

###################### Correct answer to part 3:
par(mfrow=(c(1,2))) # Change plotting window to 1x2 panels

Z <- matrix( rnorm( 1000*100), nrow=1000, ncol=100)
mZ <- myFunc( Z )

hist( mZ$means, breaks=20, xlim=3*c(-1,1)/sqrt(100), xlab="means", freq=F ) ## 1000 means of 100 observations

K <- matrix( rnorm( 1000*1000 ), nrow=1000, ncol=1000)
mK <- myFunc( K )

hist( mK$means, breaks=20, xlim=3*c(-1,1)/sqrt(100), xlab="means", freq=F ) ## 1000 means of 1000 observations
## When we have a larger number of samples we would expect there to be a smaller variance in the sampling distribution of the mean

