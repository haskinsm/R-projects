?rnorm
?matrix

## ************************************************** Part 1
a <- rnorm( matrix(nrow=1000, ncol = 100) )
myMatrix <- matrix(a, nrow=1000, ncol = 100)
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
apply(myMatrix, MARGIN = 1, function(x) sum(exp(x)+1))
?apply


## ****************************************** PART 2
myMatrix
smallMatrix <- matrix( rnorm( matrix(nrow=10, ncol=10) ), nrow=10,ncol=10)
smallMatrix

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




### ************************** ******** PART 3
a = hist( myFunc(myMatrix, 1)$mean, main ="Row means", breaks = 12,  xlab="Distribution", col="lightblue", border = "pink", xlim = c(-0.30,0.35), freq = FALSE) ## freq=FALSE gets density
c = hist( myFunc(myMatrix, 1)$mean, main ="Row means", breaks = 12,  xlab="Distribution", col="lightblue", border = "pink", xlim = c(-0.30,0.35), freq = TRUE) ## freq= TRUE gets frequency
## Set the xlim to include all the data, xlim = c(-0.25,0.25) would encompass most of the data too but misses the tails
?hist
fatMatrix <- matrix( rnorm( matrix(nrow=1000, ncol=1000) ), nrow=1000,ncol=1000)
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