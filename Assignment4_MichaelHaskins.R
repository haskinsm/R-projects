library(ggplot2)

############################### PART 1
## Read in the text file called Currency located in my Working directory
## Use sep="\t" to achieve correct formatting, and set dec="." and since there are no headers header=FALSE
allCurrData = read.csv(file = "Currency.txt", sep="\t", dec=".", header = FALSE)
allCurrData ## Check data -> looks good
dim(allCurrData)
## has 1812 rows and 2 columns. Colum 1 is of the form day month year and colum 2 contains the exchange rate
names(allCurrData)[1] <- "Date"
names(allCurrData)[2] <- "ExchangeRate" ## This names the columns as Date and ExchangeRate


## Now formatting Dates
?as.Date
?which
?apply
allCurrData$Date <- as.Date(allCurrData$Date, "%d %b %Y") ## Change the format of column Date to date format
allCurrData ## worked

## Need to create a subset which contains relevant dates
?subset
brexitCurrData = subset(allCurrData,  (Date >= '2016-06-19' & Date <= '2016-07-09') )
brexitCurrData

GG <- ggplot(brexitCurrData, aes(x=Date, y=ExchangeRate)) + labs(title="Avg Daily Exchange Rate around Brexit Vote", subtitle = "GBP:USD, 2016-06-19 to 2016-07-09", y="daily avg", x="Date")
GG <- GG + geom_line(col="green", linetype = 2) ## geom_line gives a line, linetype = 2 gives broken line and set colour of this line to green
## GG <- GG + xlim(c("2016-06-19", "2016-07-09")) ## Doesn't work will try subset the data 
## This code might work but have already subsetted it now so might try later if have time:    scale_x_date(limits = as.Date(c("2016-06-19","2016-07-09"))) 
GG <- GG + geom_hline(yintercept = mean(brexitCurrData$ExchangeRate), color="blue") ## This adds the avergae line for the period
#GG <- GG + scale_x_continuous(breaks= as.Date(c("2016-06-19", "2016-07-19")), minor_breaks = as.Date(c("2016-06-19", "2016-06-26", "2016-07-02", "2016-07-09"))   )
GG <- GG + scale_x_date( date_breaks = "1 week", date_labels = "%b %d", date_minor_breaks = "1 day") ## Breaks every week, y labels like in example, and minor breaks every day
GG <- GG + scale_y_continuous(breaks=seq(1.2, 1.5, 0.05), labels = seq(1.2, 1.5, 0.05) )
GG <- GG + theme_bw() ## Remove background colour
GG <- GG + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) ## Remove all gridlines
GG <- GG + theme(axis.text.y = element_text( angle = 90) ) ## Rotates y axis labels 90 degrees
plot(GG) ## Looks good

## Now save to pdf
ggsave("BrexitExchange.pdf", plot=GG) # saves ggplot called GG to disk and names the pdf BrexitExchange

?scale_x_date
?geom_line
?type
?lty
?xlim
?las
?par
?cex


###################################   PART 2
my_iris <- iris ##read in default iris dataset
my_iris

#paletteR = rainbow(3) ## Sets the pallete

## IG for Iris Graph
IG <- ggplot(my_iris, aes(x=Sepal.Length, y=Sepal.Width, col = Species)) + labs(title="Iris Sepal Length vs Width", y="Sepal Width", x="Sepal Length")
## Descriptive title is useful for readers

IG <- IG + geom_point( shape=1, size=3) + ## plots the points as hollow circles and gives the points a colour matching the rest of their species
        stat_ellipse(type = "norm", linetype = 2) ## Adds normal dist elipse around the groups. Makes it easy to spot groups, and also outliers in the groups
        # stat_ellipse(type = "t")
IG <- IG + theme_bw() ## Remove background colour, this makes it more visually appealing.
IG <- IG + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) 
## Remove all gridlines, this makes it more visually appealing although reduces the readbility

#IG <- IG + theme(axis.text.y = element_text( angle = 90) ) ## Rotates y axis labels 90 degrees
#IG <- IG + geom_mark_ellipse(expand=0, aes(fill=Species))
IG <- IG + theme_light() ## set the theme to dark ## This did not make it anymore appealing so was ommitted
#IG <- IG + scale_colour_brewer(palette = paletteR)
plot(IG)

ggsave("irisnew.pdf", plot=IG) # saves ggplot called GG to disk and names the pdf irisnew

?geom_density2d
#IG2 <- ggplot(data=iris, aes(x=Sepal.Length, fill=Species)) +
  #geom_density(alpha=0.5) +
  #xlim(3.9,8.5) +
  #theme_minimal()
#plot(IG2)
?palette
?stat_ellipse
?geom_point
?labs
?legend


###################################  PLOT 3

my_dims <- diamonds
# install.packages("gridExtra")
# install.packages("hrbrthemes")
# install.packages("viridis")

?diamonds
library(gridExtra) ## Used like the par function for normal plots, to get two ggplots side by side
#library(tidyverse) 
#library(hrbrthemes)
#library(viridis)

plot1 <- ggplot( my_dims, aes(x=carat, y=price, colour = cut)) + 
  labs(title="Boxpot of Price vs Colour ", y="Price in USD", x="Carat") +
  geom_boxplot() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
 # geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme( legend.position="left" ) 

plot(plot1)

plot2 <- ggplot(my_dims, aes(x=color, y=price, colour = cut)) + labs(title="Boxpot of Price vs Colour ", y="", x="Colour Category")
plot2 <- plot2 + geom_boxplot(show.legend = FALSE)

plot(plot2)

grid.arrange(plot1, plot2, ncol=2) ## Doesnt look great so scrapping this idea


## I belive a boxplot is best for the graph colour vs price as Colour is a categorical variable
plot2 <- ggplot(my_dims, aes(x=color, y=price, colour = cut)) + labs(title="Boxpot of Price vs Colour ", y="Price in USD", x="Colour Classification (Worst to Best)")
## Gives title and titles of x, y axis and sets the aesthetic 
plot2 <- plot2 + geom_boxplot(show.legend = TRUE) ## Creates boxplot
plot2 <- plot2 + theme_classic() ## Gets rid of grid lines and gives the graph a nice simple loook
plot2 <- plot2 + theme(legend.position = "bottom") ## Having it at the bottom makes the graph easier to read and means it isnt as zoomed out
plot(plot2)
ggsave("diamonds.pdf", plot=plot2) # saves ggplot called GG to disk and names the pdf  diamonds



###################################### PART 4

## Didnt look nice so abandoned this
## I believe a desnity plot would be best for price of diamonds split into two groups one where carat >2 the other where carat <=2
##ggplot(my_dims, aes(x=price, fill= carat > 2 ) ) +
  ##geom_density(alpha=0.5) +
 ## theme_minimal()

## On second though just a simple line plot of carat vs price would be good, as carat and price are both continuous variables

plot3 <- ggplot(my_dims, aes( x= carat, y=price, col=color)) + labs(title="Lineplot of Price vs Carat ", y="Price in USD", x="Carat")
## Assigns aesthetic stuff and adds titles to axis and plot
plot3 <- plot3 + geom_line(size=1, linetype=2) + ## uses a broken line and small line to increase visiblit
  theme_minimal() ## Gives a minimal theme so it is easy to read the data

plot(plot3)  
