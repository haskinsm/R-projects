## ggplot2 works directly with data frames
## base graphics work with vectors
library(ggplot2)

edison <- rnorm(100,0,1)
tesla <- rnorm(100,1,1)
DF <- data.frame(edison = edison, tesla=tesla)
plot(edison, tesla) ##plot from inital vectors
plot(DF$edison, DF$tesla) ## plot vectors from data frame

?ggplot
ggplot(DF, aes(x=edison, y=tesla)) ## aes sets the axis
## Note we have not told ggplot what type of graph we want or to plot any points
## Any info that is part of the source dataframe has to be specified inside an aes() function

## Now we add  a basic geom layer
# geom_point() makes a scatterplot
ggplot(DF, aes(x=edison, y=tesla)) + geom_point()

G <- ggplot(DF, aes(x=edison, y=tesla)) + geom_point()
G

G <- G + geom_smooth(method="lm") ## geom_smooth is a geom layer that adds a smooth over yur data, here using a linear smooth/fit. Has conifdence bands in grey
plot(G) ## or just G works fine



##############     We can zoom in on the graph in two ways:

#1) by deleting, from the graph not the dataframe, points outside the range specified by xlim and ylim
# this will change our smooth which might be whate we want if we rhink there are outliers

G + xlim(c(-2,2)) + ylim(c(-1,1.5)) ## Removes points that are outside these ranges from the graph and recalculates the linear smooth

#2) by using coord_cartesian()
# this is a true zoom so our smooth does not change from our orginal graph 

G + coord_cartesian(xlim=c(-2,2), ylim=c(-1,1.5))



##### Titles & labels   (Grpahs should be standalone, able to understand just by looking at them)
## Option 1
G + labs(title="A Demo Graph", subtitle="made by Michael", y="AC Power", x="DC Power", caption="also a little caption")

## Option 2
G + ggtitle("A Demo Graph", subtitle ="made by Michael") + xlab("DC Power") + ylab("AC Power") 


##### Syntax - static aesthetics (static means dont change/vary bassed on df groups/clusters etc)
## Initialize basic ggplot with titles etc

G <- ggplot(DF, aes(x=edison, y=tesla)) + labs(title="A Demo Graph", subtitle = "Made by Michael", y="AC Power", x="DC Power", caption="also a little caption")

# specify static colours, design, size, etc inside our geom layers
G <- G + geom_point(shape="diamond", col="pink", size=3) ## Affects the appearence of the points

G <- G +geom_smooth(method="lm", linetype="dotdash")
plot(G)

## Run vignette("ggplot2-specs") to learn more about aesthetic options
vignette("ggplot2-specs")



##### Syntax  - adaptive aesthetihics 
## Use this when we want fro colours etc to vary based on some other variable, we must include it in an aes() function in the appropriate geom layer


# add a new categorical variable to our Data frame
DF$categories <- seq(1:5) #(top graph has this as factor, bottom graph as numeric) 
## Categories will have similar enough colours in ggplot 
## You can change this by changing the way the dataframe is set up to factors or something, not sure


# initialize basic ggplot with titles etc
G <- ggplot(DF, aes(x=edison, y=tesla)) + labs(title="A Demo Graph", subtitle = "Made by Michael", y="AC Power", x="DC Power", caption="also a little caption")

# specify adaptive colours
G <- G + geom_point(shape="diamond", aes(col=categories), size=3)
plot(G)

#note we can remove legend and change colour palette

#for numeric Category variable (DF)
G + theme(legend.position = "None") + scale_colour_gradient(low="green", high="darkgreen")

#for factors Category variable (DF)
G + theme(legend.position ="None") + scale_colour_brewer(palette = "Set3") ## Wont work prob, cause the DF isnt factors or whatever



#### Syntax - X and Y axes

# we can use scale_x_continuous (or scale_y_date etc) to set the number of ticks/breals and their labels

#We can specify he number and values either within the function, or as vectors passed to the function

# labels and breaks must be of the same length

xbreaks <- seq(-3, 2.5, 0.5) ## start at -3 end at 2.5, breaks every 0.5
G + scale_x_continuous(breaks = xbreaks, labels = letters[1:12])

# We can also format the axis labels by using sprintf, or defining our own function
G + scale_x_continuous(breaks=seq(-3,2.5,1), labels = sprintf("%1.2f%%", seq(-3, 2.5,1))) + 
  scale_y_continuous(breaks=seq(-1,3.5,0.5), labels = function(x){paste0(x/10, '*10')})
?paste0
## x axis breaks here starts at -3 ends at 2.5 increments every 1
## x asis labels here are set to % with 2 decimal places, would also like them to be in a seq starting at -3 ending at 2.5 and incrementing every 1
## y axis breaks here starts at -1 and ends at 3.5, increments every 0.5 units,
# y axis labels here are set to a function, every value divided by ten with a little bit of text after it which says *10
  ## this sort of stuff is useful if you want it scaled in millios or something like that



## Tips and tricks
## ggsave("myplot.pdf") saves the most recent ggplot to disk 
## ggsave("myplot.pdf", plot=G) saves ggplot called G to disk

## + is used to add a geom layer, - is used to remove a geom layer