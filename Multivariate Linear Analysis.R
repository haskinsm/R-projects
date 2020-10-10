?exp
help.search( "logarithm" )
log10(10)
log2(10)
x <- sqrt(25) + 2
x
y = exp(2.5)
y
z = x*y
z
t = c(5,6,7)
t
s <- c(1, 2, 3, 4)
u = c(s,t)
u
?matrix
A <- matrix(c(1, 2, 2, 5), nrow = 2, ncol = 2, byrow = TRUE)
A
B = matrix(c(1, 2, 4, 2, 3, 5), nrow = 3, ncol = 2)  #default for byrow = False
B
#Method 1 fro getting a file directly from web:
music <- read.csv("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/music.csv")
music

music = 0 #To reset variable music
#Method 2, placing file in the folder in which the r file is located. (i.e. the directory)
music <- read.csv("music.csv")
music


