music <- read.csv("https://www.scss.tcd.ie/~arwhite/Teaching/STU33011/music.csv")
dim(music)
music[1,1]
mTest=music[1,1]   #[i,j] i -> row
mTest
M2=music[c(1,3,4), c(2,3,4,5)]
M2
music_num=music[ ,c(4,5,6,7,8)]
music_num
cov(music[, 4:8])
cov(music[,7:8])
cor(music[,7:8])
?cor
plot(music[,7], music[,8], col = "red")
A = matrix(c(1,2,2,5), 2, 2)
A
res = eigen(A)
?eigen
res$values[1]
res$vectors[2,]
?"%*%" 
A %*% res$vector[,2]
res$value[2] * res$vector[,2]
B = matrix(c(17.7,20.3,20.3,24.4), 2, 2)
eigen(B)
diag(x=1, 2, 2)
