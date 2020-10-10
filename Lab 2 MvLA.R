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
