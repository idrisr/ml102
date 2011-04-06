# TODO: Add comment
# 
# Author: Mike Bowles PatriciaHoffman
###############################################################################


y <- c(1,1,-1,-1,1,1,-1,-1,1,-1)
m1 <- c(0.73,0.69,0.44,0.55,0.67,0.47,0.08,0.15,0.45,0.35)
m2 <- c(0.61,0.03,0.68,0.31,0.45,0.09,0.38,0.05,0.01,0.04)
tp1 <- c(rep(0.0,100))
fp1 <- c(rep(0.0,100))
tp2 <- c(rep(0.0,100))
fp2 <- c(rep(0.0,100))

p <- sum(y == 1)
n <- sum(y == -1)
yp <- y==1
yn <- y==-1

for(i in 1:100){
	thresh <- 1 - i*0.01
	y1 <- m1>=thresh
	y2 <- m2<=thresh
	tp1[i] <- sum(y1 & yp)/sum(yp)
	fp1[i] <- sum(y1 & yn)/sum(yn)
	tp2[i] <- sum(y2 & yp)/sum(yp)
	fp2[i] <- sum(y2 & yn)/sum(yn)
	
}

plot(fp1,tp1)
abline(0,1)


plot(fp2,tp2)
abline(0,1)



