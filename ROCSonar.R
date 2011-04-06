# TODO: Add comment
# 
# Author: PatriciaHoffman
###############################################################################



library(MASS)
STrain <- read.table("sonar_train.csv",sep = ",",header = FALSE)
STest <- read.table("sonar_test.csv",sep = ",",header = FALSE)
Sonar <- rbind(STrain,STest)
Err <- matrix(nrow = 21, ncol = 3)
I <- seq(1:nrow(Sonar))

for(iLambda in seq(from = 0, to = 20)){
	#
	exp <- (+3 -4*(iLambda/20))
	xlambda <- 10^exp
	
	testErr <- 0.0
	trainErr <- 0.0
	
	for(ixval in seq(from = 1, to = 10)){
		Iout <- which(I%%10 == ixval - 1)
		SonarIn <- Sonar[-Iout,]
		SonarOut <- Sonar[Iout,]
		#Xin <- X[-Iout,]
		Xin <- SonarIn[,1:60]
		Xout <- SonarOut[,1:60]
		Yin <- SonarIn[,61]
		Yout <- SonarOut[,61]
		mod <- lm.ridge(V61~.,data=SonarIn,lambda=xlambda)
		C <- mod$coef/mod$scales
		XM <- Xin
		for(i in seq(from = 1, to = ncol(Xin))){
			XM[,i]<-Xin[,i]-mod$xm[i]
		} 
		X <- as.matrix(XM)
		A <- as.array(C)
		Yh <- X%*%A + mod$ym
		trainErr <- trainErr + sum(abs(Yin - Yh))/(nrow(as.matrix(Yin))*10)    
		XM <- Xout
		for(i in seq(from = 1, to = ncol(Xout))){
			XM[,i]<-Xout[,i]-mod$xm[i]
		} 
		X <- as.matrix(XM)
		A <- as.array(C)
		Yh <- X%*%A + mod$ym
		testErr <- testErr + sum(abs(Yout - Yh))/(nrow(as.matrix(Yout))*10)
	}
	Err[(iLambda+1),1] = trainErr
	Err[(iLambda+1),2] = testErr
	Err[(iLambda+1),3] = xlambda
}
plot(Err[,1], type='p', col='red', ylim=c(0,1),
		main = 'Error vs Log(Lambda)',
		ylab='Error',
		xlab='3 - Log(Lambda)')
points(Err[,1], pch=15, col='red')
#lines(out[,2], type='l', col='blue')
points(Err[,2], pch=16, col='blue')
legend(5, 0.2, c("TRAIN", "TEST"), cex = 1, col = c("red", "blue"),
		pch = c(15, 16), lty = 1:2)


#looks like the min is around 5 or 6 (for index) 

exp <- 6
exp <- (+3 -4*(iLambda/20))
xlambda <- 10^exp

Xin <- Sonar[,1:60]
Yin <- Sonar[,61]
mod <- lm.ridge(V61~.,data=Sonar,lambda=xlambda)
C <- mod$coef/mod$scales
XM <- Xin
for(i in seq(from = 1, to = ncol(Xin))){
	XM[,i]<-Xin[,i]-mod$xm[i]
} 
X <- as.matrix(XM)
A <- as.array(C)
Yh <- X%*%A + mod$ym
trainErr <- sum(abs(Yin - Yh))/nrow(as.matrix(Yin))    


YinP <- Yin >0
YinN <- Yin<0
YhP <- Yh>0
YhN<- Yh<0

##calculate misclassification error with threshold 0.0
sum(YinP == YhP)/length(Yin)


#Generate ROC curve
p <- sum(YinP)
n <- sum(YinN)
tp <- c(rep(0.0,100))
fp <- c(rep(0.0,100))

for(i in 1:100){
	thresh <- 2 - i*0.04
	y <- Yh>=thresh
	tp[i] <- sum(y & YinP)/p
	fp[i] <- sum(y & YinN)/n
	
}

plot(fp,tp)
abline(0,1)
