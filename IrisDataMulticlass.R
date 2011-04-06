# TODO: Add comment
# 
# Author: Mike Bowles PatriciaHoffman
###############################################################################
setwd('d:/home/idris/work/ml102')
rm(list=ls())
data(iris)
attach(iris)
library("MASS")

iris <- read.table("irisdata.csv",sep = ",",header = FALSE)
#set Y1, Y2, and Y3 to negatives
Y1 <- c(rep(-1,150))
Y2 <- Y1
Y3 <- Y2
#set 1st, 2nd, and 3rd respective chunk to +1
Y1[1:50] <- 1
Y2[51:100] <- 1
Y3[101:150]<-1

#Use all iris EXCEPT for the 5th column
X <- iris[,-5]
I <- 1:150
Err <- matrix(nrow = 21, ncol = 3)

for(iLambda in seq(from = 0, to = 20)){
	#
	exp <- (+2 -4*(iLambda/20))
	xlambda <- 10^exp
	#cat(iLambda, exp, xlambda, '\n', sep='\t')
	testErr <- 0.0
	trainErr <- 0.0
	
	for(ixval in seq(from = 1, to = 10)){
		Iout <- which(I%%10 == ixval - 1)
		Xin <- X[-Iout,]
		Xout <- X[Iout,]
		Yin <- Y3[-Iout]
		Yout <- Y3[Iout]
	
		dataIn <- cbind(Xin,Yin)
    
    #fit a linear model by ridge regression
    # '~.' signifies function to rest of data
    #lm.ridge(formula, data, subset, na.action, lambda = 0, model = FALSE,
    #     x = FALSE, y = FALSE, contrasts = NULL, ...)
    
		mod <- lm.ridge(Yin~.,data=dataIn,lambda=xlambda)
		C <- mod$coef/mod$scales
		XM <- Xin
		for(i in seq(from = 1, to = ncol(Xin))){
			XM[,i]<-Xin[,i]-mod$xm[i]
		} 
		XX <- as.matrix(XM)
		A <- as.array(C)
		Yh <- XX%*%A + mod$ym
		YhP <- Yh>=0.0
		Yp <- Yin>= 0.0
		#trainErr <- trainErr + sum(abs(Yin - Yh))/(nrow(as.matrix(Yin))*10)    
		trainErr <- trainErr + sum(YhP != Yp)/(length(Yin)*10)
		XM <- Xout
		for(i in seq(from = 1, to = ncol(Xout))){
			XM[,i]<-Xout[,i]-mod$xm[i]
		} 
		XX <- as.matrix(XM)
		A <- as.array(C)
		Yh <- XX%*%A + mod$ym
		YhP <- Yh>=0.0
		Yp <- Yout>= 0.0
		testErr <- testErr + sum(YhP != Yp)/(length(Yout)*10)
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
#legend(5, 0.5, c("TRAIN", "TEST"), cex = 1, col = c("red", "blue"),
#		pch = c(15, 16), lty = 1:2)

#Fit all three and claculate misclassification error
xlambda <- 0.15
Xin <- X
Yin <- Y1
dataIn <- cbind(Xin,Yin)
mod <- lm.ridge(Yin~.,data=dataIn,lambda=xlambda)
C <- mod$coef/mod$scales
XM <- Xin
for(i in seq(from = 1, to = ncol(Xin))){
	XM[,i]<-Xin[,i]-mod$xm[i]
} 
XX <- as.matrix(XM)
A <- as.array(C)
Yh1 <- XX%*%A + mod$ym


Yin <- Y2
dataIn <- cbind(Xin,Yin)
mod <- lm.ridge(Yin~.,data=dataIn,lambda=xlambda)
C <- mod$coef/mod$scales
XM <- Xin
for(i in seq(from = 1, to = ncol(Xin))){
	XM[,i]<-Xin[,i]-mod$xm[i]
} 
XX <- as.matrix(XM)
A <- as.array(C)
Yh2 <- XX%*%A + mod$ym

Yin <- Y3
dataIn <- cbind(Xin,Yin)
mod <- lm.ridge(Yin~.,data=dataIn,lambda=xlambda)
C <- mod$coef/mod$scales
XM <- Xin
for(i in seq(from = 1, to = ncol(Xin))){
	XM[,i]<-Xin[,i]-mod$xm[i]
} 
XX <- as.matrix(XM)
A <- as.array(C)
Yh3 <- XX%*%A + mod$ym

#positive predictions by class
Y1h <- (Yh1 > Yh2) & (Yh1 > Yh3)
Y2h <-(Yh2> Yh1) & (Yh2>Yh3)
Y3h <- (Yh3> Yh2) & (Yh3>Yh1)

#number of correct positives in each class
sum(Y1>0 & Y1h)
sum(Y2>0 & Y2h)
sum(Y3>0 & Y3h)

