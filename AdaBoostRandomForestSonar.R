# TODO: Add comment
# 
# Author: Mike Bowles PatriciaHoffman
###############################################################################

library(rpart)
setwd("d:/home/idris/work/ml102")
train<-read.csv("sonar_train.csv",header=FALSE)
test<-read.csv("sonar_test.csv",header=FALSE)
y<-train[,61]
x<-train[,1:60]
y_test<-test[,61]
x_test<-test[,1:60]
train_error<-rep(0,500)
test_error<-rep(0,500)
f<-rep(0,130)
f_test<-rep(0,78)
i <- 1
iter <- 500

#create 500 trees
while(i<=iter){
  #print(c(toString(i),'in'))
  #e the -y*f
	w<-exp(-y*f)
	w<-w/sum(w)
	fit<-rpart(y~.,x,w,method="class", parms=list(split='gini'))
  #map value from 0..1 to -1..1
	g<--1+2*(predict(fit,x)[,2]>.5)
	g_test<--1+2*(predict(fit,x_test)[,2]>.5)
  # e is for error. A for effort.
	e<-sum(w*(y*g<0))
  
  # why multiply by .1?
	alpha<-.1*.5*log ( (1-e) / e )
	f<-f+alpha*g
	f_test<-f_test+alpha*g_test
	train_error[i]<-sum(1*f*y<0)/130
	test_error[i]<-sum(1*f_test*y_test<0)/78
	i<-i+1
}
plot(seq(1,500),test_error,type="l",
		main="AdaBoost Error Plot",
		ylim=c(0,.5),ylab="Error Rate",xlab="Iterations",lwd=2)
lines(train_error,lwd=2,col="purple")
#legend(4,.5,c("Training Error","Test Error"),
#		col=c("purple","black"),lwd=2)

#load randomForest package
library(randomForest)
train<-read.csv("sonar_train.csv",header=FALSE)
test<-read.csv("sonar_test.csv",header=FALSE)

y2<-as.factor(train[,61])
x<-train[,1:60]
y_test<-as.factor(test[,61])
x_test<-test[,1:60]
fit2<-randomForest(x,y)
1-sum(y2==predict(fit2,x))/length(y)

1-sum(y_test==predict(fit2,x_test))/length(y_test)
