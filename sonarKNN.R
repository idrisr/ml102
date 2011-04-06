library(class)
train<-read.csv("sonar_train.csv", header=FALSE)
y<-as.factor(train[,61])
x<-train[,1:60]
fit<-knn(x,x,y,k=1)
1-sum(y==fit)/length(y)
test<-read.csv("sonar_test.csv", header=FALSE)
y_test<-as.factor(test[,61])
x_test<-test[,1:60]
fit_test<-knn(x,x_test,y,k=1)
1-sum(y_test==fit_test)/length(y_test) 