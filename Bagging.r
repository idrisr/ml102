# TODO: Brimean Bagging Algorithm
# 
# Author: Abhishek Dubey
###############################################################################
require(rpart)

rm(list=ls())
setwd("D:\\MachineLearning\\DataSets")
data = read.delim("parkinsons.data.txt", sep=",", header = TRUE)


# function to do 10 fold cross-validation on parkinsons dataset and 
# and returns the model
crossValidate <- function(dataset) {
	
	nxval <- 10
	out <- matrix(nrow = nxval, ncol = 2)
	I <- seq(from = 1, to = nrow(dataset))
	
	trainErr <- 0.0
	testErr <- 0.0
	for(ixval in seq(from =  1, to = nxval)){
		Iout <- which(I%%nxval == ixval%%nxval)
		trainIn <- dataset[-Iout,]
		trainOut <- dataset[Iout,]
		yin <- trainIn[,18]
		yout <- trainOut[,18]
		xin <- trainIn[,-18]
		xout <- trainOut[,-18]
		
		mod <- rpart(status~., data=trainIn, method = "anova" )
		tr <- sum(abs(yin - predict(mod,xin)))/(nrow(as.matrix(yin)))    
		te<-sum(abs(yout - predict(mod,xout)))/(nrow(as.matrix(yout)))
		
		trainErr <- trainErr + tr/nxval
		testErr <- testErr + te/nxval
		
		out[ixval,1] <- tr
		out[ixval,2] <- te
	}
	return (mod)
}


# Actual Bootstrapping code
trainErr <- 0.0
testErr <- 0.0
dataSize = nrow(data)
bootCount <- 50
iRepeat <- 100
sampleSize <- round(0.9 * nrow(data))

j <- 0
errTable <- out <- matrix(nrow = iRepeat, ncol = 2)

while (j < iRepeat) {
	
	#get new sample set
	dataIndex <- sample(dataSize, sampleSize, replace = FALSE )
	
	# trainSet set is L
	trainSet <- data[dataIndex,]
	testSet <- data[-dataIndex,]
	
	# do vanilla cross validation and get model  
	mod <- crossValidate(trainSet)
	
	#get final prediction on the inital test set
	txout <- testSet[, -18]
	tyout <- testSet$status
	
	# now use this model to do predictions on the actual test set
	# this will be used as the baseline
	opred <- predict(mod,testSet)
	
	#calculate err
	oerr <-  sum(abs(tyout - opred))/(nrow(as.matrix(tyout)))
		 
	#array to store results of 25 predictors
	results <- array (dim = c(dataSize - sampleSize, bootCount))
	
	i <- 0
	while(i <= bootCount){
		
		# genearting bootstrapping sample set. btpTrain is Lb
		btpTrainIndex <- sample(nrow(trainSet), replace = TRUE)
		btpData <- trainSet[btpTrainIndex,]
		bFit <- rpart(status~., btpData, method = "anova" )
		
		xin <- btpData[,-18]
		xout <- trainSet[,-18]
		
		yin <- btpData$status
		yout <- trainSet$status
		
		#calculate training error and test error
		tr <- sum(abs(yin - predict(bFit,xin)))/(nrow(as.matrix(yin)))
		
		#use L as the testing set
		te<-sum(abs(yout - predict(bFit,xout)))/(nrow(as.matrix(yout)))
				
		# get cp for the model with lowest error
		# this cp will be used to prune the tree
		cp = bFit$cptable[which.min(bFit$cptable[,"xerror"])]
		
		#now prune tree using L
		pbFit <- prune(bFit, cp=cp)
				
		# get test error for each model
		finalPred <- predict(pbFit,txout)
		fte <-sum(abs(tyout - finalPred))/(nrow(as.matrix(tyout)))
		out[j,1]= fte
		
		#store predictions for voting
		results [,i] = finalPred
		i <- i+1
	}
	
	#calculate votes for each class
	predictedClass <- matrix(nrow = nrow(results), ncol = 1)
	for(ii in seq(from =  1, to = nrow(results))){
		class1 <- 0
		class0 <- 0
		for(jj in seq(from =  1, to = bootCount)){
			
			# since we have only two classes
			if(results[ii,jj] == 1){
				class1 <- class1 +1
			}else {
				class0 <- class0 +1
			}
		}
		if(class1 > class0){
			predictedClass[ii] <- 1
		}else if (class0 > class1) {
			predictedClass[ii] <- 0
		}else if (class0 == class1){
			# incase of a tie. Store the class with lowest label
			# brieman et.al.
			predictedClass[ii] <- 0		
		}
	}
	
	#display confusion matrix
	table(predictedClass,tyout)
	
	#calcualte bootstrapping error rate 
	berr <-sum(abs(tyout - predictedClass))/(nrow(as.matrix(tyout)))
	
	j <- j+1
	cat("Iteration:",j ,"\n")
	
	#store the errors for vanilla model and model using bootstrap
	errTable[j, 1] <- berr
	errTable[j, 2] <- oerr
}

#plot graph
plot(seq(1,iRepeat),errTable[,1],type="l",
		main="Bagging vs Base",
		ylab="Error Rate", ylim=c(0,1),xlab="Iterations",lwd=2, col="blue")
lines(errTable[,2],lwd=2,col="green")
legend(70,0.8,c("Bagging","Base"),
		col=c("blue","green"),lwd=2)

#some metrics
# counter to save number of time base model and bagging did same
constCount <- 0
# counter to save number of time base model did better than bagging
baseCount <- 0 
# counter to save number of time bagging did better than base model
baggCount <- 0
for(ii in seq(from =  1, to = nrow(errTable))){
	if (errTable[ii,1] < errTable[ii,2]) {
		baggCount<- baggCount +1
	} else if (errTable[ii,1] > errTable[ii,2]) {
		baseCount<- baseCount +1
	}else {
		constCount <- constCount +1
	} 
}
cat("Percenage when Bagging did better:",baggCount/iRepeat*100,"%\n")
cat("Percenage when base model did better:",baseCount/iRepeat*100,"%\n")
cat("Percentage when base model was equal to bagging:",constCount/iRepeat*100,"%\n")

