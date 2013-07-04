require(quantmod)
require(nnet)

source('dataImportUtility.R')
source('dataUtility.R')

createFakeDataSet<-function(initvals, betas)
{
	ret<-initvals
	for(i in 1:400){
		ret<-c(ret, ret[i:(i+length(betas)-1)] %*% betas)
	}
	ret
}

#1 2 3 4 5 6 7
#  2 3 4 5 6 7 8
#    3 4 5 6 7 8 9
 
# predict rolling
iteratedPredict<-function(model, obs, lags)
{
	nstart<-max(lags)+1
	pred<-obs
	pred[nstart:length(pred)]<-0
	for(i in nstart:length(pred)){
		mydf<-data.frame(rbind(pred[c(i-lags)]))
		colnames(mydf)<-paste("X", lags, sep="")
		pred[i]<-predict(model, newdata=mydf)
	}
	#browser()
	mydf<-data.frame(Target=as.numeric(obs), Predicted=as.numeric(pred))
	mydf
}

plotPredicted<-function(df)
{
	plot(df$Target, type='l', ylab="Aktiekurs", xlab="Dag")
	lines(df$Predicted, type='l', col='red')
}

predictIt<-function(x, lags, coefs)
{
	stopifnot(max(lags)<length(x))
	b<-numeric(400+length(x)+1)
	b[1:length(x)]<-x
	for(i in (length(x)+1):length(b)){
		b[i]<-c(1,b[i-lags])%*%coefs
	}
	b
}

# Fetch and process data
myData<-fetchData('jm.st', 1000)
myCl<-Cl(myData)
testlags<-c(1:20)
myDataset<-createTimeLaggedDataSet(myCl, testlags)

# Build a model
trainInds<-c(1:200)
testInds<-c(1:dim(myDataset)[1])[-trainInds]
lmfit<-lm(Y ~ ., data=myDataset, subset=trainInds)
lmfit2<-step(lmfit)

# Fitted vs. Predicted
lmFitDf<-data.frame(Target=myDataset[trainInds, 1], Predicted=predict(lmfit2))
lmFitSingDf<-data.frame(Target=myDataset[, 1], Predicted=predict(lmfit2, newdata=myDataset))
lmFitIteratedDf<-iteratedPredict(lmfit2, as.vector(myCl), testlags)

# Neural network
myScaled<-scale(myDataset)
nnetfit<-nnet(Y~., data=myScaled, size=5, linout=TRUE, subset=trainInds, decay=0.1)
nnFitDf<-data.frame(Target=as.data.frame(myScaled)[trainInds, 1], Predicted=predict(nnetfit))
nnFitSingDf<-data.frame(Target=as.data.frame(myScaled)[, 1], Predicted=predict(nnetfit, newdata=myScaled))
nnFitIteratedDf<-iteratedPredict(nnetfit, as.data.frame(myScaled)[testInds, 1], testlags)
