require(quantmod)
require(fImport)

source('dataUtility.R')

#myohlc<-fetchData('ABB.ST', 3000)
myret<-returns(Cl(myohlc))
myret[myret>0]<-1
myret[myret<0]<-0
myretdf<-createTimeLaggedDataSet(myret, c(1:12))
myretdf<-removeNA(myretdf)
# Transform response variable to 0:1 for down day and up day!
myretdf$Y[myretdf$Y>0]<-1
myretdf$Y[myretdf$Y<0]<-0
# Turn all variables in the data frame to factors
#myretdf<-data.frame(lapply(myretdf, factor))
nnetfet<-nnet(Y~., data=myretdf, size=15, maxit=1000)
monkey<-data.frame(Target=myretdf$Y, Pred=predict(nnetfet))

