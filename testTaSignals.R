require('ttrTests')
require('quantmod')
require('timeSeries')

# Plot the financial time series and the buy and sell signals indicated by +1 and -1 respectively
plotSignal<-function(x, signal)
{
	#chartSeries(x, TA=c(addMACD(30,100,15),addBBands(),addRSI(21)))
	chartSeries(x, TA=c(addBBands(),addRSI(21),addMomentum(10)))
	abline(v=which(signal<0), col="red")
	abline(v=which(signal>0), col="green", lty="dotted")
}

generateRsiChart<-function(stock, interval=c(1:30))
{
	mylist<-list()
	for(i in interval){
		rets<-returns(stock, method="discrete")
		inds<-which(rsiSignal3(stock, params=c(21,30,70,i))>0)
		mylist<-c(mylist, list(rets[inds]))
	}
	mylist
}

source('taSignals.R')
source('dataImportUtility.R')

mydata<-read.table('data/omxs30_10Years.csv', sep=',', header=TRUE, row.names=1)
mydata<-as.timeSeries(mydata)
mystock<-mydata[, grep("LUPE",colnames(mydata))]
myclose<-removeNA(Cl(mystock))

stock<-myclose
stock<-as.xts(stock)
#plotSignal(stock, diff(rsiSignal3(stock, params=c(21,30,70,4))))
#plotSignal(stock, diff(rsiSignal2(stock, params=c(21,30,70,4))))
#plotSignal(stock, diff(macdSignal(stock, params=c(30,100,15))))
#plotSignal(stock, diff(smaSignal(stock, params=c(30,100))))
#plotSignal(stock, diff(bollingerSignal(stock, params=c(20,2))))
plotSignal(stock, diff(rsiWithMoneyManagement(stock, params=c(14,30,70))))

# Check statistical validity this function requires a vector or a timeSeries with "Close" attribute
#returnStats(as.vector(stock), ttr=rsiSignalWithMomentum, params=c(12, 30, 70, 12, 10))
returnStats(as.vector(stock), ttr=rsiWithMoneyManagement, params=c(14, 30, 70))

