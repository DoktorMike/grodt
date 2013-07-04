require(fImport)
require(fPortfolio)
require(quantmod)

source('dataImportUtility.R')

mySymbols<-c('^OMX')

# Load the data online or offline
myData<-fetchData(mySymbols, 3650)
#myData<-readSeries('data/omxs30_10Years.csv', sep=',')

# Get only the Close prices
myData<-Cl(myData)

myMonthlyReturns<-applySeries(returns(myData, "discrete", TRUE), by="monthly", FUN="colSums")
myQuarterlyReturns<-applySeries(returns(myData, "discrete", TRUE), by="quarterly", FUN="colSums")

# Add day of week and convert to data frame
myDf<-as.data.frame(returns(myData, "discrete", TRUE))
myDf<-cbind(myDf, Day=weekdays(as.Date(rownames(myDf))))
myDf<-cbind(myDf, Month=months(as.Date(rownames(myDf))))
myDf<-cbind(myDf, Quarter=quarters(as.Date(rownames(myDf))))

# Calculate the average return per weekday
dayStats<-stats::aggregate(myDf[,1], list(Day=myDf$Day), mean)
monthStats<-stats::aggregate(myDf[,1], list(Month=myDf$Month), mean)
quarterStats<-stats::aggregate(myDf[,1], list(Quarter=myDf$Quarter), mean)

