source('dataImportUtility.R')

mySymbols<-scan('OMXS30SymbolsYahoo.csv', what=character())
#myData<-fetchData(mySymbols, 360)
mySymbols<-gsub(".ST", "", mySymbols)

maxSpreads<-LoHi(as.xts(myData[, grep(mySymbols[1], colnames(myData))]))
colnames(maxSpreads)<-mySymbols[1]
for(symbol in mySymbols[2:length(mySymbols)]){
	#boxplot(as.vector(LoHi(as.xts(myData[, grep(symbol, colnames(myData))]))))
	#scan()
	ms<-try(LoHi(as.xts(myData[, grep(symbol, colnames(myData))])), TRUE)
	if(class(ms) != "try-error"){
		colnames(ms)<-symbol
		maxSpreads<-merge(maxSpreads, ms)
	}
}

barplot(apply(removeNA(maxSpreads), 2, mean), las=2)
