source('chartsGenerator.R')
source('taScreeners.R')
source('dataUtility.R')

mySymbols<-scan('SymbolsYahoo.csv', what=character())
#myData<-fetchData(mySymbols, 500)

# Convert data and interpolate missing values there could be missing values left but
# they will be removed in the individual charter
myxts<-as.xts(myData)
myxts<-apply(myxts, 2, interpolateNA)

#myData<-readSeries('data/data_1year_yahoo_symbols.csv', sep=',')

# Generate all charts from the symbols
generateCharts(mySymbols, myxts, path="charts")

# Screen all the stocks
myScreenV<-taScreenerValues(mySymbols, as.data.frame(myxts))
myScreenS<-taScreenerSignal(mySymbols, myData)

