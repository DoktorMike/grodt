require(fAssets)
require(quantmod)
require(ggplot2)
require(reshape2)

source('dataImportUtility.R')
source('dataUtility.R')

mySymbols<-scan('SymbolsYahoo.csv', what=character())

# Load the data from disk
#myData<-readSeries('data/omxs30_10Years.csv', sep=',')
# Load the data online
myData<-fetchData(mySymbols, 400)

# Use only Close prices and remove missing values
myClose<-removeNA2(myData)
myClose<-Cl(myClose)
colnames(myClose)<-gsub(".Close", "", colnames(myClose))
myxts<-as.xts(myClose)
myret<-returns(myxts)

# Ggplot
mydf<-melt(data.frame(myret))
ggplot(mydf, aes(y=value, factor(variable))) + 
  geom_boxplot() +
  coord_flip()

# Build a correlation tree and arrange the securities in that order and select the top 10
hc<-assetsSelect(myret)
myClose<-myClose[, assetsArrange(removeNA(myret), "hclust")]
mySelected<-myClose[, 1:10]
print(colnames(mySelected))
