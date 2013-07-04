library(quantmod)
source('dataImportUtility.R')

getSupportResistanceLines<-function(stock, howmany=3)
{
	a<-hist(stock, plot=FALSE)
	prices<-a$mids[order(a$counts)]
	n<-length(prices)
	if(howmany > n) howmany<-n
	ret<-prices[(n-howmany+1):n]
	ret
}

identifyLocalMinima<-function(stock, lookBack)
{
	inds<-c()
	from<-lookBack+1
	to<-length(stock)-lookBack-1
	for(i in from:to) { 
		if(all(stock[(i-lookBack):(i-1)] > stock[i]) && all(stock[(i+1):(i+lookBack)] > stock[i]))
			inds<-c(inds, i)
	}
	inds
}

identifyLocalMaxima<-function(stock, lookBack)
{
	inds<-c()
	from<-lookBack+1
	to<-length(stock)-lookBack-1
	for(i in from:to) { 
		if(all(stock[(i-lookBack):(i-1)] < stock[i]) && all(stock[(i+1):(i+lookBack)] < stock[i]))
			inds<-c(inds, i)
	}
	inds
}

# Estimate the upper or lower trend line in the trend channel
estimateTrendLine<-function(stock, lookBack, type="upper")
{
	x<-c()
	if(type=="upper") x<-identifyLocalMaxima(stock, lookBack)
	else x<-identifyLocalMinima(stock, lookBack)
	y<-stock[x]
	lm(Y~X, data=data.frame(Y=y, X=x))
}

getSupportResistanceLinesV2<-function(stock, lookBack=4, n=2)
{
	#mins<-kmeans(stock[identifyLocalMinima(stock, lookBack)], n)$centers
	#maxs<-kmeans(stock[identifyLocalMaxima(stock, lookBack)], n)$centers
	mins<-stock[identifyLocalMinima(stock, lookBack)]
	maxs<-stock[identifyLocalMaxima(stock, lookBack)]
	ret<-kmeans(union(mins, maxs), n)$centers
	
	#ret<-union(mins, maxs)
	ret
}

addVLine = function(dtlist)
{
	plot(addTA(xts( rep(TRUE, NROW(dtlist)), dtlist), on=1, col="#333333"))
}

# Does not work
addHLine = function(plist)
{
	plot(addTA(xts(plist, rep(TRUE, NROW(plist))), on=1, col="#333333"))
}

addTrendLine = function(stock, coefs)
{
	intercept<-coefs[1]
	slope<-coefs[2]
	x<-1:length(stock)
	lines(x, intercept+slope*x, col='red')
}

# A lookback value of 10 seems to work ok for 1 year data
# Figure out why this does not work with OHLC data..
plotChartAndTrendLines = function(stock, lookBack)
{
	chartSeries(stock)
	stock<-Cl(stock)
	upTl<-estimateTrendLine(stock, lookBack, "upper")
	loTl<-estimateTrendLine(stock, lookBack, "lower")
	addTrendLine(stock, coef(upTl))
	addTrendLine(stock, coef(loTl))
	#abline(v=identifyLocalMaxima(stock, 10), col='red')
	#abline(v=identifyLocalMinima(stock, 10), col='blue')
}

stockOhlc<-fetchData('MEKO.ST', 500)
stock<-Cl(stockOhlc)
#chartSeries(stockOhlc, log.scale=TRUE)
#abline(h=getSupportResistanceLines(stock, 4), col='red')


