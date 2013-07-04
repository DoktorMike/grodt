source('dataImportUtility.R')
require(quantmod)

# Generates the charts I typically want to see
generateChart<-function(stock, fname)
{
  print(fname)
	stock<-removeNA(stock)
  #browser()
  if(!is.null(dim(stock)) && dim(stock)[1] > 100){
  	png(paste(fname, ".png", sep=""), width=1280, height=600, pointsize=19)
  	#try(chartSeries(stock, name=fname, TA=c(addBBands(), addRSI(21), addMFI(21), addROC(10), addWMA(30), addWMA(100), addOBV(), addTDI())))
  	chartSeries(stock, name=fname, TA=c(
      addBBands(), 
      addRSI(21), 
      addMFI(21), 
      addROC(10), 
  		addWMA(50, wts=rep(1, 50)), 
      addWMA(100, wts=rep(1, 100)), 
      addVo()))
  	dev.off()
  }
}

generateCharts<-function(stocks, mydat, path='charts')
{
	# Delete old charts
	if(file.exists(path)) unlink(path, recursive=TRUE)
	dir.create(path)
	# Generate new charts
	for(stockName in stocks){
    #browser()
		cols<-grep(stockName, colnames(mydat))
		generateChart(mydat[, cols], paste(path, "/", stockName, sep=""))
	}
}
