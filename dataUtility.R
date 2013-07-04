getStock<-function(name, data)
{
  ret<-data[,grep(name, colnames(data))]
  ret
}

# Finds and stores the distribution of the number of consecutive ups and downs over the x period
# x should typically be a log return series of a stock
findDownUpDistributions<-function(x)
{
	pos<-numeric(0)
	neg<-numeric(0)
	cnt<-1
	for(i in 2:length(x)){
		xp<-x[i-1]
		xn<-x[i]
		if(xn*xp < 0){ #different signs
			if(xp < 0) neg<-c(neg, cnt)
			else pos<-c(pos, cnt)
			cnt<-1
		}else
			cnt<-cnt+1
	}
	if(x[length(x)] < 0) neg<-c(neg, cnt)
	else pos<-c(pos, cnt)
	list(Pos=pos, Neg=neg)
}


# Find all NA and do a linear interpolation of them
interpolateNA<-function(x)
{
	#stopifnot(any(is.data.frame(x), is.xts(x), is.matrix(x)))
	inds<-which(is.na(x))
	if(length(inds)>0) try({x[inds]<-approx(x, xout=inds)$y})
	x
}

# This removes columns if all rows in that column is NA. It removes a row if any column in that row is NA.
removeNA2<-function(data)
{
	x<-data
  if(is.vector(x)){
    inds<-is.na(x)
    if(all(inds)) return(NULL)
    return(x[!inds])
  }
	na.col<-apply(is.na(x), 2, all)
  if(all(na.col)) return(NULL)
	if(length(na.col) > 0) x<-x[, !na.col]
	na.row<-apply(is.na(x), 1, any)
	if(length(na.row) > 0) x<-x[!na.row, ]
	x
}

cleanUpMatrix<-function(x)
{
  if(!is.matrix(x) && !is.data.frame(x)) stop("You need to supply a data.frame or a matrix!")
  removeOrNot<-apply(x, 2, function(z) if(length(which(is.na(z)))/length(z) > 0.1) 1 else 0)
  ret<-x[, removeOrNot]
  ret
}

# Create a time lagged dataset for autoregressive prediction
createTimeLaggedDataSet<-function(x, lags)
{
	mydf<-data.frame(lag(x, k=c(0,lags)))
	colnames(mydf)<-c("Y", paste("X", lags, sep=""))
	mydf<-mydf[-lags, ]
	mydf
}

# 