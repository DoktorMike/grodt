require(ttrTests)

# All of these TTR functions should return positions, i.e., return 1 as long as we are on a buy signal. Not just when the signal is issued.

smaSignal<-function(x, params=c(30,100), burn=0, short=FALSE)
{
	ret<-rep(0, length(x))
	d<-SMA(x, n=params[1])-SMA(x, n=params[2])
	d<-ifelse(d<0,0,1)
	d[is.na(d)]<-0
	ret<-d
	ret
}

rsiSignal<-function(x, params=c(21, 30, 70), burn=0, short=FALSE)
{
	stock<-x[!is.na(x)]
	ret<-rep(0, length(stock))
	overSold<-params[2]
	overBought<-params[3]
	y<-RSI(stock, params[1])
	y[is.na(y)]<-50
	ret[y<overSold]<-1
	ret[y>overBought]<-ifelse(short==FALSE, 0, -1)
	ret
}

# Experimental rsi testing if buying at 30 and keeping for a few days works
rsiSignal2<-function(x, params=c(21, 30, 70, 7), burn=0, short=FALSE)
{
	ret<-rep(0, length(x))
	overSold<-params[2]
	overBought<-params[3]
	y<-RSI(x, params[1])
	y[is.na(y)]<-50
	ret[y<overSold]<-1
	ret[y>overBought]<-0
	inds<-which(ret>0)
	for(i in inds) ret[seq(i,(i+params[4]))]<-1
	ret
}

# Experimental rsi testing if buying on the way up from 30 and selling on the way down from 70
# A position is hold for no more than the fourth parameter days
rsiSignal3<-function(x, params=c(21, 30, 70, 4), burn=0, short=FALSE)
{
	ret<-rep(0, length(x))
	overSold<-params[2]
	overBought<-params[3]
	numDaysKeep<-params[4]
	y<-RSI(x, params[1])
	y[is.na(y)]<-50
	same<-0
	for(i in 2:length(y)){
		if(same <= numDaysKeep){
			ret[i]<-ret[i-1]
			same<-same+1
		}
		else same<-0
		if(y[i-1] >= overBought){
			if(y[i] < overBought) ret[i] <- -1
		}else if(y[i-1]<= overSold){
			if(y[i] > overSold) ret[i] <- 1
		}
	}
	if(!short) ret[which(ret<0)]<-0
	ret
}

# This tries to combine the rsi with the momentum. If a buy signal is issued in momentum 
# check backwards n steps to see if an rsi buy signal has already been issued. If so a buy signal is issued
rsiSignalWithMomentum<-function(x, params=c(12, 30, 70, 12, 10), burn=0, short=FALSE)
{
	lagParam<-params[5]
	momParam<-params[4]
	rsiParams<-params[1:3]
	
	momIndicator<-momentumSignal(x, momParam, burn, short)
	rsiIndicator<-rsiSignal(x, rsiParams, burn, short)
	resIndicator<-rep(0, length(momIndicator))
	
	# Sanity check
	stopifnot(length(momIndicator) == length(rsiIndicator))
	
	currentSignal<-factor("Neutral", levels=c("Buy", "Sell", "Neutral"))
	
	i<-1
	while(i<=length(rsiIndicator))
	{
		startInd<-ifelse(i-lagParam > 0, i-lagParam, 1)
		if(momIndicator[i]>0){
			if(currentSignal=="Buy"){
				resIndicator[i]<-1
			}else if(any(rsiIndicator[startInd:i] > 0)){
				resIndicator[i]<-1
				currentSignal[1]<-"Buy"
			}
		}else{
			if(currentSignal=="Sell"){
				resIndicator[i]<-0
			}else if(any(rsiIndicator[startInd:i] <= 0)){
				resIndicator[i]<-0
				currentSignal<-"Sell"
			}
		}
		i<-i+1
	}
	
	resIndicator
}

macdSignal<-function(x, params=c(12, 26, 9), burn=0, short=FALSE)
{
	ret<-rep(0, length(x))
	y<-MACD(as.vector(x), params[1], params[2], params[3])
	y<-y[, "macd"]-y[, "signal"]
	d<-ifelse(y<0,0,1)
	d[is.na(d)]<-0
	ret<-d
	ret
}

bollingerSignal<-function(x, params=c(20, 2), burn=0, short=FALSE)
{
	ret<-rep(0, length(x))
	y<-BBands(x, n=params[1], sd=params[2])
	ret[x>y[,"up"]]<-0
	ret[x<y[,"dn"]]<-1
	ret
}

momentumSignal<-function(x, params=c(10), burn=0, short=FALSE)
{
	ret<-rep(0, length(x))
	y<-momentum(x, params[1])
	ret[y>0]<-1
	ret[y<0]<-ifelse(short==FALSE,0,-1)
	ret
}

# Theck the stop loss function. Works ok but I should probably use a trailing StopLoss instead of a stopGain?
rsiWithMoneyManagement<-function(x, params=c(21, 30, 70), burn=0, short=FALSE)
{
	stock<-x[!is.na(x)]
	ret<-rep(0, length(stock))
	
	#Buy/Sell signals (could in principle be modular)
	mysignals<-rsiSignal(x, params)
	stopifnot(length(mysignals)==length(stock))
	myrsi<-RSI(stock, params[1])
	myrsi[is.na(myrsi)]<-50
	
	# Position status
	havePosition<-FALSE
	entryPrice<--1 # The entry price for the trade
	stopLoss<--1 # The Stop Loss price
	stopGain<--1 # The Stop Profit price
	delayCntr<-0 # Used to stop buy signal within 10 days of a stop loss
	
	for(i in 1:length(ret)){
		if(delayCntr > 0){
			ret[i]<-0
			delayCntr<-delayCntr-1
		}else if(havePosition){
			ret[i]<-1
			if(any(stock[i] < stopLoss, stock[i] > stopGain, myrsi[i] >= 70)){
				havePosition<-FALSE
				delayCntr<-10
			}
		}else{
			ret[i]<-0
			if(myrsi[i] <= 30){
				entryPrice<-as.numeric(stock[i])
				stopLoss<-entryPrice*0.97
				stopGain<-entryPrice*1.20
				havePosition<-TRUE
			}
			#cat("Buying\n")
			#browser()	
		}
		#if(i>390) browser()
	}
	ret
}
