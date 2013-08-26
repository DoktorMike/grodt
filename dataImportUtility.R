require(fImport)

fixColnames<-function(mydat)
{
	colnames(mydat)<-gsub(".Adj.Close", ".Adjusted", colnames(mydat))
	mydat
}

# Fetch the data from yahooImport
fetchData<-function(stocks, ndays=365, freq="daily")
{
	myData<-c()
	for(symbol in stocks)
	{
		tmp<-0
		try(tmp<-yahooSeries(symbol, nDaysBack=ndays, frequency=freq))
		if(is.timeSeries(tmp)) myData<-cbind(myData, tmp)
		Sys.sleep(1)
	}
	myData<-fixColnames(myData)
	myData
}

fetchEuroInvestorData<-function(symbols)
{
	left<-"http://www.euroinvestor.dk/HistoricalQuotes/HistoricalQuotes.aspx?lang=DA&fn="
	right<-"&outputmode=5&format=csv&separator=;"
	for(i in symbols){
		mystr<-paste(left, i, right, sep="")
		outfile<-paste("EURO_", i, ".csv", sep="")
		download.file(mystr, outfile)
	}
}

readAndParseNordnetKeyFigures<-function(fname, numyears=5)
{
	myColClasses<-c("character", rep("numeric", numyears))
	a<-read.table(fname, sep="\t", dec=",", skip=0, header=TRUE, row.names=1, colClasses=myColClasses)
	egetKapital<-a[grep("Eget kapital", rownames(a)), ]
  egetKapitalSkulder<-a[grep("Summa eget kapital och skulder", rownames(a)), ]
	antalAktier<-a[grep("Antal aktier", rownames(a)), ]
	resultatPerAktie<-a[grep("Resultat per aktie", rownames(a)), ]
	substansVarde<-egetKapital/antalAktier*1000
  #browser()
	tillvaxt<-cbind((resultatPerAktie[1,-numyears]-resultatPerAktie[1,-1])/abs(resultatPerAktie[1,-1]), NA)
	pe10pris<-resultatPerAktie*10
  soliditet<-egetKapital/egetKapitalSkulder
	colnames(tillvaxt)<-colnames(a)
	ret<-rbind(ResultatPerAktie=resultatPerAktie, 
             SubstansVarde=substansVarde, 
             Tillvaxt=tillvaxt, 
             PE_10_Pris=pe10pris,
             Soliditet=soliditet)
	ret<-round(ret, 2)
	ret
}

readAndParseNordnetKeyFiguresDk<-function(fname, numyears=5)
{
  myColClasses<-c("character", rep("numeric", numyears))
  a<-read.table(fname, sep="\t", dec=",", skip=0, header=TRUE, row.names=1, colClasses=myColClasses)
  egetKapital<-a[grep("Egenkapital", rownames(a))[1], ]
  egetKapitalSkulder<-a[grep("Egenkapital og gæld i alt", rownames(a)), ]
  antalAktier<-a[grep("Antal aktier", rownames(a)), ]
  resultatPerAktie<-a[grep("Resultat pr. aktie", rownames(a)), ]
  substansVarde<-egetKapital/antalAktier*1000
  tillvaxt<-cbind( (resultatPerAktie[1,-numyears]-resultatPerAktie[1,-1])/abs(resultatPerAktie[1,-1]), NA)
  pe10pris<-resultatPerAktie*10
  soliditet<-egetKapital/egetKapitalSkulder
  colnames(tillvaxt)<-colnames(a)
  #a<-rbind(a, SubstansVärde=substansVarde)
  ret<-rbind(ResultatPerAktie=resultatPerAktie, 
             SubstansVarde=substansVarde, 
             Tillvaxt=tillvaxt, 
             PE_10_Pris=pe10pris,
             Soliditet=soliditet)
  ret<-round(ret, 2)
  ret
}