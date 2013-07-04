require(quantmod)
require(fImport)

source('dataImportUtility.R')

# Load the data online
myts<-yahooSeries('^OMX', from="2005-01-01", to="2010-12-31")
myts<-fixColnames(myts)
myxts<-as.xts(myts)

# Convert to monthly and quarterly
myMonthly<-Cl(to.monthly(myxts))
myQuarterly<-Cl(to.quarterly(myxts))

# Decompose into trend, seasonality and irregular
stlMonthly<-stl(ts(as.vector(myMonthly), start=c(2005, 01), freq=12), s.window="per")
stlQuarterly<-stl(ts(as.vector(myQuarterly), start=c(2005, 1), freq=4), s.window="per")

# Get monthly error bars
myMonthlyDf<-data.frame(Mean=numeric(12), Upper=numeric(12), Lower=numeric(12), Percent=numeric(12))
for(i in 1:12) { 
	dat<-myMonthly[seq(i,6*12,12)]; 
	er<-1.96*sd(dat)/sqrt(6); 
	myMonthlyDf[i, ]<-c(mean(dat), mean(dat)+er, mean(dat)-er, er/mean(dat))
}

# Get quarterly error bars
myQuarterlyDf<-data.frame(Mean=numeric(4), Upper=numeric(4), Lower=numeric(4), Percent=numeric(4))
for(i in 1:4) { 
	dat<-myQuarterly[seq(i,6*4,4)]; 
	er<-1.96*sd(dat)/sqrt(6); 
	myQuarterlyDf[i, ]<-c(mean(dat), mean(dat)+er, mean(dat)-er, er/mean(dat))
}

require(gplots)

# Plot the monthly seasonality
png(file="Omxs30 monthly seasonality.png")
a<-stlMonthly[[1]][, "seasonal"][1:12]
perc<-as.vector(myMonthlyDf$Percent)
barplot2(a, name=month.name, las=2, main="Monthly Seasonality of the OMXS30 Index", ylab="Index value", plot.ci=TRUE, ci.u=a+perc*a, ci.l=a-perc*a)
dev.off()

png(file="Omxs30 quarterly seasonality.png")
a<-stlQuarterly[[1]][, "seasonal"][1:4]
perc<-as.vector(myQuarterlyDf$Percent)
barplot2(a, name=c("Q1", "Q2", "Q3", "Q4"), main="Quarterly Seasonality of the OMXS30 Index", ylab="Index value", plot.ci=TRUE, ci.u=a+perc*a, ci.l=a-perc*a)
dev.off()
