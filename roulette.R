

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

simulateIt<-function(len=3, startMoney=1000, betBase=100, stopProfitLevel=2000, debug=FALSE)
{
  money<-startMoney
  cntr<-1
  lastVal<-sample(c(-1,1), 1)
  bet<-0
  looseCnt<-0
  iterations<-0
  while(money>0 && money < stopProfitLevel){
    iterations<-iterations+1
    outcome<-sample(c(-1,1), 1)
    if(bet > 0){ # I have a bet
      if(outcome == lastVal){
        money<-money-bet
        looseCnt<-looseCnt+1
      }else{
        money<-money+bet
        looseCnt<-0
      }
      bet<-0
    }
    cntr<-ifelse(outcome == lastVal, cntr+1, 1)
    if(cntr > len){
      bet<-betBase*(looseCnt+1)
    }
    if(debug)
      print(paste("Last val: ", lastVal, 
                  ", Val: ", outcome, 
                  ", Bet: ", bet, 
                  ", Money: ", money, sep=""))
    lastVal<-outcome
  }
  if(debug) print(paste("Iterations: ", iterations))
  bankrupt<-ifelse(money>0, FALSE, TRUE)
  ret<-list(Money=money, Iterations=iterations, Bankrupt=bankrupt)
  ret
}

simulateMany<-function(n=100)
{
  ret<-rep(TRUE, n)
  for(i in 1:n){
    a<-simulateIt(len=3, startMoney=1000, stopProfitLevel=2000, betBase=200)
    ret[i]<-a$Bankrupt
  }
  ret
}

plotStatistics <- function () {
  # Generate game and measure repeat frequency
  game<-sample(c(-1,1), 10000, replace=TRUE)
  distr<-findDownUpDistributions(game)
  
  # Plot distribution
  plotParetoChart(table(distr$Neg), main="Roulette #same colour in a row")
}