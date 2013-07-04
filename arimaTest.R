armodel<-(arima(myCl[1:200], order=c(1,1,1)))
a<-(predict(armodel, 55))
plot(myCl[1:255], type="l")
lines(a$pred, col="blue")

