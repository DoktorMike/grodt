require(fgui)

EuropeanOption <- function (s, k, r, t, vol, CallOption) {
  d1 <- (log(s/k)+(r+0.5*vol^2)*t)/(vol*sqrt(t))
  d2 <- d1-vol*sqrt(t)
  if (CallOption){  
    return (s*pnorm(d1)-k*exp(-r*t)*pnorm(d2))
  } else {
    return (k*exp(-r*t)*pnorm(-d2)-s*pnorm(-d1))
  }
}

res <- gui(EuropeanOption, argOption=list(CallOption=c("TRUE","FALSE")))

