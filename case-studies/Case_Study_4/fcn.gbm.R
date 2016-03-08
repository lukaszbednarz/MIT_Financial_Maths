fcn.itsreturns <- function(input) {
  out<- diff(log(input))[-1]
  out<-merge(out, diff.Date(time(input)))
  out<-merge(out, xts(as.Date(time(input)[-1]) , time(input)[-1]))
  out<-merge(out, out[,3]-out[,2])
  out<-merge(out, xts(as.POSIXlt(time(input)[-1])$wday + 1, time(input)[-1]))
  dimnames(out)[[2]]<-c("ret", "ndays", "date.end", "date.start", "dayofweek")
  return(out)
}


fcn.gbm.compare<- function(input, sub = "FX", end.time=tail(time(input), n=1)) {
  print("Geometric Brownian Motion Model");
  
  data<-input[time(input) <= end.time]
  
  print("Parameter Estimates under Scale 1 (Trading Days)");
  
  n<- NROW(data)
  mu<- 1/n*sum(data$ret)
  Sigma<- 1/(n)*sum((data$ret - mu)^2)
  
  print(n)
  
  print(cat("mu =", mu+0.5*Sigma, "Sigma =", sqrt(253*Sigma)))
  
  q<-seq(0.0,1,0.01)
  b<-qnorm(q,mu, sqrt(Sigma))
  
  print("Plotting Quantiles")
  h<-hist(data$ret, 
          breaks=b,
          plot=FALSE)
  par(mfcol=c(2,1))
  barplot(names.arg=q[-1], h$counts, main = "Trading Days")
  abline(sum(a=h$counts)/100,b=0)
  
  print("Parameter Estimates under Scale 2 (Clock Time)")
  
  
  mu<- 1/(sum(data$ndays))*sum(data$ret)
  Sigma<- 1/(n)*sum(((data$ret - mu*data$ndays)^2)/data$ndays)
  
  print(cat("mu =", mu+0.5*Sigma, "Sigma =", sqrt(253*Sigma)))
  
  b<-qnorm(q,mu, sqrt(Sigma))
  
  print("Plotting Quantiles")
  h<-hist(data$ret, 
          breaks=b,
          plot=FALSE)
  
  barplot(names.arg=q[-1], h$counts, main = "Calendar Days")
  abline(sum(a=h$counts)/100,b=0)
  par(mfcol=c(1,1))
}

fcn.lag0<-function(y, lag=1) {
  
  return(lag(y, k=lag))
}