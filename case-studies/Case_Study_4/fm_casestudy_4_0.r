# 0.1 Install/load libraries
setwd("C:/Users/lbednarz/Documents/Learning/MIT_Financial_Maths/case-studies/Case_Study_4")
source("fm_casestudy_0_InstallOrLoadLibraries.r")
library("zoo")

# 0.2 Load R workspace created by script fm_casestudy_fx_1.r
load(file="fm_casestudy_fx_1.RData")
#print(dbnames0)

# 1.0 Extract time series matrix of exchange rates for symbols givenby list.symbol0
list.symbol0<-c("DEXCHUS", "DEXJPUS", "DEXKOUS", "DEXMAUS", 
                "DEXUSEU", "DEXUSUK", "DEXTHUS", "DEXSZUS")


fxrates000<-fred.fxrates.00[,list.symbol0]  

dim(fxrates000)

head(fxrates000)

tail(fxrates000)


# Print symbol/description/units of these rates from data frame fred.fxrates.doc

options(width=120)
print(fred.fxrates.doc[match(list.symbol0, fred.fxrates.doc$symbol),
                          c("symbol0", "fx.desc", "fx.units")])


source("fcn.gbm.R")

fx.USEU<-fxrates000[,"DEXUSEU"]
fx.USUK<-fxrates000[,"DEXUSUK"]

# 1.2 Geometric Brownian Motion Model(two time scales)
# Case 1. EUR/USD

par(mfcol=c(2,1))
plot(fx.USEU)

fx.USEU.itsreturns<-fcn.itsreturns(fx.USEU)
# head(fx.USEU.itsreturns)
# tail(fx.USEU.itsreturns)
# dimnames(fx.USEU.itsreturns)
plot(zoo(fx.USEU.itsreturns[,"ret"]), ylab="Return")

hist(fx.USEU.itsreturns$ret,100, probability = TRUE, ylab = "Return", main = "fx.USEU")
lines(density(fx.USEU.itsreturns$ret), col="green")

qqnorm(fx.USEU.itsreturns$ret)

# Geometric Brownian Motion Model
y<-fx.USEU.itsreturns[,"ret"]


fcn.gbm.compare(fx.USEU.itsreturns)
fcn.gbm.compare(fx.USEU.itsreturns, end.time = "2013-09-27")
  

# Case 1. GBP/USD

par(mfcol=c(2,1))
plot(fx.USUK)

fx.USUK.itsreturns<-fcn.itsreturns(fx.USUK)
# head(fx.USEU.itsreturns)
# tail(fx.USEU.itsreturns)
# dimnames(fx.USEU.itsreturns)
plot(zoo(fx.USUK.itsreturns[,"ret"]), ylab="Return")

hist(fx.USUK.itsreturns$ret,100, probability = TRUE, ylab = "Return", main = "fx.USEU")
lines(density(fx.USUK.itsreturns$ret), col="green")

qqnorm(fx.USUK.itsreturns$ret)


fcn.gbm.compare(fx.USUK.itsreturns)
fcn.gbm.compare(fx.USUK.itsreturns, end.time = "2013-09-27")


# 1.3 Time Dependence in Squared-Returns
y<-fx.USEU.itsreturns[time(fx.USEU.itsreturns) <= "2013-09-27","ret"]
par(mfrow=c(2,2))
acf(y)
acf(y, type = "partial")
acf(y^2)
acf(y^2, type = "partial")


# 1.4 Gaussian ARCH and GARCH Models
y.arch1<-garch(y, order=c(0,1), trace = FALSE)
y.arch2<-garch(y, order=c(0,2), trace = FALSE)
y.arch10<-garch(y, order=c(0,10), trace = FALSE)
y.garch11<-garch(y, order=c(1,1), trace = FALSE)

options(show.signif.stars = FALSE)


summary(y.arch1)
summary(y.arch2)
summary(y.arch10)
summary(y.garch11)

names(y.garch11)

vol.estmat<-cbind( y.arch1$fitted.values[,1],
                   y.arch2$fitted.values[,1],
                   y.arch10$fitted.values[,1],
                   y.garch11$fitted.values[,1])

par(mfcol=c(2,1), xpd=FALSE)
ts.plot(vol.estmat[,1], col=c(1,2,3,4), main="ARCH(1)", ylab="vol")
abline(h=sqrt(var(y)), col=1, lwd=3)

ts.plot(vol.estmat[,2], col=c(2,2,3,4), main="ARCH(2)", ylab="vol")
abline(h=sqrt(var(y)), col=1, lwd=3)

ts.plot(vol.estmat[,3], col=c(3,2,3,4), main="ARCH(10)", ylab="vol")
abline(h=sqrt(var(y)), col=1, lwd=3)

ts.plot(vol.estmat[,4], col=c(4,2,3,4), main="GARCH(1,1)", ylab="vol")
abline(h=sqrt(var(y)), col=1, lwd=3)

par(mfcol=c(1,1))
ts.plot(vol.estmat, col=c(1,2,3,4), main="ARCH and GARCH Fits", ylab="vol")


# 1.5 GARCH(1,1) Models with  tDistributions
# 1. Load rugarch library
library("rugarch")

# 2.Fortimesseries y, specify an AR(p) autoregressive model
# (implicit Gaussian/Normal assumption for errors)
y.ar<-ar(y)
print(y.ar$order)

y.ar.0<-ar(y, order.max = y.ar$order, aic = FALSE)
summary(y.ar.0)

y.lag1<-fcn.lag0(y, lag = 1)
y.lag2<-fcn.lag0(y, lag = 2)
y.lag3<-fcn.lag0(y, lag = 3)
y.lag4<-fcn.lag0(y, lag = 4)
y.lag5<-fcn.lag0(y, lag = 5)
y.lag6<-fcn.lag0(y, lag = 6)
y.lag7<-fcn.lag0(y, lag = 7)

head(y.lag1)

y.ar.7<-lm(y ~ y.lag1 + y.lag2 + y.lag3 + y.lag4 + y.lag5 + y.lag6 + y.lag7)
summary(y.ar.7)


# Note the t statistic for the order-7 parameter exceeding 2.
# The r function arma() provides an alternative specification of theAR(7) model
#
# The function applies a different estimation algorithm (numerical optimization with 
# the r function optim() using finite-differences for gradients).
#
# Comparison of the output shows different results numerically, but
# the fitted models are consistent with each other.

y.ar.00<-arma(y, order = c(y.ar$order, 0))
summary(y.ar.00)

par(mfcol=c(3,1))
# Plot residuals histogram

# Plot[1,1]
hist0<-hist(scale(as.numeric(y.ar.7$residuals)), freq = FALSE, nclass = 200,
            main = "Histogram of AR(7) Standardized Residuals")
# scale the residuals to have mean 0 and variance 1
x.density<-sort(scale(coredata(y.ar.7$residuals)))
y.density<-dnorm(x.density)
lines(x.density, y.density, col=4, lwd=2)

# Plot[2,1]
qqnorm(y.ar.7$residuals, main = "Normal Q-Q Plot of AR(7) Residuals")

# Plot[3,1]
qqnorm(scale(y.ar.7$residuals), ylab = "Sample Quantiles Normalized",
       main = "Normal Q-Q Plot of Standardized Residuals")
abline(a=0, b=1)

# Fit Gaussian GARCH(1,1) using the functions ugarchspec() and ugarchfit() from 
# the library "rugarch"

spec= ugarchspec(mean.model = list(armaOrder = c(7,0)))
fit.garch11.gaussian=ugarchfit(spec=spec, data = y)
fit.garch11.gaussian.fit<-attributes(fit.garch11.gaussian)$fit
names(fit.garch11.gaussian.fit)

par(mfcol=c(2,1))
#plot[1,1]
hist(fit.garch11.gaussian.fit$z, nclass = 200, probability = TRUE,
     main = "Histogram of Gaussian\nGARCH(1,1) Residuals",
     xlab = "Standardized Residual (z)")
x.density<-sort(fit.garch11.gaussian.fit$z)
y.density<-dnorm(scale(x.density))
lines(x.density, y.density, col=4, lwd=2)

# plot[2,1]
qqnorm(x.density,
       main = paste("Normal Q-Q Plot of AR(7) - GARCH(1,1)\n",
                    "Standardized Residuals\n",
                    "(Gaussian Assumption)", sep = ""))
abline(a=0, b=1)

# These results show how the AR(7)-GARCH(1,1) model with Gaussian residuals
# improves the fit in terms o fthe QQ plot when compared to the AR(7) model with
# no GARCH structure

# Fit t-Distribution GARCH(1,1) model
# Use t-dist (df0=10)

df0<-10
fixed.pars.df0=list(shape=df0)
spec.B.df0<-ugarchspec(distribution.model = "std",
                       fixed.pars = fixed.pars.df0,
                       mean.model = list(armaOrder=c(7,0)))
fit.B.df0<-ugarchfit(spec=spec.B.df0, data = y)
fit.B.df0.attributes.fit<-attributes(fit.B.df0)$fit

# Plot histogram of residuals and fitted distribution
# Adjust standardized residuals (z) which have variance 1
# to t residuals
x.density0<-sort(fit.B.df0.attributes.fit$z)*sqrt(df0/(df0-2))
y.density0<-dt(x.density0, df=df0)
y.quantile0<-qt(c(1:length(x.density0))/(length(x.density0)+1), df=df0)

par(mfcol=c(2,1))
hist(x.density0, nclass = 200, probability = TRUE,
     xlab = "Stardized Residual (z)",
     main = paste("Empiric Histogram of Standardized Residuals\n",
                  "AR(10) - GARCH(1,1) With t-Dist, (df=",
                  as.character(df0),")\nFitted t Density", collapse = ""))

lines(x.density0, y.density0, col=3,lwd=2)
plot(y.quantile0, x.density0,
     xlab = "Theoretical Quantiles", ylab="Sample Quantiles",
     main = paste("Q-Q Plot of Standardized Residuals \nAR(10) - GARCH(1,1) ",
                  "With t-Dist. (df=", as.character(df0), ")", collapse = ""))
abline(a=0,b=1)

# These results show that adjusting the AR(7)-GARCH(1,1) model toassume
# t-distribution (df=10) for the residuals improves the fit.
#
# The theoretical quantiles of the t distribution are larger in magnitude
# at the extremes of the data

# Compare the table of AR(7)-GARCH(1,1) parameters from the two fits
# assuming Gaussian and t-dist(df=10)
fit.garch11.gaussian.fit$matcoef
fit.B.df0.attributes.fit$matcoef

fit.garch11.gaussian.fit$persistence
fit.B.df0.attributes.fit$persistence

par(mfcol=c(3,1))
plot(y001<-fit.garch11.gaussian.fit$sigma, type="l",
     main="Daily Sigma of Gaussian GARCH(1,1)", ylab="Daily Sigma")
plot(y002<-fit.B.df0.attributes.fit$sigma, type="l",
     main="Daily Sigma of t-Dist GARCH(1,1)", ylab="Daily Sigma")
plot(y001-y002, main="Difference in Daily Sigma Estimates",
     ylab="Daily Sigma Difference", type="l")
abline(h=0, col=6)

# Re-do the plot on Volatility (annualized st. dev.) scale
par(mfcol=c(3,1))
plot(y001<-sqrt(252)*fit.garch11.gaussian.fit$sigma, type="l",
     main="Daily Sigma of Gaussian GARCH(1,1)", ylab="Daily Sigma")
plot(y002<-sqrt(252)*fit.B.df0.attributes.fit$sigma, type="l",
     main="Daily Sigma of t-Dist GARCH(1,1)", ylab="Daily Sigma")
plot(y001-y002, main="Difference in Daily Sigma Estimates",
     ylab="Daily Sigma Difference", type="l")
abline(h=0, col=6)

# Value at Risk Plot for AR(7)-GARCH(1,1) Model with
# t distribution (df=10) innovations
fit000<-fit.B.df0.attributes.fit
fit000.longtermmean<-as.numeric(fit000$coef[1]/(1-sum(fit000$coef[2:8])))
quantile.level0<-0.025
t.df0<-df0
tquantile.lo<-qt(quantile.level0, df=10)
tquantile.hi<-qt(1-quantile.level0, df=10)
fit.varlimit.hi<-fit000$fitted.values +
  tquantile.hi*fit000$sigma*sqrt((t.df0-2.)/(t.df0))
fit.varlimit.lo<-fit000$fitted.values +
  tquantile.lo*fit000$sigma*sqrt((t.df0-2.)/(t.df0))

fit.varlimit.hi<-fit000.longtermmean +
  tquantile.hi*fit000$sigma*sqrt((t.df0-2.)/(t.df0))
fit.varlimit.lo<-fit000.longtermmean +
  tquantile.lo*fit000$sigma*sqrt((t.df0-2.)/(t.df0))

fit000.0<-0*fit.varlimit.hi+fit000.longtermmean

mean(y < fit.varlimit.lo)
mean(y > fit.varlimit.hi)

ylim000=c(min(c(fit.varlimit.hi, fit.varlimit.lo, y)),
          max(c(fit.varlimit.hi, fit.varlimit.lo, y)))

par(mfcol=c(1,1))
plot(c(1:length(y)),y$ret,  type = "h", col=4,
     xlab = "Index",
     ylim = ylim000,
     ylab = "Return",
     main = paste(c("Series with ",
                    round(100*quantile.level0, digits = 1),
                    "% VaR Limits"), collapse = ""))

lines(c(1:length(fit.varlimit.hi)), fit.varlimit.hi, col=3, lwd=2)
lines(c(1:length(fit.varlimit.lo)), fit.varlimit.lo, col=2, lwd=2)
abline(h=0, col=7)

# Determination of maximum-likelihood estimate of t distribution
# degrees of freedom

# Fix the AR(7) model for the mean process
spec=ugarchspec(mean.model = list(armaOrder=c(7,1)))

# Consider degres of freedom ranging from 4 to 30
list.df0<-c(4:30)

# Create matrices for the log likelihoods and fitted sigmas
# where each column corresponds to one of the t distribution fitted models

mat.log.likelihoods<-matrix(0,nrow=length(y), ncol=length(list.df0))
mat.fitted.sigma<-matrix(0,nrow = length(y), ncol = length(list.df0))
for (j.df0 in c(1:length(list.df0))) {
  df0<-list.df0[j.df0]
    fixed.pars.df0=list(shape=df0)
    spec.B.df0<-ugarchspec(distribution.model = "std",
                           fixed.pars = fixed.pars.df0,
                           mean.model = list(armaOrder=c(7,0)))
    fit.B.df0<-ugarchfit(spec = spec.B.df0, data = y)
  
  fit.B.df0.attributes.fit<-attributes(fit.B.df0)$fit
  mat.log.likelihoods[,j.df0]<-fit.B.df0.attributes.fit$log.likelihoods
  mat.fitted.sigma[,j.df0]<-fit.B.df0.attributes.fit$sigma
  
}

 
### Likelihood Plot vs Degrees of freedom
# (Condition on first 31 observations so likelihood of everymodel
# is based on same sample of cases).

par(mfcol=c(1,1))

mat.log.likelihoods.tot<-apply(mat.log.likelihoods[-c(1:8),], 2, sum)
print(cbind(list.df0, mat.log.likelihoods.tot))

head(mat.log.likelihoods)

plot(list.df0, mat.log.likelihoods.tot,
     ylab="Minus Log Likelihood",
     xlab="Degrees of Freedom for t-distribution",
     main="t-Distribution GARCH(1,1)\nNegative Log Likelihood vs Degrees of Freedem")
