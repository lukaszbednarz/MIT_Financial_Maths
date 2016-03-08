setwd(paste(getwd(), "/Learning/MIT_Financial_Maths/case-studies/Case_Study_3", sep=""))

source("fm_casestudy_0_InstallOrLoadLibraries.r")

dbnames0<-load(file="casestudy_1_0.RData")
#print(dbnames0)

head(fred.data0)
tail(fred.data0)

library("graphics")
library("quantmod")
plot(fred.data0[,"DGS10"])


# There are dates (rows of fred.data0) with missing values (NAs)
# Print out the counts of missing values
# using the function apply to count the TRUE values in each colum of the
# logical matrix is.na(fred.data0), which replaces the matrix fred.data0 with
# the element-wise evaluation of the function is.na() which is TRUE if
# the argumnet is missing (i.e., NA)
print(apply(is.na(fred.data0),2,sum))

#Identify rows for which DGS10 data is missing

index.fred.data0.notavail<-which(is.na(fred.data0[,"DGS10"])==TRUE)

print(time(fred.data0[index.fred.data0.notavail]))

#Note that the FRED data is missing when there are holidays or market-closes
#in the bond market of the US.
#Define fred.data0.0 as submatrix with nonmissing data for DGS10

fred.data0.0<-fred.data0[which(is.na(fred.data0[,"DGS10"])==FALSE),]

print(apply(is.na(fred.data0.0),2,sum))

#Some column variables of fred.data0.0 have missing values
#(i.e., DAAA, DBBB, DCOILWTICO).
#Our focus is on DGS10,the yield of constant-maturity 10 Year US bond.
y.DGS10.daily<-na.omit(fred.data0.0[,"DGS10"])
dim(y.DGS10.daily)
dimnames(y.DGS10.daily)[[2]]
head(y.DGS10.daily)

#The function to.weekly() and to.monthly() converts a time series data object
#to an Open/High/Low/Close series on a periodicity lower than the input data object.
head(to.weekly(y.DGS10.daily))

#Check how the first row of to.weekly(y.DGS10.daily) is consistent with
#the first 5 rows of y.DGS10.daily

#The function chart Series() plots Open/High/Low/Close series
chartSeries(y.DGS10.daily)

chartSeries(to.weekly(y.DGS10.daily))

chartSeries(to.monthly(y.DGS10.daily))

#The 4th column of the output from functions to.weekly() and to.monthly() 
#is the Close corresponding to the periodicity.

#Define the two vector time series of weekly close values and of monthly close values

y.DGS10.weekly<-to.weekly(y.DGS10.daily)[,4]
y.DGS10.monthly<-to.monthly(y.DGS10.daily)[,4]

head(to.weekly(y.DGS10.daily))
head(y.DGS10.weekly)

dim(y.DGS10.weekly)
dim(y.DGS10.monthly)

#Plot the ACF(auto-correlation function) and PACF (partial auto-correlation function)
#for each periodicity

par(mfcol=c(2,3))
acf(y.DGS10.daily)
acf(y.DGS10.daily, type="partial")
acf(y.DGS10.weekly)
acf(y.DGS10.weekly, type="partial")
acf(y.DGS10.monthly)
acf(y.DGS10.monthly, type="partial")

# The function adf.test() conducts the Augmented Dickey-Fuller Test
# For each periodicity, apply the function adf.test() twice:
# 1) to the un-differenced series (null hypothesis: input series has a unit root)
# 2) to the first-differenced series (same null hypothes is about differenced series)
# help(adf.test)
# provides references for the test
# Results for the un-differenced series:

adf.test(y.DGS10.daily)
adf.test(y.DGS10.weekly)
adf.test(y.DGS10.monthly)

# Results for the first-differenced series:
adf.test(na.omit(diff(y.DGS10.daily)))
adf.test(na.omit(diff(y.DGS10.weekly)))
adf.test(na.omit(diff(y.DGS10.monthly)))


par(mfcol=c(2,3))
acf(na.omit(diff(y.DGS10.daily)))
acf(na.omit(diff(y.DGS10.daily)), type="partial")
acf(na.omit(diff(y.DGS10.weekly)))
acf(na.omit(diff(y.DGS10.weekly)), type="partial")
acf(na.omit(diff(y.DGS10.monthly)))
acf(na.omit(diff(y.DGS10.monthly)), type="partial")

par(mfcol=c(2,1))
plot(y.DGS10.monthly)
plot(diff(y.DGS10.monthly))
abline(h=0, col=2)

# understanding partial autocorrelation coefficients
y0<-na.omit(diff(y.DGS10.monthly))
args(acf)

par(mfcol=c(2,1))
y0.acf<-acf(y0, type = "correlation")
y0.pacf<-acf(y0, type = "partial")

# Create a table of the first 10 acf values and pacf values:
y0.acf<-acf(y0, type = "correlation", plot=FALSE, lag.max = 10)
y0.pacf<-acf(y0, type = "partial", plot = FALSE, lag.max = 10)

class(y0.acf)

# The length of the acf vector is 11 for the simple acf
# and 10 for the partial acf.
# The simple acf includes the zero-th order autocorrelation which is 1.0
# Apply the function cbind() to bind together columns into a matrix
# (use as.matrix() to coerce an n-vector into an (nx1) matrix)

tab.acf_pacf<-cbind( 
  as.matrix(y0.acf$acf[-1]),
  as.matrix(y0.pacf$acf))

print(tab.acf_pacf)

# set names of rows and columns:
dimnames(tab.acf_pacf)<-list(c(1:10), c("acf","pacf"))

# Consider the auto-regression models where 
# y0 is the dependent variables 
# lags of y0 are theindependent variables

y0.lag1<-lag(y0, k=1)
y0.lag2<-lag(y0, k=2)
y0.lag3<-lag(y0, k=3)
y0.lag4<-lag(y0, k=4)

# The r function lm() fits the linear model by least squares
# The r function summary.lm() summarizes a fitted model(output from lm())

options(show.signif.stars = FALSE)
summary.lm(lm(y0 ~ y0.lag1))

print(y0.lag1)
dim(y0.lag1)
dim(y0)

lm.lag1<-lm(y0 ~ y0.lag1)
summary.lm(lm.lag1)

summary.lm(lm(y0 ~ y0.lag1 + y0.lag2))
summary.lm(lm(y0 ~ y0.lag1 + y0.lag2 + y0.lag3))
summary.lm(lm(y0 ~ y0.lag1 + y0.lag2 + y0.lag3 + y0.lag4))
tab.acf_pacf[1:4,]

# EVALUATING THE STATIONARITY AND THE CYCLICALITY OF THE FITTED AR(2) MODEL TO MONTHLY DATA.

# we fit the AR(2) model and evaluate the roots of the characteristic polynomial
# The AR(2) model has a p-value 0.0229 which is statistically significant

lmfit0<-lm(y0 ~ y0.lag1 + y0.lag2)
summary(lmfit0)

lmfit0$coefficients

# Extract AR(2) coefficients phi1 and phi2 as named elements of
# output list element $coefficients

lmfit0.phi1<-lmfit0$coefficients["y0.lag1"]
lmfit0.phi2<-lmfit0$coefficients["y0.lag2"]

# polyroot(z) returns complex roots of polynomial with coefficients z
char.roots<-polyroot(c(1, -1.*lmfit0.phi1, -1.*lmfit0.phi2))
print(char.roots)

print(Conj(char.roots)*char.roots)

# These roots are complex and outside the unit circle so the fitted model is stationary
# With complex roots, there is evidence of cyclicality int heseries
# The following computation computes the period as it is determined by the
# coefficients of the characteristic polynomial.

twopif0=acos(abs(lmfit0.phi1)/(2*sqrt(-lmfit0.phi2)))
f0= twopif0/(8*atan(1))

period0 <- 1/f0

print(as.numeric(period0))

# THE BEST AR(2) MODEL USING THE AIC CRITERION

# 8.1 Apply function ar() to identify best AR(K) model by the AIC criterion----
# see help(ar) for details of the function

help(ar)

y0.ar<-ar(y0)

# The output object is a list with named elements:

names(y0.ar)

# The output element $order is the the AR(p) order p which has minimum AIC statistic
y0.ar$order
y0.ar$order.max
y0.ar$aic

par(mfcol=c(1,1))
plot(y0.ar$aic, main = "Relative AIC Statistic\ AR(p) Models of Monthly Data", xlab = "Order p")

#8.2 Using ar() and lm() tospecify/summarize AR(p) fitted models----

y0.ar.7<-ar(y0, aic=FALSE, order.max = 7)
y0.ar.7

ar(x = y0, aic=FALSE, order.max = 7)

# The function ar() gives coefficient estimates but does not summarize
# the autoregression model with the regression detail of the function
# summary.lm()
# Summarize the fit the AR(7) modelusing lm() with lagged variables:

y0.lag5<-lag(y0,k=5)
y0.lag6<-lag(y0,k=6)
y0.lag7<-lag(y0,k=7)

summary.lm(lmfit0<-lm(y0~y0.lag1+y0.lag2+y0.lag3+y0.lag4+
                        +y0.lag5+y0.lag6+y0.lag7,x=TRUE,y=TRUE))

lm(formula=y0~y0.lag1+y0.lag2+y0.lag3+y0.lag4+y0.lag5+y0.lag6+y0.lag7,x=TRUE,y=TRUE)


# EVALUATING THE STATIONARITY OF THE BEST AR(P) MODEL
# Again we can check the stationarity of the order-7 autoregression using
# polyroot(z) which returns complex roots of polynomial with coefficients z

#                 p(x)=z[1]+z[2]x+z[n]xn????1

char.roots.DGS10 <- polyroot(c(1,-1*y0.ar$ar))
char.roots.DGS10.modsq <- (Conj(char.roots.DGS10)*char.roots.DGS10)
char.roots.DGS10.modsq0 <- sqrt((Re(char.roots.DGS10.modsq))^2 + 
                                  (Im(char.roots.DGS10.modsq))^2)

print(char.roots.DGS10)

print(char.roots.DGS10.modsq0)

# The smallest root modulus is 1.4027 which is outside the complex unit circle.

# COMPUTE/EVALUATE INFLUENCE MEASURES / CASE-DELETION STATISTICS

# Compute lmfit0, the fit of the AR(7) model and print out its summar 

summary.lm(lmfit0 <- lm(y0 ~ y0.lag1 + y0.lag2 + y0.lag3 +
                       y0.lag4 + y0.lag5 + y0.lag6 + y0.lag7,
                       x=TRUE, y=TRUE, weights = 1*(is.na(y0.lag7)==FALSE)))

names(lmfit0)
dim(lmfit0$x)
dim(lmfit0$y)
length(y0)

# Compute influence measures (case-deletion statistics) of the fitted model

lmfit0.inflm <- influence.measures(lmfit0)
names(lmfit0.inflm)

# Show the dimentions and first rows of the 12-column output list element $infmat
dim(lmfit0.inflm$infmat)
head(lmfit0.inflm$infmat)

# Show the dimensions and first rows of the 12-column output list element $is.inf
dim(lmfit0.inflm$is.inf)
head(lmfit0.inflm$is.inf)

# The $is.inf elements are TRUE if the magnitude of the respective influence
# measure in $infmat exceeds nominal cutoffs; see help(influence.measures)

# Count of influential cases by column/influence-measure

apply(lmfit0.inflm$is.inf,2,sum)

# Table counts of influential/non-influential cases
# as measured by the hat/leverage statistic.

table(lmfit0.inflm$is.inf[,"hat"])

# Plot dependent variable vs each independent variable 
# and selectively highlight influentialcases 
# (Use output elements lmfit0$x and lmfit0$y rather than the input argument variables)
head(lmfit0$x)

par(mfcol=c(3,3))
for(j in c(1:7)){
  plot(lmfit0$x[,1+j],lmfit0$y,
       main=paste("y0 vs y0.lag",as.character(j),"\nHigh-LeverageCases(redpoints)",sep=""),
       cex.main=0.8)
  abline(h=0,v=0)
#abline(lmfit0,col=3,lwd=3)

# Plot cases with high leverage as red (col=2) "o" s
  index.inf.hat<-which(lmfit0.inflm$is.inf[,"hat"]==TRUE)
  points(lmfit0$x[index.inf.hat,j+1],lmfit0$y[index.inf.hat],
         col=2,pch="o")
}

# Plot leverage of cases (diagonals of hat matrix)

plot(lmfit0.inflm$infmat[,"hat"])
print(time(lmfit0.inflm$infmat)[index.inf.hat])

# Note the 2 cases are time points in the heart of the financial crisis of 2008.

# Time series plot of residuals, applying the function zoo() to create
# the residual time series object (the $residuals element of the lm() output
# has data and time attributes that are not the same length due to
# incorrect handling of missing data/rows

names(lmfit0)
length(lmfit0$residuals)
length(time(lmfit0$residuals))
length(coredata(lmfit0$residuals))

lmfit0$residuals <- zoo(as.numeric(lmfit0$residuals), order.by = time(lmfit0$residuals)[-(1:7)])
plot(lmfit0$residuals, ylab = "Residual", xlab = "Date")

# The R function $plot.lm()$ generates a useful 2x2 display
# of plots for various regression diagnostic statistics:

layout(matrix(c(1,2,3,4),2,2))
plot(lmfit0)
