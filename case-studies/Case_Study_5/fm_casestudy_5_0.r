# 0.1 Install/load libraries
setwd("C:/Users/lbednarz/Documents/Learning/MIT_Financial_Maths/case-studies/Case_Study_5")
source("fm_casestudy_0_InstallOrLoadLibraries.r")

# Collect macro economic data from FRED database
# Macro Variables
#
# UNRATE    unemployment
# FEDFUNDS  Federal Funds Rate
# TB3MS     Treasury Bill Rate
# CPIAUCSL  CPI Index All Urban Customers All Items
# M1SL      M1
# GDPDEF    GNP deflator
# GDP       real GNP
# GPDI      real business fixed investment
# TWEXBMTH  Trade weighted value of dollar
# SP500     S&P 500 Index

getSymbols("UNRATE", src = "FRED")
head(UNRATE)
chartSeries(UNRATE)

getSymbols("FEDFUNDS", src = "FRED")
head(FEDFUNDS)
chartSeries(FEDFUNDS)

getSymbols("CPIAUCSL", src = "FRED")
head(CPIAUCSL)
chartSeries(CPIAUCSL)

getSymbols("M1SL", src = "FRED")
head(M1SL)
chartSeries(M1SL)

getSymbols("GDPDEF", src = "FRED")
head(GDPDEF)
chartSeries(GDPDEF)

getSymbols("GDP", src = "FRED")
head(GDP)
chartSeries(GDP)

getSymbols("TB3MS", src = "FRED")
head(TB3MS)
chartSeries(TB3MS)

getSymbols("TWEXBMTH", src = "FRED")
head(TWEXBMTH)
chartSeries(TWEXBMTH)

# Collect index data from Yahoo
# 1.1.1 Set start and end date for collection in YYYYMMDD (numeric) format
date.start<-20000101
date.end<-20160302

# 1.1.2 Collect historical datafor S&P 500 Index
SP500<-getYahooData("^GSPC", start = date.start, end = date.end)
head(SP500)
chartSeries(SP500)

# 1.3. Ordinary and Partial Autocorrelations of Reduced Set
# Consider focusing on 3 variables
ymat0<-merge(UNRATE, FEDFUNDS, CPIAUCSL)
head(ymat0)
tail(ymat0)

ind.quarterly0<-1*(is.na(ymat0[,3])== FALSE)
sum(ind.quarterly0)

dim(ymat0)

ymat00<-ymat0[which(ind.quarterly0 == 1),]
head(ymat00)

par(mfcol=c(3,1))
plot(ymat00[,1], main = dimnames(ymat00)[[2]][1])
plot(ymat00[,2], main = dimnames(ymat00)[[2]][2])
plot(ymat00[,3], main = dimnames(ymat00)[[2]][3])

# Extract window from 1960-2000

ymat00.0<-window(ymat00, 
                 start = as.Date("1960-01-01"),
                 end = as.Date("2000-12-31"))

dim(ymat00.0)
head(ymat00.0)

acf(ymat00.0, lag.max = 10)
acf(ymat00.0, type = "partial", lag.max = 10)

# Vector Autoregressive (VAR) Model of Reduced Set

# The function VARSet() is from the package vars; see Pfaff(2008).
# This function identifies the optimal VAR(p) order p.

ymat00.0.VAR.const<-VARselect(ymat00.0, lag.max = 12, type = "const")
ymat00.0.VAR.const$selection

# Fit the VAR model corresponding to Schwarz Criterion (SC) which is the BIC

ymat00.0.VAR.const.0<-VAR(ymat00.0, p=ymat00.0.VAR.const$selection[3], type = "const")
options(show.signif.stars = FALSE)
summary(ymat00.0.VAR.const.0)


# 1.5 Impulse Response Functions for a Fitted VAR(p) Model

plot(irf(ymat00.0.VAR.const.0, impulse = "UNRATE"))

# When unemployment rises
#   the Federal Funds rate is projected to decline
#   (consistent with Federal Reserve Policy)
#
#   the CPI decreases (lower employment results in less
#   pressure to increase consumer prices)

plot(irf(ymat00.0.VAR.const.0, impulse = "FEDFUNDS"))

# When the FED Funds rate increases :
#   The Unemployment rate tends to increase;
#   so reducing the Fed Funds rate would tend to reduce unemployment

#   The CPI increases; increases in the Fed Funds rate are
#   associated with increase in CPI over future quarters

plot(irf(ymat00.0.VAR.const.0, impulse = "CPIAUCSL"))

# When the CPI increases
#
#   The Federal Funds rate tends to increase over subsequent quarters.
#   This is consistent with Federal Reserve policy of rising
#   interest rates to control for inflation