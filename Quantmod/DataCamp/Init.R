# This is a file that will try to implement the quantstrat code form datacamp
require(quantmod)
require(quantstrat)
# boiler plate=================

initdate = "1999-01-01"
from = "2003-01-01"
to = "2015-12-31"
currency("USD")
stock("LQD", currency = "USD", multiplier = 1)
getSymbols("LQD", from = "2000-01-01", src = "google", to = "2016-06-30", adjust = TRUE)
# remove any NAs
LQD <- LQD[!is.na(LQD$LQD.Open), ]
#===============
tradesize = 1000000
initeq = 1000000
#=======================
#initialise the portfoli etc
strategy.st = portfolio.st = account.st <- "firststrat"
rm.strat(strategy.st)
# each account can contain one or more portfolio and each portfolio can contain one or more strategy
# Initiate portfolio
initPortf(portfolio.st, symbols = "LQD", initDate = initdate, currency = "USD")
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initeq)
initOrders(portfolio.st, initDate = initdate)
strategy(strategy.st, store = TRUE)
#=======================================================
# Indicators
#There are five steps to calling indicators
# add.indicator(), strategy name, function for calculation (SMA), inputs for function as list, label indicator.
add.indicator(strategy = strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 200), label = "SMA200")
# apply indicators to check
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(LQD))
head(test, n = 3)
tail(test, n = 5)
# indicator names take their names from the original name a dot and then the label. 
head(HLC(LQD))
#dates can be narrowe4d by using the following form
HLC(LQD["2012-01-01/2012-01-07"])
#===============Add own strategy
DVO <- function(HLC, navg = 2, percentlookback = 126) {
  
  # Compute the ratio between closing prices to the average of high and low
  ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)
  
  # Smooth out the ratio outputs using a moving average
  avgratio <- SMA(ratio, n = navg)
  
  # Convert ratio into a 0-100 value using runPercentRank()
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}
add.indicator(strategy = strategy.st, name = "DVO", arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126), 
              label = "DVO_2_126")
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(LQD))
# this does not work.  No conformable.  Is this the change to data? 
# However, if I run it myself the strategy works
a <- DVO(HLC = LQD)
plot(a)




plot(Cl(SPY))
#getSymbols("LQD", from = "2000-01-01, to = 2016-06-30", src = "google")
sma = SMA(Cl(SPY), n = 200)
lines(sma, col = 'red')
#getSymbols("SPY", from = "2000-01-01", to = "2013-12-31", scr = "yahoo")
require(quantstrat)
rm(list = ls())


strategy.st = portfolio.st = account.st <- "firststrat"
rm.strat(strategy.st)
initPortf(portfolio.st, symbols = "SPY", initDate = initdate, currency= "USD")
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", 
         initEq = initeq)
initOrders(portfolio.st, initDate = initdate)
strategy(strategy.st, store = TRUE)
add.indicator(strategy = strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 200), label  = "SMA200")
#stock("SPY", currency = "USD")
DVO <- function(HLC, navg = 2, percentlookback = 126) {
  
  # Compute the ratio between closing prices to the average of high and low
  ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)
  
  # Smooth out the ratio outputs using a moving average
  avgratio <- SMA(ratio, n = navg)
  
  # Convert ratio into a 0-100 value using runPercentRank()
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}
add.indicator(strategy = strategy.st, name = "DVO", 
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126), 
              label = "DVD_2_126")
#use applyindicatos to test strategy
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))


