# This is a file that will try to implement the quantstrat code form datacamp
require(quantmod)
require(quantstrat)
# boiler plate=================

initdate = "1999-01-01"
from = "2003-01-01"
to = "2015-12-31"
currency("USD")
stock("SPY", currency = "USD", multiplier = 1)
getSymbols("SPY", from = "2000-01-01", src = "google", to = "2016-06-30", adjust = TRUE)
# remove any NAs
SPY <- SPY[!is.na(SPY$SPY.Open), ]
#===============
tradesize = 1000000
initeq = 1000000
#=======================
#initialise the portfoli etc
strategy.st = portfolio.st = account.st <- "firststrat"
rm.strat(strategy.st)
# each account can contain one or more portfolio and each portfolio can contain one or more strategy
# Initiate portfolio
initPortf(portfolio.st, symbols = "SPY", initDate = initdate, currency = "USD")
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initeq)
initOrders(portfolio.st, initDate = initdate)
strategy(strategy.st, store = TRUE)
#=======================================================
# Indicators
#There are five steps to calling indicators
# add.indicator(), strategy name, function for calculation (SMA), inputs for function as list, label indicator.
add.indicator(strategy = strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 500), label = "SMA500")
add.indicator(strategy = strategy.st, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n = 200), label = "SMA200")
# apply indicators to check
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))
head(test, n = 3)
tail(test, n = 5)
# indicator names take their names from the original name a dot and then the label. 
head(HLC(SPY))
#dates can be narrowe4d by using the following form
HLC(SPY["2012-01-01/2012-01-07"])
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
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))
# this does not work.  No conformable.  Is this the change to data? 
# However, if I run it myself the strategy works
a <- DVO(HLC = SPY, navg = 2, percentlookback = 126)
plot(a)
# WTF
# copied from course page. 
# Add the DVO indicator to your strategy
add.indicator(strategy = strategy.st, name = "DVO", 
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126),
              label = "DVO_2_126")

# Use applyIndicators to test out your indicators
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))

# Subset your data between Sep. 1 and Sep. 5 of 2013
test_subset <- test["2013-09-01/2013-09-05"]
#===================
# Push on and sort later. 
add.signal(strategy.st, name = "SigComparison", 
           arguments = list(columns = c("str1", "str2"), 
                            relationship = "lt"), 
           label = "siglabel")
add.signal(strategy = strategy.st, 
           name = "sigCrossover", 
           arguments = list(columns = c("str1", "str2"), 
                            relationship = "eq"), 
           label = "siglabel")
add.signal(strategy = strategy.st, 
           name = "sigCrossover", 
           arguments = list(columns = c("SMA50", "SMA200"), 
                            relationship = "gt"), 
           label = "longfilter")
add.signal(strategy = strategy.st, 
           name = "sigCrossover", 
           arguments = list(columns = c("SMA50", "SMA200"), 
                            relationship = "lt"), 
           label = "filterexit")
add.signal(strategy = strategy.st, 
           name = "sigThreshold",
           arguments = list(column = "str1", 
                           threshold = 20, 
                           cross = TRUE, 
                           relationship = "lt"), 
           label = "siglabel")
# cross = TRUE minics sigCrossover
# cross = FALSE mimics sigComparison
add.signal(strategy = strategy.st, 
           name = "sigThreshold",
           arguments = list(column = "DVO_2_126", 
                           threshold = 20, 
                           cross = FALSE, 
                           relationship = "lt"), 
           label = "tresholdfilter")

add.signal(strategy = strategy.st, 
           name = "sigThreshold",
           arguments = list(column = "DVO_2_126", 
                           threshold = 80, 
                           cross = TRUE, 
                           relationship = "gt"), 
           label = "tresholdfilter")
#sigFormila() creates a signal that is composed of other signals
add.signal(strategy.st, 
           name = "sigFormula", 
           arguments = list(formula = "regular logical statement inside an if statement", 
                            cross = TRUE), 
           label = "yourlabel")
add.signal(strategy.st, 
           name = "sigFormula", 
           arguments = list(formula = "statement 1 & statement 2", 
                            cross = TRUE), 
           label = "yourlabel")

add.signal(strategy.st, 
           name = "sigFormula", 
           arguments = list(formula = "longthreshold & longfilter", 
                            cross = TRUE), 
           label = "longentry")
test_init <- applyIndicators(strategy.st, mktdata = OHLC(SPY))
test <- applySignals(strategy.st, mktdata = test_init)
# this also does not work.  It appears to be the same problem
head(test_init)

add.signal(strategy.st, 
           name = "cross",
           arguments = list(column = "SMA500",
                            cross = TRUE,
                            relationship = "gt"),
           label = "crossover" )











plot(Cl(SPY))
#getSymbols("SPY", from = "2000-01-01, to = 2016-06-30", src = "google")
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


