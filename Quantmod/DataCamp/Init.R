# This is a file that will try to implement the quantstrat code form datacamp
require(quantmod)
getSymbols("SPY", from = "2000-01-01", src = "google", to = "2016-06-30", adjust = TRUE)
plot(Cl(SPY))
#getSymbols("LQD", from = "2000-01-01, to = 2016-06-30", src = "google")
sma = SMA(Cl(SPY), n = 200)
plot(Cl(SPY))
lines(sma, col = 'red')
#getSymbols("SPY", from = "2000-01-01", to = "2013-12-31", scr = "yahoo")
require(quantstrat)
tradeaize = 1000000
initeq = 1000000
initdate = "01-01-1995"
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

