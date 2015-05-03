# http://www.r-bloggers.com/the-zomma-warthog-index/
require(PerformanceAnalytics)
require(quantmod)
getSymbols("XLP", from="1990-01-01")
getSymbols("GLD", from="1990-01-01")
getSymbols("TLT", from="1990-01-01")
prices <- cbind(Ad(XLP), Ad(GLD), Ad(TLT))
prices <- prices[!is.na(prices[, 2]),]
rets <- Return.calculate(prices)
warthogRets <- Return.portfolio(rets, weights=c(.5, .15, .35), rebalance_on = "years")
getSymbols("SPY", from="1990-01-01")
SPYrets <- Return.calculate(Ad(SPY))
comparison <- merge(warthogRets, SPYrets, join='inner')
charts.PerformanceSummary(comparison)
# 
years <- apply.yearly(comparison, Return.cumulative)
years <- years[-1,] #remove 2004
years
sum(years[,1] > years[,2])/nrow(years)
sapply(years, mean)
# 
years <- years[-4,] #remove 2008
sapply(years, mean)
# 
diff <- comparison[,1] - comparison[,2]
charts.PerformanceSummary(diff, main="short SPY against portfolio")
#strategy against SPY post-crisis
charts.PerformanceSummary(comparison["2009::"])
