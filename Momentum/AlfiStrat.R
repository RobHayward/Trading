require(quantmod)
require(PerformanceAnalytics)
getSymbols.yahoo('QQQ', env = (.GlobalEnv), from = "1999-01-01")
daysSinceHigh <- function(x, n){
  apply(embed(x, n), 1, which.max) -1
}
myStrat <- function(x, nHold = 50, nHigh = 100){
  position <- ifelse(daysSinceHigh(x, nHigh) <=nHold, 1, 0)
  c(rep(0, nHigh-1), position)
}
myStock <- Cl(QQQ)
myPosition <- myStrat(myStock, 50, 100)
bmkReturns <- dailyReturn(myStock, type = "arithmetic")
myReturns <- bmkReturns*Lag(myPosition, 1)
myReturns[1] <- 0
names(bmkReturns) <- 'Nasdaq'
names(myReturns) <- 'Strategy'
charts.PerformanceSummary(cbind(bmkReturns, myReturns))
Performance <- function(x){
  cumRetx = Return.cumulative(x)
  annRetx = Return.annualized(x, scale = 252)
  sharpex = SharpeRatio.annualized(x, scale = 252)
  winpctx = length(x[x > 0]) / length(x[x != 0])
  annSDx = sd.annualized(x, scale = 252)
  DDs <- findDrawdowns(x)
  maxDDx = min(DDs$return)
  maxLx = max(DDs$length)

Perf = c(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx, maxLx)
names(Perf) = c("Cumulative Return", "Annual Return", "Annualised Sharpe Ratio", 
                "Win %", "Annualised Volatility", "Maximum Drawdown", 
                "Max Length Drawdown")
return(Perf)
}
    
cbind(Strategy = Performance(myReturns), Nasdaq = Performance(bmkReturns))
table.DownsideRisk(myReturns)
