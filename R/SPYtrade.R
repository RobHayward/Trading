# http://quantstrattrader.wordpress.com/2014/11/07/
#predicting-high-yield-with-spy-a-two-part-post-2/
library(quantmod)
library(PerformanceAnalytics)
library(downloader)
getSymbols("VWEHX", from="1950-01-01")
getSymbols("SPY", from="1900-01-01")
getSymbols("HYG", from="1990-01-01")
getSymbols("SHY", from="1990-01-01")
getSymbols("VFISX", from="1990-01-01")

spySma20Cl <- SMA(Cl(SPY), n=20)
clSig <- Cl(SPY) > spySma20Cl
clSig <- lag(clSig, 1)

vwehxCloseRets <- Return.calculate(Cl(VWEHX))
vfisxCloseRets <- Return.calculate(Cl(VFISX))
vwehxAdjustRets <- Return.calculate(Ad(VWEHX))
vfisxAdjustRets <- Return.calculate(Ad(VFISX))

hygCloseRets <- Return.calculate(Cl(HYG))
shyCloseRets <- Return.calculate(Cl(SHY))
hygAdjustRets <- Return.calculate(Ad(HYG))
shyAdjustRets <- Return.calculate(Ad(SHY))

mutualAdRets <- vwehxAdjustRets*clSig + vfisxAdjustRets*(1-clSig)
mutualClRets <- vwehxCloseRets*clSig + vfisxCloseRets*(1-clSig)

etfAdRets <- hygAdjustRets*clSig + shyAdjustRets*(1-clSig)
etfClRets <- hygCloseRets*clSig + shyCloseRets*(1-clSig)
mutualFundBacktest <- merge(mutualAdRets, mutualClRets, join='inner')
charts.PerformanceSummary(mutualFundBacktest)
data.frame(t(rbind(Return.annualized(mutualFundBacktest)*100, 
                   maxDrawdown(mutualFundBacktest)*100,
                   SharpeRatio.annualized(mutualFundBacktest))))

clSig <- Cl(SPY) > SMA(Cl(SPY), n=20)

HYG <- merge(HYG, clSig, join='inner')
names(HYG)[7] <- "precomputed_signal"

#no parameters or indicators--we precalculated our signal

add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="precomputed_signal", threshold=.5, 
                          relationship="gt", cross=TRUE),
           label="longEntry")


add.signal(strategy.st, name="sigThreshold",
           arguments=list(column="precomputed_signal", threshold=.5, 
                          relationship="lt", cross=TRUE),
           label="longExit")

add.rule(strategy.st, name="ruleSignal",
         arguments=list(sigcol="longEntry", sigval=TRUE, orderqty=1, ordertype="market",
                        orderside="long", replace=FALSE, prefer="Open"),
         type="exit", path.dep=TRUE)

add.rule(strategy.st, name="ruleSignal", 
         arguments=list(sigcol="longExit", sigval=TRUE, orderqty="all", ordertype="market", 
                        orderside="long", replace=FALSE, prefer="Open"), 
         type="exit", path.dep=TRUE)

#apply strategy
t1 <- Sys.time()
out <- applyStrategy(strategy=strategy.st,portfolios=portfolio.st)
t2 <- Sys.time()
print(t2-t1)

#set up analytics
updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)  