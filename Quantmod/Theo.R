####################################################################

#This is a Demo of the MACD Trading Strategy Implemented using the 
#developing quant program Quantstrat

####################################################################

require(zoo)
require(quantmod)
require(TTR)
require(blotter)
#install.packages("quantstrat", repos="http://R-Forge.R-project.org")
require(quantstrat)
require(devtools)
require(IKTrading)
install_github("IlyaKipnis/IKTrading")

currency("USD")
## [1] "USD"
#stock("SPY",currency="USD",multiplier=1)
## [1] "SPY"
#ls(envir=FinancialInstrument:::.instrument)
## [1] "SPY" "USD"
#ls(all=T)
## [1] ".blotter" ".strategy" "filename"

# system settings
initDate <- '1999-12-31'
startDate <- '2000-01-01'
endDate <- '2016-01-31'
initEq <- 1e6
Sys.setenv(TZ="UTC")
getSymbols('SPY', from=startDate, to=endDate, index.class="POSIXct", adjust=T)
#colnames(SPY) <- c("Open", "High", "Low", "Close", "Adjusted", "Volume")

#SPY=to.monthly(SPY, indexAt='endof', drop.time=FALSE)
#SPY$SMA10m <- SMA(Cl(SPY), 10)


# initialise portfolio, account
qs.strategy <- "qsMACD"
rm.strat(qs.strategy) # remove strategy etc. if this is a re-run
initPortf(qs.strategy,'SPY', initDate=initDate)
initAcct(qs.strategy,portfolios=qs.strategy, initDate=initDate, initEq=initEq)

# initialize orders container
#args(initOrders)
## function (portfolio = NULL, symbols = NULL, initDate = "1999-12-31",
## ...)
## NULL
initOrders(portfolio=qs.strategy,initDate=initDate)
# instantiate a new strategy object
#args(strategy)
## function (name, ..., assets = NULL, constraints = NULL, store = FALSE)
## NULL
##This initilises the strategy , where all the indicators, signals & Rules are 
strategy(qs.strategy,store=TRUE)


#args(getStrategy)
## function (x, envir = .strategy)
## NULL
strat <- getStrategy(qs.strategy)
#class(strat)
## [1] "strategy"
#summary(strat)

#Define the MACD

macd <- MACD( Cl(SPY), 12, 26, 9, maType="EMA" )


#Calculating the histogram
MACD_SIG = 100 * (EMA(x = Cl(SPY), n = 12) / EMA(x = Cl(SPY), n = 26))
MACD_SIGL <- EMA(x <- MACD_SIG, n = 9)
MACD_hist <- MACD_SIG - MACD_SIGL
names(MACD_hist) = "hist"
SPY <- cbind(SPY, MACD_hist)

add.indicator(qs.strategy, name = "MACD",
              arguments = list(x=quote(Ad(mktdata))),label='osc')

add.indicator(strategy = qs.strategy, name = "SMA",
              arguments = list(x = quote(Ad(mktdata)), n=200), label="SMA200")

#summary(getStrategy(qs.strategy))

#Adding theshold for MACD cross +ve
add.signal(qs.strategy,name="sigThreshold",
           arguments=list(column="hist",relationship="gt",threshold=0,cross=TRUE),
           label="signal.gt.zero")

#Adding Threshold for MACDcross -ve
add.signal(qs.strategy,name="sigThreshold",
           arguments=list(column="hist",relationship="lt",threshold=0,cross=TRUE),
           label="signal.lt.zero")

#Signal Comparision for Close > 200day MA
add.signal(qs.strategy, name = "sigComparison", 
           arguments = list(column = c("Adjusted", "SMA200"),
                            relationship = "gt"),
           label = "Cl.gt.SMA")


#Signal Comparision for Close < 200day MA
add.signal(qs.strategy, name = "sigComparison", 
           arguments = list(column = c("Close", "SMA200"),
                            relationship = "lt"),
           label = "Cl.lt.SMA")

#Long signal MACD & 200DAY
add.signal(qs.strategy, name = "sigAND", 
           arguments = list(column = c("signal.gt.zero", "Cl.gt.SMA"), cross = TRUE), 
           label = "long.entry")

#Short signal MACD & 200DAY
add.signal(qs.strategy, name = "sigAND", 
           arguments = list(column = c("signal.lt.zero", "Cl.lt.SMA"), cross = TRUE), 
           label = "short.entry")




#summary(getStrategy(qs.strategy))

#Buy order Rule
add.rule(qs.strategy,name='ruleSignal',
         arguments = list(sigcol="long.entry",sigval=TRUE,orderqty=100,
                          ordertype='market',orderside='long'),
         type='enter',label='enter1')

#Close the Buy when macd crosses down
add.rule(qs.strategy,name='ruleSignal',
         arguments = list(sigcol="signal.lt.zero",sigval=TRUE,orderqty='all',
                          ordertype='market',orderside='long'),
         type='exit',label='exit1')

#Sell order Rule
add.rule(qs.strategy,name='ruleSignal',
         arguments = list(sigcol="short.entry",sigval=TRUE,orderqty=-100,
                          ordertype='market',orderside='short'),
         type='enter',label='enter2')

#Close the Sell when macd crossed up
add.rule(qs.strategy,name='ruleSignal',
         arguments = list(sigcol="signal.gt.zero",sigval=TRUE,orderqty='all',
                          ordertype='market',orderside='short'),
         type='exit',label='exit2')

summary(getStrategy(qs.strategy))

fastMA = 12
slowMA = 26
signalMA = 9
maType="EMA"
tradeSize <- initEq/100

#Apply The Strategy
applyStrategy(strategy=qs.strategy , portfolios=qs.strategy, parameters = 
                list(nFast=fastMA, nSlow=slowMA, nSig=signalMA,maType=maType),
              verbose = TRUE )
getTxns(Portfolio=qs.strategy, Symbol="SPY")

updatePortf(qs.strategy)
updateAcct(qs.strategy)
updateEndEq(qs.strategy)

#Check to see if the Program is coded correctly
#This should output [TRUE] if correct
checkBlotterUpdate <- function(port.st,account.st,verbose=TRUE)
{
  ok <- TRUE
  p <- getPortfolio(port.st)
  a <- getAccount(account.st)
  syms <- names(p$symbols)
  port.tot <- sum(sapply(syms,FUN = function(x) eval(parse(
    text=paste("sum(p$symbols",x,"posPL.USD$Net.Trading.PL)",sep="$")))))
  port.sum.tot <- sum(p$summary$Net.Trading.PL)
  if( !isTRUE(all.equal(port.tot,port.sum.tot)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match sum of symbols P&L")
  }
  initEq <- as.numeric(first(a$summary$End.Eq))
  endEq <- as.numeric(last(a$summary$End.Eq))
  if( !isTRUE(all.equal(port.tot,endEq-initEq)) ) {
    ok <- FALSE
    if( verbose )
      print("portfolio P&L doesn't match account P&L")
  }
  if( sum(duplicated(index(p$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in portfolio summary")
  }
  if( sum(duplicated(index(a$summary))) ) {
    ok <- FALSE
    if( verbose )
      print("duplicate timestamps in account summary")
  }
  return(ok)
}
checkBlotterUpdate(qs.strategy,qs.strategy)

# create custom theme
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
# plot performance

chart.Posn(Portfolio=qs.strategy,Symbol="SPY",
           Dates="2000::",theme=myTheme)
add_MACD()
add_SMA(n = 200)
add_BBands()



tstats <- t(tradeStats(qs.strategy))
tstats



