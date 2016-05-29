library(quantstrat)
library(IKTrading)
options("getSymbols.warning4.0"=FALSE)
rm(list=ls(.blotter), envir = .blotter)
currency('USD')
Sys.setenv(TZ="UTC")
symbols = 'SPY'
suppressMessages(getSymbols(symbols, from = "1998-01-01", to = "2012-12-31"))
# define the stock.  This will be different if using futures data. 
# The contracts will complicate matters in those cases
# this will require multipliers. 
stock(symbols, currency = 'USD', multiplier = 1)
# This is a date before the transactions are started.  
initDate = "1990-01-01"
tradeSize = 1000000
initEq = tradeSize*length(symbols)
rm.strat(portfolio.st)
rm.strat(strategy.st)
# these can be used to make sure that there is a clean environemnt.
strategy.st <- portfolio.st <- account.st <- 'RSI_10_6'
# they are all called 'RSI_10_6`
initPortf(portfolio.st, symbols = symbols, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st,initDate = initDate, currency = 'USD', 
         initEq=initEq)
initOrders(portfolio.st, initDate=initDate)
strategy(strategy.st, store = TRUE)
#parameters
nRSI = 2
thresh1 = 10
thresh2 = 6

nSMAexit = 5
nSMAfilter = 200

period = 10
pctATR = 0.02
# This is the trade size.  It is linked to volatility and is designed
# to make sure that the risk is stable. There are larger trades when there
# are larger moves. 
maxPct = 0.04
# indicators
add.indicator(strategy.st, name ="lagATR", 
              #lagATR is in the IKTrading library.  It will calculate the ATR and it will 
              # lag it so that we have yesterday's ATR to use for today.
              # the name is a function that is to be computed. 
              arguments = list(HLC=quote(mktdata)), n = period, 
              # ATR will take the high, low close and the label will allow this to be 
              # used later.
              label = "atrx")

add.indicator(strategy.st, name = "RSI", 
              arguments = list(price = quote(Cl(mktdata)), n = nRSI), 
              label = "rsi")
# quantmode convention says that the price must be a 'quote'
add.indicator(strategy.st, name = 'SMA', 
              arguments = list(price = quote(Cl(mktdata)), n = nSMAexit),
              label = 'quickSMA')
add.indicator(strategy.st, name = 'SMA', 
              arguments = list(price = quote(Cl(mktdata)), n = nSMAfilter), 
              label = 'filterMA')
###Signals

add.signal(strategy.st, name = 'sigComparison', 
           arguments = list(column = c('rsi', 'filterMA'), 
                            relationship = 'gt'), label = 'pptrend')
# gt stands for greater than.  There are a few of these
add.signal(strategy.st, name = 'sigThreshold', 
           arguments = list(column = 'rsi', threshold = 'thresh1',
                            relationship = 'lt', cross = FALSE), 
           label = 'rsiThresh1')
# cross is false because we want a trigger even if there is a 
add.signal(strategy.st, name = 'sigThreshold', 
           arguments = list(column = 'rsi', threshold = 'thresh2',
                            relationship = 'lt', cross = FALSE), 
           label = 'rsiThresh2')
# These strategies only trigger if all the components are true. 
add.signal(strategy.st, name = 'sigAND', 
           arguments = list(columns = c('rsiThresh1', 'upTrend'), 
                            cross = TRUE), label = 'longEntry1')
add.signal(strategy.st, name = 'sigAND',
           arguments = list(columns = c('rsiThresh2', 'upTrend'), 
                            cross = TRUE),
           label = 'longEntry2')
add.signal(strategy.st, name = 'sigCrossover', 
           arguments = list(columns = c('Close', 'quickMA'), 
                            relationship = 'gt'), 
           label = 'exitLonNormal')
add.signal(strategy.st, name = 'sigCrossover', 
           arguments = list(columns = c('close', 'filterMA'), 
                            relationship = 'lt'), 
           label = 'exitLongFilter')
#This is an exit if the close is back below the 200 day MA. 

### Rules
add.rule(strategy.st, name = 'ruleSignal', 
         arguments = list(sigcol = 'longEntry1', 
                          sigval = TRUE, 
                          # this will usually be the case
                          ordertype = 'market',
                          #it is possibe to have limit and stop
                          orderside = 'long',
                          replace = FALSE, 
                          # true will overide other rules for the day
                          prefer = 'open',
                          # this it to ensure that you trade after the signal
                          OSFUN = osDollarATR, 
                          # ordersize function.  Can be constant but..
                          # this comes from the IKTrading package
                          # it will look for a ATR column pctATR in our case
                          # and use that to calculate the size of order
                          tradesize = tradeSize,
                          pctATR = pctATR, 
                          maxpctATR = pctATR,
                          # this will limit the size of the overall position
                          atrMod = 'X'), 
         type = 'enter', path.dep = TRUE, label = 'enterLong1')
# this label refers to the label for the signal. 
add.rule(strategy.st, name = 'ruleSignal',
         arguments = list(sfgcol = 'longEntry2', 
                          sigval = TRUE,
                          ordertype = 'long',
                          replace = FALSE, 
                          prefer = 'open',
                          OSFUN = osDollarATR, 
                          tradeSize = tradeSize, 
                          pctATR = pctATR, 
                          maxpcATR = maxPct, 
                          atrMod = 'X'), 
         type = 'enter', path.dep = TRUE, label = 'enterLong2')

add.rule(strategy.st, name = 'ruleSignal', 
         arguments = list(sigcol = 'exitLongNormal', 
                          sigval = TRUE, 
                          orderqty = 'all', 
                          # get rid of all the position
                          # does not try to scale the order
                          ordertype = 'market', 
                          orderside = 'long', 
                          replace = FALSE, 
                          prefer = 'open'), 
         type = 'exit', path.dep = TRUE, label = 'normalExitLong')
# type will just exit.  It is possible to rebalance, but that is not used here

add.rule(strategy.st, name = 'ruleSignal', 
         arguments = list(sigcol = 'exitLongFilter', 
                          sigval = TRUE, 
                          orderqty = 'all',
                          ordertype = 'market',
                          orderside = 'long', 
                          replace = FALSE, 
                          prefer = 'open'), 
         type = 'exit', path.dep = TRUE, label = 'filterExitLong')
# labels are important in quantstrat.  These are not neede dhere but are 
# important if using stock loss. 

t1 <- Sys.time()
out <- applyStrategy(strategy = strategy.st, mktdata = SPY, portfolios = portfolio.st)
t2 <- Sys.time()
print(t2-t1)
# set up analytics
updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st, dateRange)
updateEndEq(account.st)

 
