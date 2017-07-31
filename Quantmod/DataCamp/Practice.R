# This is the standard initialisation file
tradeaize = 1000000
initeq = 1000000
initdate = "01-01-1995"
strategy.st = portfolio.st = account.st <- "firststrat"
rm.strat(stratgy.st)
initPortf(portfolio.st, symbols = "LQD", initDate = initdate, currency= "USD")
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", 
        initEq = initeq)
initOrders(portfolio.st, initDate = initdate)
strategy(strategy.st, store = TRUE)
add.indicator(strategy = strategy.st, name = "SMA", arguments = list(x = quote(Cl(mkdata)), n = 200), label  = "SMA200")
