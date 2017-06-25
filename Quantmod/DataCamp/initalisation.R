# This is the standard initialisation file
tradeaize = 1000000
initeq = 1000000

strategy.st = portfolio.st = account.st <- "firststrat"
rm.strat(stratgy.st)
initPortf(portfolio.st, symbols = "LQD", initDate = initdate, currency= "USD")
initAcc(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", 
        initEq = initeq)
initOrders(portfolio.st, initDate = initdate)
strategy(strategy.st, store = TRUE)