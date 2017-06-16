# blotter example
require(FinancialInstrument)
#define currency and stocks
currency("USD")
symbols = c("IBM", "F")
for(symbol in symbols){
  stock(symbol, currency = "USD", multiplier = 1)
}
#download price data
require(quantmod)
getSymbols(symbols, from = '2007-01-01', to = '2007-01-31', src = 'yahoo', 
           index.clas = c('POSIXt', 'POSIXct'))
#initialise portfoil object 'p'
print('Creating Portfolio \"p\"...')
initPortf('p', symbols = symbols, currency = "USD")
#Make a couple of trade in IBM
addTxn(Portfolio = "p", Symbol = "IBM", TxnDate = '2007-01-03', 
       TxnQty = 50, TxnPrice = 96.5, TxnFees = -0.05*50)
addTxn("p", "IBM", '2007-01-04', 50, 97.1, TxnFees = -0.05*50)
# a few in F
addTxn("p", "F", '2007-01-03', -100, 7.60, TxnFees = pennyPerShare(-100))
addTxn("p", "F", '2007-01-10', 50, 7.78, TxnFees = pennyPerShare(50))
# Add MMM
getSymbols("MMM", from = '2007-01-01', to = '2007-01-31', src = 'yahoo', 
           index.class = c("POSIXct", "POSIXct"))
stock("MMM", currency = "USD", multiplier = 1)
addTxn("p", "MMM", '2007-01-05', -50, 77.9, TxnFees = -0.05*50)
addTxn("p", "MMM", '2007-01-08', 50, 77.6, TxnFees = -0.05*50)
addTxn("p", "MMM", '2007-01-09', 50, 77.6, TxnFees = -0.05*50)
print('Updating portfolio \"p\"...')
updatePortf(Portfolio = "p", Dates = '2007-01')
print('Creating account \"a\" for portfolio \"p\"...')
initAcct(name = "a", portfolios = "p", initEq = 10000, currency = "USD")
print('Updated account \"a\"...')
updateAcct("a", '2007-01')
updateEndEq("a", '2007-01')
PortfReturns("a", '2007-01')
#Examine COntents of the Portfolio
## Here are the transaction record
getTxns(Portfolio = "p", Symbol = "MMM", Date = '2007-01')
getTxns(Portfolio = "p", Symbol = "MMM", Date = "2007-01-03::2007-01-05")
# Positions
getPos(Portfolio = "p", Symbol = "MMM", Dates = "2007-01")
getPos(Portfolio = "p", Symbol = "MMM", Date = "2007-01-05")
getPosQty(Portfolio = "p", Symbol = "MMM", Date = "2007-01")
# Not sure why two of the three above do not work.  Is it the date? 
chart.Posn(Portfolio = "p", Symbol = "MMM", Dates = "2007-01")
# copy objects to local workspace
p = getPortfolio("p")
a = getAccount("a")

p$symbol$MMM$txn
p$symbols$MMM$posPL
p$symbols$MMM$postPL.USD
