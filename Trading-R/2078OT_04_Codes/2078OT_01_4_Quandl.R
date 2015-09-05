install.packages(Quandl") 
library(Quandl)
library(xts)

currencies <- c( "USD", "CHF", "GBP", "JPY", "RUB", "CAD", "AUD")
currencies <- paste("QUANDL/EUR", currencies, sep = "")
currency_ts = lapply(as.list(currencies), Quandl, start_date="2005-01-01",end_date="2013-06-07", type="xts")

Q <- cbind(currency_ts[[1]]$Rate,currency_ts[[3]]$Rate,currency_ts[[6]]$Rate,currency_ts[[7]]$Rate)

matplot(Q, type = "l", xlab = "Time", ylab = "FX rate", main = "Exchange rates	", xaxt = 'n', yaxt = 'n' )
ticks = axTicksByTime(currency_ts[[1]])
abline(v = ticks,h = seq(min(Q), max(Q), length = 5), col = "grey", lty = 4)
axis(1, at = ticks, labels = names(ticks))
axis(2, at = seq(min(Q), max(Q), length = 5), labels = round(seq(min(Q), max(Q), length = 5), 1))


