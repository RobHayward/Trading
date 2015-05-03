require(quantmod)
require(TTR)
getSymbols("BAC")
head(da)
da$S <- SMA(da$BAC.Close, n = 3)
plot(da$BAC.Close)
lines(da$s, tpe = 'l', col = 'red')
  