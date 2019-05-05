# MACD.R
# This will implement and post the MACD
require(quantmod)
da <- getSymbols('SPY', auto.assign = FALSE)
head(da)
head(da)
chart_Series(da$SPY.Close, type = 'candelsticks', col = 'Black')
MACD_series <- MACD(da$SPY.Close)
head(MACD_series)
tail(MACD_series)
plot(MACD_series, type = 'l')
#======================================
library(quantmod)
library(tseries)
par(mfrow = c(2,1))
da1 <- da[(da$Date  > as.Date("2014-01-01", format = "%Y-%m-%d") 
           & da$Date < as.Date("2015-03-15", format = "%Y-%m-%d")),]
da1 <- da1[rev(rownames(da1)),]
dat <- as.ts(da1[,c(2:5)])
plotOHLC(dat, origin = "2014-12-25", ylab = "SP500", main = "SP500", xlab = "", xaxt = "n") 
RSI_series <- RSI(dat[,1])
plot(RSI_series)
abline(h = 30, col = 'red', lty = 2)
abline(h = 70, col = 'red', lty = 2)

