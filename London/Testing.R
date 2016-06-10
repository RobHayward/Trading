library(quantmod)
da <- read.csv("./London/EXUSEU.csv", stringsAsFactors = FALSE)
da$DATE = as.Date(da$DATE, format = "%d/%m/%Y")
tail(da)
# To use quantmod chart
#da$VALUE <- as.xts(da$VALUE, order.by = da$DATE)
#chartSeries(da$VALUE, TA = "EMA")
plot(da, type = 'l', ylab = "Dollars per EUR", main ="EUR-USD")
x <- c(as.Date("2008-07-30"), as.Date("2016-05-20"))
y <- c(1.5730, 1.31)
lines(x, y, col = "DarkGreen")

