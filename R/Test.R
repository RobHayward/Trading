require(quantmod)
require(PerformanceAnalytics)
# don't use this as you get more data by using the downloaded data
# getSymbols("TU")
# head(TU)
da <- read.csv("Data/TU.csv")
da$Date <- as.Date(da$Date, format = "%Y-%m-%d")
head(da)
str(da)
xtsible(da)
dax <- as.xts(da[,2:dim(da)[2]], order.by = da[,1])
str(dax)
head(dax)
dax$ret <- Return.calculate(dax$Close)
lookback <- 250
holddays <- 25
longs <- rep(NA, dim(da)[1] - lookback - holddays)
shorts <- rep(NA, dim(da)[1] - lookback - holddays)
for(i in seq(lookback + 1, dim(da)[1] - holddays)){
  longs[i] <- da$Close[i] > da$Close[i - lookback]
}
for(i in seq(lookback + 1, dim(da)[1] - holddays)){
  shorts[i] <- da$Close[i] < da$Close[i - lookback]
}
pro <- rep(seq(lookback, dim(dax)[1] - holddays))
for(i in seq(lookback, dim(dax)[1] - holddays)){
  if (identical(longs[i], TRUE)){
  pro[i] <- sum(dax$ret[seq(i, i + holddays)], na.rm = TRUE) 
  } else if(identical(shorts[i], TRUE)){
   pro[i] <- -sum(dax$Close[seq(i, i + holddays)], na.rm = TRUE)
 } else {
  pro[i] <- 0
}
}
---------------
  # This does not work.  I am not sure why this does not work. There seem to be e
  # extra ordinary losses. 
# Random number generation with randtoolbox package
