# TU hypothesis testing
require(dplyr)
require(quantmod)
require(PerformanceAnalytics)
da <- read.csv("Data/TU.csv")
da$Date <- as.Date(da$Date, format = "%Y-%m-%d")
# turn the dates round so that lagging can be carried out
da <- da[order(da$Date),]
head(da)
lookback <- 250
holddays <- 25
pos <- rep(0, dim(da)[1])
da <- da %>% 
  mutate(Closel = lag(Close)) %>% 
  mutate(dClose = log(Close) - log(Closel))
head(da)
tail(da)
longs <- rep(NA, dim(da)[1] + 1 - lookback)
shorts <- rep(NA, dim(da)[1] + 1 - lookback)
for(i in lookback:dim(da)[1]){
  longs[i] <- da$Close[i] > da$Close[i + 1 - lookback]
}
for(i in lookback:dim(da)[1]){
  shorts[i] <- da$Close[i] < da$Close[i + 1 - lookback]
}
da <- da[lookback:dim(da)[1],]
head(da)
tail(da)
longs
# This is the first chapter of Alog trading book.  
# When long or short is true, add the returns 
ret <- rep(NA, dim(da)[1])
for(i in lookback:dim(da)[1] - holddays){
  if(longs[i] == TRUE){
  ret[i] <- sum(da$dClose[i]:da$dClose[i + holddays], na.rm = TRUE) 
  } else if(shorts[i] == TRUE){
   ret[i] <- -sum(da$dClose[i]:da$dClose[i + holddays], na.rm = TRUE)
 } else {
  ret[i] <- 0
}
}
ret <- function(start, end){
  ret[i] <- sum(start:end)
}
ret1 <- apply(ret, da$dclose[lookback:dim(da)[1] - holddays],
              da$close[lookback + ho])
da$ret <- annualReturn(da$Close)
dat <- as.xts(da, order.by = da$Date)
head(dat)
head(da)
class(dat)
class(dat[1])
str(dat)
