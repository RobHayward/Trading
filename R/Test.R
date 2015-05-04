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
longs <- rep(NA, dim(da)[1] + 1 - lookback - holddays)
shorts <- rep(NA, dim(da)[1] + 1 - lookback - holddays)
for(i in lookback + 1:dim(da)[1]){
  longs[i] <- da$Close[i] > da$Close[i + 1 - lookback]
}
for(i in lookback + 1:dim(da)[1]){
  shorts[i] <- da$Close[i] < da$Close[i + 1 - lookback]
}
pro <- rep(NA, dim(dax)[1])
for(i in lookback:dim(dax)[1] - holddays){
  if (longs[i] == TRUE){
  pro[i] <- sum(dax$ret[i]:dax$ret[i + holddays], na.rm = TRUE) 
  } else if(shorts[i] == TRUE){
   pro[i] <- -sum(dax$Close[i]:dax$Close[i + holddays], na.rm = TRUE)
 } else {
  pro[i] <- 0
}
}
#----------------------
shorts[lookback +1]
for(i in lookback + 1: dim(da)[1] - lookback){
  print(i)
}
shorts[lookback +1] <- da$Close[lookback + 1]  < da$Close[lookback + 1 - lookback]
longs[2] <- da$Close[3] > da$Close[2]
head(longs)
ret <- function(start, end){
  ret[i] <- sum(start:end)
}
a <- seq(1, 10, 1)
for(i in 1:length(a)){
  if(a[i] == 3){
print(1)
}  else if (a[i] ==1){
print(3)
}
else {
  print(2)
  }
}
str(longs)
