# TU hypothesis testing
require(dplyr)
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
for(i in lookback:dim(da)[i]){
  if(longs == TRUE){
  ret[i] <- sum(da$dClose[i]:da$dClose[i + holddays] 
  }
 if(short == TRUE){
   ret[i] <- -sum(da$Close[i]:da$Close[i + holddays])
 }  
else{
  ret[i] <- 0
}
