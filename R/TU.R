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
for(i in 1:dim(da)[1] - lookback){
  longs <- da$Close[i + lookback] > da$Close[i]
}
for(i in 1:dim(da)[1] - lookback){
  shorts <- da$Close[i + lookback] < da$Close[i]
}
# This is the first chapter of Alog trading book.  