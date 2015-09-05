


library(quantmod)
bitcoin <- read.table("D:\\Bitcoin.csv", header = T, sep = ";", row.names = 1)
bitcoin <- tail(bitcoin, 150)
bitcoin <- as.xts(bitcoin)
windows(20,10)
chartSeries(bitcoin, dn.col = "red", TA="addRSI(10);addEMA(10)")
windows(20,10)
chartSeries(bitcoin, dn.col = "red", TA="addMACD();addSMA(10)")











