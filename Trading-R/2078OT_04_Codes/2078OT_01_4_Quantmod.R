library(quantmod)
bmw_stock<- new.env()
getSymbols("BMW.DE", env = bmw_stock, src = "yahoo", from = as.Date("2010-01-01"), to = as.Date("2013-12-31"))
BMW<-bmw_stock$BMW.DE
head(BMW)
chartSeries(BMW,multi.col=TRUE,theme="white")
addMACD() 
addBBands()
BMW_return <-= 
log(BMW$BMW.DE.Close/BMW$BMW.DE.Open)
qqnorm(BMW_return, main = "Normal Q-Q Plot of BMW daily log return",
 xlab = "Theoretical Quantiles",
        ylab = "Sample Quantiles", plot.it = TRUE, datax = FALSE
 )
qqline(BMW_return, col="red")
