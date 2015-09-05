
Time__ <- 10 # ennyi perces gyertyákat rajzol

get_price <- function(stock_symbol = "c"){
url_ <- paste("http://www.nasdaq.com/symbol/", stock_symbol, sep = "")
a <- scan(file = url_, what = "character")
a <- paste(a, collapse = "")
n <- as.numeric(regexpr("id=\"qwidget_lastsale\"", a))
a <- substr(a, n, n+100)
n <- as.numeric(regexpr(">", a))
m <- as.numeric(regexpr("<", a))
a <- substr(a, n+2, m-1)
as.numeric(a)
}

DrawChart <- function(time_frame_in_minutes, number_of_candles = 25, symbol = "c", l = 50, u = 51){

OHLC = matrix(NA, 4, number_of_candles)
OHLC[,number_of_candles] <- get_price(symbol)
windows(width=30, height=15)

par(bg = rgb(.9,.9,.9))
plot(x=NULL, y = NULL, xlim=c(1,number_of_candles+1), ylim=c(l,u), xlab="", ylab="", xaxt = "n", yaxt = "n")
abline(h = axTicks(2), v=axTicks(1), col = rgb(.5,.5,.5), lty=3); 
axis(1, at = axTicks(1), las = 1, cex.axis = 0.6)
axis(2, at = axTicks(2), las = 1, cex.axis = 0.6)
box()
allpars=par(no.readonly = TRUE)

while(T){
start_ <- Sys.time()
while(as.numeric(difftime(Sys.time(), start_, units = "mins")) < time_frame_in_minutes){
OHLC[4,number_of_candles] <- get_price(symbol)
OHLC[2,number_of_candles] <- max(OHLC[2,number_of_candles], OHLC[4,number_of_candles])
OHLC[3,number_of_candles] <- min(OHLC[3,number_of_candles], OHLC[4,number_of_candles])



frame()
par(allpars) 
abline(h = axTicks(2), v=axTicks(1), col = rgb(.5,.5,.5), lty=3); 
axis(1, at = axTicks(1), las = 1, cex.axis = 0.6);
axis(2, at = axTicks(2), las = 1, cex.axis = 0.6);
box()
for(i in 1:number_of_candles){polygon(c(i,i+1,i+1,i), c(OHLC[1,i], OHLC[1,i], OHLC[4,i], OHLC[4,i]), col = ifelse(OHLC[1,i]<=OHLC[4,i], rgb(0,0.8,0), rgb(0.8,0,0)))
lines(c(i+1/2, i+1/2), c(OHLC[2,i], max(OHLC[1,i], OHLC[4,i])));lines(c(i+1/2, i+1/2), c(OHLC[3,i], min(OHLC[1,i], OHLC[4,i])))}
abline(h = OHLC[4,number_of_candles], col = "violet", lty = "dashed")

}

OHLC <- OHLC[,2:number_of_candles]
OHLC <- cbind(OHLC, NA)
OHLC[1,number_of_candles] <- OHLC[4,number_of_candles-1]



}}



DrawChart(Time__,50)






 



