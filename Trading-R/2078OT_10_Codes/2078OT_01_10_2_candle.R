
OHLC <- function(d){
k = nrow(d)
windows(width=30, height=15)
par(bg = rgb(.9,.9,.9)); 
plot(x=NULL, y = NULL, xlim=c(1,k+1), ylim=c(min(d),max(d)), xlab="", ylab="", xaxt = "n")
abline(h = axTicks(2), v=axTicks(1), col = rgb(.5,.5,.5), lty=3); 
axis(1, at = axTicks(1), las = 1, cex.axis = 0.6)
for(i in 1:k){polygon(c(i,i+1,i+1,i), c(d[i,1], d[i,1], d[i,4], d[i,4]), col = ifelse(d[i,1]<=d[i,4], rgb(0,0.8,0), rgb(0.8,0,0)))
lines(c(i+1/2, i+1/2), c(d[i,2], max(d[i,1], d[i,4])));lines(c(i+1/2, i+1/2), c(d[i,3], min(d[i,1], d[i,4])))}
}

#trendfoduló keresés:

is.trend <- function(ohlc,i,j){
avg1 = mean(ohlc[(i-25):i,4])
avg2 = mean(ohlc[(j-25):j,4])
if(avg1 >= avg2) return(FALSE)
ohlc <- ohlc[i:j,]
n <- nrow(ohlc)
candle_l <- pmin(ohlc[,1],ohlc[,4])
valley <- rep(FALSE, n)

for(k in 2:(n-1)){valley[k] <- ( (candle_l[k-1] >= candle_l[k]) & (candle_l[k+1] >= candle_l[k]) )}
z = candle_l[valley]
if(all(z == cummax(z))) return(TRUE)
return(FALSE)
}

is.trend.rev <- function(ohlc,i,j){
if(is.trend(ohlc,i,j) == FALSE) return(FALSE)

last_candle <- ohlc[j+1,]
reverse_candle <- ohlc[j+2,]
ohlc <- ohlc[i:j,]

if( last_candle[4] < last_candle[1]) return(FALSE)
if( last_candle[4] < max(ohlc[,c(1,4)]) ) return(FALSE)
if( reverse_candle[1] < last_candle[4] | reverse_candle[4] >= last_candle[1] ) return(FALSE)
return(TRUE)

}

bitcoin <- read.table("D:\\Bitcoin.csv", header = T, sep = ";")
n <- nrow(bitcoin)

result <- c(0,0)
for (a in 26:726){
for (b in (a+3):min(n-3,a+100)){

if( is.trend.rev(bitcoin, a,b) & b-a > 10 ) {
result <- rbind(result, c(a,b)) }
if(b==n) break
}}

z=aggregate(result, by = list(result[,2]), FUN = min)[-1, 2:3]
for (h in 1:nrow(z)) {OHLC(bitcoin[z[h,1]:z[h,2]+2,]); title(main = z[h,])}




