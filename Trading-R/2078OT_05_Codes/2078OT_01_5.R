library(Quandl)
library(xts)


EURUSD <- Quandl("QUANDL/EURUSD", start_date="2014-01-01",end_date="2014-07-01", type="xts")
USDEUR <- Quandl("QUANDL/USDEUR", start_date="2014-01-01",end_date="2014-07-01", type="xts")

windows(width = 15, height = 8)
par(mfrow = c(1, 2))

plot(USDEUR)
plot(EURUSD)

print(USDEUR[1:5,])
print(EURUSD[1:5,])

######################################################################################################################

Margrabe <- function(S1, S2, sigma1, sigma2, Time, rho, delta1 = 0, delta2 = 0){

sigma <- sqrt(sigma1^2 + sigma2^2 - 2*sigma1*sigma2*rho)

d1 <- ( log(S1/S2) + ( delta2-delta1 + sigma^2/2 ) * Time ) / (sigma*sqrt(Time))

d2 <- ( log(S1/S2) + ( delta2-delta1 - sigma^2/2 ) * Time ) / (sigma*sqrt(Time))

M <- S1*exp(-delta1*Time)*pnorm(d1) - S2*exp(-delta2*Time)*pnorm(d2)

return(M)

}



####################################################################### 

Correlated_Wiener <- function(cor){
windows(200, 75)
par(mfrow = c(1, 3), oma = c(0, 0, 2, 0))
for(i in 1:3){
    W1 <- cumsum(rnorm(100000))
    W2 <- cumsum(rnorm(100000))
    W3 <- cor*W1+sqrt(1-cor^2)*W2
    plot(W1,W3, type= "l", ylab = "", xlab = "")
              }
mtext("Correlated Wiener-processes", outer = TRUE, cex = 1.5, line = -1)
}

D2_Wiener <- function(){
windows(200, 75)
par(mfrow = c(1, 3), oma = c(0, 0, 2, 0))
for(i in 1:3){
    W1 <- cumsum(rnorm(100000))
    W2 <- cumsum(rnorm(100000))
    plot(W1,W2, type= "l", ylab = "", xlab = "")
              }
mtext("2-dimensional Wiener-processes", outer = TRUE, cex = 1.5, line = -1)
}











