Apple <- read.table("Apple.csv", header = T, sep = ";")
r <- log(head(Apple$Price,-1)/tail(Apple$Price,-1))
m <- mean(r)
s <- sd(r)
VaR1 <- -qnorm(0.05, m, s)
print(VaR1)


VaR2 <- -quantile(r, 0.05)
print(VaR2)


sim_norm_return <- rnorm(10000, m, s)
VaR3 <- -quantile(sim_norm_return, 0.05)
print(VaR3)


sim_return <- r[ceiling(runif(10000)*251)]
VaR4 <- -quantile(sim_return, 0.05)
print(VaR4)


library(fOptions)

X <- 100
Time <- 2
r <- 0.1
sigma <- 0.3
mu <- 0.2
S <- seq(1,200, length = 1000)
call_price <- sapply(S, function(S) GBSOption("c", S, X, Time, r, r, 
  sigma)@price)
plot(S, call_price, type = "l", ylab = "", main = "Call option price 
  in function of stock prompt price")


X <- 100
Time <- 2
r <- 0.1
sigma <- 0.3
mu <- 0.2
S <- seq(1,200, length = 1000)
call_price <- sapply(S, function(S) GBSOption("c", S, X, Time, r, r, sigma)@price)
put_price <- sapply(S, function(S) GBSOption("p", S, X, Time, r, r, sigma)@price)
portfolio_price <- call_price + put_price
windows()
plot(S, portfolio_price, type = "l", ylab = "", main = "Portfolio 
  price in function of stock prompt price")
# portfolio VaR simulation
p0 <- GBSOption("c", 100, X, Time, r, r, sigma)@price + 
  GBSOption("p", 100, X, Time, r, r, sigma)@price
print(paste("price 
    of portfolio:",p0))
S1 <- 100*exp(rnorm(10000, mu - sigma^2 / 2 , sigma))
P1 <- sapply(S1, function(S) GBSOption("c", S, X, 1, r, r, 
  sigma)@price + GBSOption("p", S, X, 1, r, r, sigma)@price )
VaR <- quantile(P1, 0.05)
print(paste("95% VaR of portfolio: ", p0 - VaR))



install.packages("fOptions")
library(fOptions)
kmv_error <- function(V_and_vol_V, E=3,Time=1,D=10,vol_E=0.8,r=0.05){
  V <- V_and_vol_V[1]
  vol_V <- V_and_vol_V[2]
  E_ <- GBSOption("c", V, D, Time, r, r, vol_V)@price
  tmp <- vol_V*sqrt(Time)
  d1 <- log(V/(D*exp(-r*Time)))/tmp + tmp/2
  Nd1 <- pnorm(d1)
  vol_E_ <- Nd1*V/E*vol_V
  err <- c(E_ - E, vol_E_ - vol_E)
  err[1]^2+err[2]^2
}
a <- optim(c(1,1), fn = kmv_error)
print(a)



op <- function(){
n <- rpois(1, 20)
z <- rlnorm(n,5,2)
sum(z)
}
Loss <- replicate(10000, op())
hist(Loss[Loss<50000], main = "", breaks = 20, xlab = "", ylab = "")
print(paste("Expected loss = ", mean(Loss)))
print(paste("99.9% quantile of loss = ", quantile(Loss, 0.999)))
