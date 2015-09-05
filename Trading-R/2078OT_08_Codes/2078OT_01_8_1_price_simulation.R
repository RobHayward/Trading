set.seed(2014)
library(fOptions)

Price_simulation = function(S0, mu, sigma, rf, K, Time,  dt, plots = FALSE ){
  
  t <- seq(0, Time, by = dt)
  N <- length(t)
  
  W <- c(0,cumsum(rnorm(N-1)))
  S <- S0*exp((mu-sigma^2/2)*t + sigma*sqrt(dt)*W)
  
  delta <- rep(0, N-1)
  call_ <- rep(0, N-1)
  
  for(i in 1:(N-1) ){
    delta[i] <- GBSGreeks("Delta", "c", S[i], K, Time-t[i], rf, rf, sigma)
    call_[i] <- GBSOption("c", S[i], K, Time-t[i], rf, rf, sigma)@price}
  
  if(plots){
    dev.new(width=30, height=10)
    par(mfrow = c(1,3))
    plot(t, S, type = "l", main = "Price of underlying")
    plot(t[-length(t)], delta, type = "l", main = "Delta", xlab = "t")
    plot(t[-length(t)], call_ , type = "l", main = "Price of option", xlab = "t")
  }
}

Price_simulation(100, 0.2, 0.3, 0.05, 100, 0.5, 1/250, plots = TRUE)
