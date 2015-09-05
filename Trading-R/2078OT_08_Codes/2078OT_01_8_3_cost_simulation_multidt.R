library(fOptions)

cost_simulation = function(S0, mu, sigma, rf, K, Time, dt, periods){
  
  t <- seq(0, Time, by = dt)
  N <- length(t)
  W = c(0,cumsum(rnorm(N-1)))
  S <- S0*exp((mu-sigma^2/2)*t + sigma*sqrt(dt)*W)
  SN = S[N]
  
  delta <- rep(0, N-1)
  call_ <- rep(0, N-1)
  
  for(i in 1:(N-1) ){
    delta[i] <- GBSGreeks("Delta", "c", S[i], K, Time-t[i], rf, rf, sigma)
    call_[i] <- GBSOption("c", S[i], K, Time-t[i], rf, rf, sigma)@price
  }
  
  S = S[seq(1, N-1, by = periods)]
  delta = delta[seq(1, N-1, by = periods)]
  
  m = length(S)
  
  share_cost <- rep(0,m)
  interest_cost <- rep(0,m)
  total_cost <- rep(0, m)
  
  share_cost[1] <- S[1]*delta[1]
  interest_cost[1] <- (exp(rf*dt*periods)-1) * share_cost[1]
  total_cost[1] <- share_cost[1] + interest_cost[1]
  
  
  for(i in 2:(m)){
    share_cost[i] <- ( delta[i] - delta[i-1] ) * S[i]
    interest_cost[i] <- ( total_cost[i-1] + share_cost[i] ) * (exp(rf*dt*periods)-1)
    total_cost[i] <- total_cost[i-1] + interest_cost[i] + share_cost[i]
  }
  
  c = max( SN - K , 0)
  
  cost = c - delta[m]*SN + total_cost[m]                         
  
  return(cost*exp(-Time*rf))
  
}
dev.new(width=30,height=20)
par(mfrow = c(2,3))
i = 0
per = c(2,4,8,20,40,80) 
call_price = GBSOption("c", 100, 100, 0.5, 0.05, 0.05, 0.3)@price
results = matrix(0, 6, 5)
rownames(results) = c("1/2 days", "1 day", "2 days", "1 week", "2 weeks", "4 weeks")
colnames(results) = c("E", "lower", "upper", "v", "ratio")
for (j in per){
  i = i+1
  A = rep(0, 1000)
  set.seed(10125987)
  for (h in 1:1000){A[h] = cost_simulation(100, .20, .30,.05, 100, 0.5, 1/1000,j)}
  
  E = mean(A)
  v = sd(A)
  results[i, 1] = E
  results[i, 2] = E-1.96*v/sqrt(1000)
  results[i, 3] = E+1.96*v/sqrt(1000)
  results[i, 4] = v
  results[i, 5] = v/call_price
  hist(A, freq = F, main = "", xlab = "", xlim = c(4,16), ylim = c(0,0.8))
  title(main = rownames(results)[i], sub = paste("E = ",round(E, 4) ,"  sd = ",round(v, 4)))
  curve(dnorm(x, mean=mean(A), sd=sd(A)), col="darkblue", lwd=2, add=TRUE, yaxt="n")
}
print(results)
dev.new()
curve(dnorm(x,results[1,1], results[1,4]), 6,14, ylab = "", xlab = "cost")
for (l in 2:6) curve(dnorm(x, results[l,1], results[l,4]), add = TRUE, xlim = c(4,16), ylim = c(0,0.8), lty=l)
legend(legend=rownames(results), "topright", lty = 1:6)
