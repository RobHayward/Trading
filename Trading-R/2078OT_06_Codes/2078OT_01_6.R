

CIR_pdf = function(x, alpha, beta, sigma, delta_T, r0 = 0.1){
q = (2*alpha*beta)/(sigma^2) - 1
c = (2*alpha)/(sigma^2*(1-exp(-alpha*delta_T)))
u = c*r0*exp(-alpha*delta_T)
y = dchisq(2*c*x, df = 2*q+2, ncp = 2*u)
}


x <- seq(0, 0.15, length = 1000)
y1 <- CIR_pdf(x, .3, 0.05, 0.1, 1)
y2 <- CIR_pdf(x, .3, 0.05, 0.1, 2)
y3 <- CIR_pdf(x, .3, 0.05, 0.1, 5)
y4 <- CIR_pdf(x, .3, 0.05, 0.1, 50)

par(mar = c(2,2,2,2), mfrow = c(2,2))  
matplot(x, cbind(y1,y2,y3,y4), type = "l",ylab ="",xlab = "", col = 1:5, lty = 1)
legend("topright", c("T-t = 1", "T-t = 2", "T-t = 5", "T-t = 50"), fill = 1:5, cex = 0.7)

y1 <- CIR_pdf(x, .2, 0.05, 0.1, 1)
y2 <- CIR_pdf(x, .4, 0.05, 0.1, 1)
y3 <- CIR_pdf(x, .6, 0.05, 0.1, 1)
y4 <- CIR_pdf(x,  1, 0.05, 0.1, 1)
  
matplot(x, cbind(y1,y2,y3,y4), type = "l",ylab ="",xlab = "", col = 1:5, lty = 1)
legend("topright", c("alpha = 0.2", "alpha = 0.4", "alpha = 0.6", "alpha = 1"), fill = 1:5, cex = 0.7)

y1 <- CIR_pdf(x, .3, 0.10, 0.1, 1)
y2 <- CIR_pdf(x, .3, 0.12, 0.1, 1)
y3 <- CIR_pdf(x, .3, 0.14, 0.1, 1)
y4 <- CIR_pdf(x, .3, 0.16, 0.1, 1)
  
matplot(x, cbind(y1,y2,y3,y4), type = "l",ylab ="",xlab = "", col = 1:5, lty = 1)
legend("topright", c("beta = 0.1", "beta = 0.12", "beta = 0.14", "beta = 0.16"), fill = 1:5, cex = 0.7)

x <- seq(0, 0.25, length = 1000)
y1 <- CIR_pdf(x, .3, 0.05, 0.03, 1)
y2 <- CIR_pdf(x, .3, 0.05, 0.05, 1)
y3 <- CIR_pdf(x, .3, 0.05, 0.10, 1)
y4 <- CIR_pdf(x, .3, 0.05, 0.15, 1)
  
matplot(x, cbind(y1,y2,y3,y4), type = "l",ylab ="",xlab = "", col = 1:5, lty = 1)
legend("topright", c("sigma = 3%", "sigma = 5%", "sigma = 10%", "sigma = 15%"), fill = 1:5, cex = 0.7)

####################################################################################################################################


vasicek = function(alpha, beta, sigma, n = 1000, r0 = 0.05){
v <- rep(0, n)
v[1] = r0
for (i in 2:n){
    v[i] <- v[i-1]+alpha*(beta - v[i-1]) + sigma*rnorm(1)
          }
     return(v)
}


r = matrix(0, 1000, 3)
set.seed(2014)
r[,1] <- vasicek(0.002, 0.065, 0.0002)
set.seed(2014)
r[,2] <- vasicek(0.02, 0.065, 0.0002)
set.seed(2014)
r[,3] <- vasicek(0.2, 0.065, 0.0002)

matplot(r, type = "l", ylab = "", xaxt = "no",  main = "Vasicek trajectories with alpha = 0.2%, 2% and 20%")
lines(c(-1,1001), c(0.065, 0.065), col = "grey", lwd = 2, lty = 3)

############################################################################################################################




vasicek_pdf = function(x, alpha, beta, sigma, delta_T, r0 = 0.05){
e <- r0*exp(-alpha*delta_T)+beta*(1-exp(-alpha*delta_T))
s <- sigma^2/(2*alpha)*(1-exp(-2*alpha*delta_T))
y <- dnorm(x, mean = e, sd = s)
return(y)
}

x <- seq(-0.1, 0.2, length = 1000)
y1 <- vasicek_pdf(x, .2, 0.1, 0.15,10)
y2 <- vasicek_pdf(x, .2, 0.1, 0.15, 5)
y3 <- vasicek_pdf(x, .2, 0.1, 0.15, 3)
y4 <- vasicek_pdf(x, .2, 0.1, 0.15, 2)

par(xpd = T ,mar = c(2,2,2,2), mfrow = c(2,2))  
matplot(x, cbind(y1,y2,y3,y4), type = "l",ylab ="",xlab = "", col = 1:5, lty = 1)
legend("topleft", c("T-t = 2", "T-t = 3", "T-t = 5", "T-t = 10"), fill = 1:5, cex = 0.7)

y1 <- vasicek_pdf(x, .2, 0.1, 0.15, 5)
y2 <- vasicek_pdf(x, .2, 0.12, 0.15, 5)
y3 <- vasicek_pdf(x, .2, 0.14, 0.15, 5)
y4 <- vasicek_pdf(x, .2, 0.16, 0.15, 5)

matplot(x, cbind(y1,y2,y3,y4), type = "l", ylab ="",xlab = "", col = 1:5, lty = 1)
legend("topleft", c("beta = 0.1", "beta = 0.12", "beta = 0.14", "beta = 0.16"), fill = 1:5, cex = 0.7)

y1 <- vasicek_pdf(x, .1, 0.1, 0.15, 5)
y2 <- vasicek_pdf(x, .2, 0.1, 0.15, 5)
y3 <- vasicek_pdf(x, .3, 0.1, 0.15, 5)
y4 <- vasicek_pdf(x, .4, 0.1, 0.15, 5)

matplot(x, cbind(y1,y2,y3,y4), type = "l", ylab ="",xlab = "", col = 1:5, lty = 1)
legend("topleft", c("alpha = 0.1", "alpha = 0.2", "alpha = 0.3", "alpha = 0.4"), fill = 1:5, cex = 0.7)

y1 <- vasicek_pdf(x, .1, 0.1, 0.10, 5)
y2 <- vasicek_pdf(x, .1, 0.1, 0.12, 5)
y3 <- vasicek_pdf(x, .1, 0.1, 0.14, 5)
y4 <- vasicek_pdf(x, .1, 0.1, 0.15, 5)

matplot(x, cbind(y1,y2,y3,y4), type = "l", ylab ="",xlab = "", col = 1:5, lty = 1)
legend("topleft", c("sigma = 0.1", "sigma = 0.12", "sigma = 0.14", "sigma = 0.15"), fill = 1:5, cex = 0.7)
















