turnover_data <- read.table("turnover_data.csv", header = T, sep = ";") 
format(turnover_data[1:5, 1:6],digits = 3)
plot(turnover_data$AA[1:26], type = “l”, main = “AA”, xlab = “time”, ylab=”turnover”)
AA_average <- matrix(turnover_data$AA, 26, 546/26)
plot(rowMeans(AA_average), type = "l", main = “AA” , xlab = "time", ylab = "turnover")
n <- 520
m <- ncol(turnover_data)
sample <- as.matrix(turnover_data[1:n, ])
S <- sample %*% t(sample)
D <- eigen(S)$values
V <- eigen(S)$vectors
plot(D, main = "Eigenvalues", xlab = "", ylab = "")
Eig <- V[, 1]
F <- sqrt(n) * Eig
Lambda <- F %*% sample / n
K <- F %*% Lambda
IC <- sample - K
K26 <- matrix(0, 26, m)
for (i in 1:m) {
    tmp <- matrix(K[,i], 26, n/26)
    K26[,i] <- rowMeans(tmp)
                 }
library(forecast)
models <- lapply(1:m, function(i)
    Arima(IC[, i], order = c(1, 0, 0), method = "CSS"))
coefs <- sapply(models, function(x) x$coef)
round(coefs, 4)
library("tseries")
arma_mod <- arma(IC[, 1], order = c(1, 0))
ARf <- sapply(1:m, function(i) forecast(models[[i]], h = 26)$mean)
AR_result <- K26+ARf
library(tsDyn)
setar_mod <- apply(IC,2,setar, 1);
setar_coefs <-  sapply(setar_mod, FUN = coefficients)
round(setar_coefs, 4)
SETARf <- matrix(0, 27, m)
SETARf[1,] <- sample[520,]
for (i in 2:27){
SETARf[i,] <- 
(setar_coefs[1,]+SETARf[i-1,]*setar_coefs[2,])*
(SETARf[i-1,] <= setar_coefs[5,]) + 
(setar_coefs[3,]+SETARf[i-1,]*setar_coefs[4,])*
(SETARf[i-1,] >  setar_coefs[5,])
}
SETAR_result = K26 + SETARf[2:27,]
par(mfrow = c(2, 5))
for (i in 1:10) {matplot(cbind(AR_result[, i], turnover_data[521:546, i]), type = "l", main = colnames(turnover_data)[i], xlab = "", ylab = "", col = c("red", "black"))}

