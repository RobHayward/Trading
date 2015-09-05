#2 Factor Models

#Data selection
stocks <- read.table("stocks.csv", header = TRUE, sep = ";")
str(stocks)
stocks[1:5, c(1, 3:4, ncol(stocks))]

library(matrixStats)
d <- read.table("data.csv", header = TRUE, sep = ";", colClasses = c("Date", rep("numeric",4014)))
d[1:7, c(1:5, (ncol(d) - 6):ncol(d))]
d <- d[, colSums(is.na(d)) == 0]
d <- d[, c(T, colMins(d[, 2:ncol(d)]) > 0)]

 
#Estimation of APT with principal component analysis

p <- d[, 3:ncol(d)]
r <- log(p[2:nrow(p), ] / p[1:(nrow(p) - 1), ])
r <- r[, runif(nrow(r)) < 0.1]
pca <- princomp(r)
plot(pca$sdev)
factanal(r, 5)


#Estimation of the Fama-French model
library(Quandl)
LIBOR <- Quandl('FED/RILSPDEPM01_N_B',start_date = '2010-06-01', end_date = '2014-06-01')

d2 <- d[, 2:ncol(d)]   
d2 <- log(tail(d2, -1)/head(d2, -1))  
d <- cbind(d[2:nrow(d), 1], d2)   
d <- merge(LIBOR, d, by = 1) 
print(d[1:5, 1:5])

d$LIBOR <- d[,2] / 36000
d[1:5, c(1,(ncol(d) - 3):ncol(d))]
stocks = stocks[stocks$Symbol %in% colnames(d),]
stocks$BookToMarketRatio <- stocks$BookValuePerShare / stocks$LastSale
str(stocks)
avg_size <- mean(stocks$MarketCap)
BIG   <- as.character(stocks$Symbol[stocks$MarketCap > avg_size])
SMALL <- as.character(stocks[stocks$MarketCap < avg_size,1])
d$SMB <- rowMeans(d[,colnames(d) %in% SMALL]) - rowMeans(d[,colnames(d) %in% BIG])
avg_btm <- mean(stocks$BookToMarketRatio)
HIGH <- as.character(stocks[stocks$BookToMarketRatio > avg_btm, 1])
LOW <- as.character(stocks[stocks$BookToMarketRatio < avg_btm, 1])
d$HML <- rowMeans(d[, colnames(d) %in% HIGH]) - rowMeans(d[, colnames(d) %in% LOW])

d$Market <- d$SP500 - d$LIBOR
d$C   <- d$C - d$LIBOR
model <- glm( formula = "C ~ Market + SMB + HML" , data = d)
estimation <- model$coefficients[1]+
  model$coefficients[2] * d$Market +
  model$coefficients[3]*d$SMB +
  model$coefficients[4]*d$HML
plot(estimation, d$C, xlab = "estimated risk-premium",ylab = "observed riks premium",main = "Fama-French model for Citigroup")
lines(c(-1, 1), c(-1, 1), col = "red")
outlier <- which.max(d$C)
d$C[outlier] <- 0 
model_new <- glm( formula = "C ~ Market + SMB + HML" , data = d)
estimation_new <- model_new$coefficients[1]+
  model_new$coefficients[2] * d$Market +
  model_new$coefficients[3]*d$SMB +
  model_new$coefficients[4]*d$HML
dev.new()
plot(estimation_new, d$C, xlab = "estimated risk-premium",ylab = "observed riks premium",main = "Fama-French model for Citigroup")
lines(c(-1, 1), c(-1, 1), col = "red")
summary(lm( formula = "C ~ Market + SMB + HML" , data = d))

d$EXEL <- d$EXEL - d$LIBOR
model2 <- glm( formula = "EXEL~Market+SMB+HML" , data = d)
summary(model2)
estimation2 <- model2$coefficients[1] +
  model2$coefficients[2] * d$Market +
  model2$coefficients[3] * d$SMB + model2$coefficients[4] * d$HML
dev.new()
plot(estimation2, d$EXEL, xlab = "estimated risk-premium",ylab = "observed riks premium",main = "Fama-French model for EXEL")
lines(c(-1, 1), c(-1, 1), col = "red")
