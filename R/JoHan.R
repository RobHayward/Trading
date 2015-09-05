# This will carry out the Johansen technique to test for integration
da1 <- read.csv("Data/EWA.csv")
da2 <- read.csv("Data/EWC.csv")
da3 <- read.csv("Data/IGE.csv")
da <- cbind(da1, da2, da3, by = da1$Date)
head(da)
da <- da[,-c(8, 15, 22)]
colnames <- c("Date", "EWAO", "EWAH", "EWAL", "EWAC", "EWAV","EWAAC", 
"EWCO", "EWCH", "EWCL", "EWCC", "EWCV","EWCAC","IGEO", "IGEH", 
"IGEL", "IGEC", "IGEV","IGEAC" )
colnames(da) <- colnames
da$Date <- as.Date(da$Date, format = "%d/%m/%Y")
str(da)
library(urca)
plot(da$Date, da$EWAC, type = 'l', main = "Australia, Canadian and Commodity ETF", 
     xlab = "Date", ylab = "Price", col = "Dark green", ylim = c(10, 50))
lines(da$Date, da$EWCC, col = 'blue')
lines(da$Date, da$IGEAC)
# added a trend because of the trend in residuals that was evident before. 
t <- seq(1, length(da$IGEAC), 1)
eq1 <- lm(da$EWAC ~ da$EWCC + t)
summary(eq1)
print(eq1)
plot(eq1$residuals, type = 'l')
nob2 <- length(da$EWAC)
EWACr <- c(rep(0, nob2))
EWACr[1:nob2-1] <- log(da$EWAC[1:nob2 - 1])-log(da$EWAC[2:nob2])
da$EWACr <- EWACr
EWCCr <- c(rep(0, nob2))
EWCCr[1:nob2-1] <- log(da$EWCC[1:nob2 - 1])-log(da$EWCC[2:nob2])
da$EWCCr <- EWCCr
IGECr <- c(rep(0, nob2))
IGECr[1:nob2-1] <- log(da$IGEC[1:nob2 - 1])-log(da$IGEC[2:nob2])
da$IGECr <- IGECr
  # eq2 <- lm(da$EWACr[seq(1,nob2 - 2, 1)] ~ da$EWCCr[seq(2,nob2 - 1, 1)] + 
  #          da$EWACr[seq(2,nob2 - 1, 1)] + eq1$residuals[seq(2,nob2 - 1, 1)])
head(da)
dat <- da[,c(1, 7, 13, 19, 20, 21, 22)]
head(dat)
eq2 <- ca.jo(dat[c(2, 3)], ecdet = "none", K =2)
summary(eq2)
#
