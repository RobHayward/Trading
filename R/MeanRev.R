require(Quandl)
# it is best not to use dplyr as it interferes with vrtest
# require(dplyr)
da <- Quandl("BNP/USDCAD",authcode="mUCjthkJFQDsYVrFh4Gh", start_date = "2007-07-01", end_date = "2012-03-31")
names(da) <- c("Date", "CAD")
da$lCAD <- log(da$CAD)
nob <- length(da$CAD)
r <- c(rep(0, nob))
r[1:nob - 1] <- log(da$CAD[1:nob - 1])-log(da$CAD[2:nob])
da$r <- r
# the following is not used because the dplyr package interfers with vrtest
head(da)
plot(da$Date, da$CAD, type = 'l', main = "USD/CAD", ylab = "CAD", xlab = "Date")
library(urca)
library(xtable)
test <- ur.df(da$lCAD, type = "drift", selectlags = "AIC")
dims <- list(rownames = "Unit root", colnames = c("Phi", "1pc", "5pc", "10pc"))
testtable <- matrix(c(Test = test@teststat[1], Cv = test@cval[1,]), nrow = 1, 
                    dimnames = dims)
testtable

xtable(testtable, caption = "Test statistic and critical values", label = "tab")
library(pracma)
a <- hurstexp(da$lCAD)
library(vrtest)
b <- Lo.Mac(da$r[1:nob - 1], c(2, 5, 10))
b
# Half life test of log change on lagged value.  Half life is -log(2)/beta
eq <- lm(da$r[1:nob - 1] ~ da$lCAD[2:nob])
summary(eq)
HL <- -log(2)/eq$coefficients[2]
HL
# This is the cointegration
da1 <- read.csv("Data/EWA.csv")
da2 <- read.csv("Data/EWC.csv")
da <- cbind(da1, da2, by = da1$Date)
head(da)
da <- da[,-c(8, 15)]
colnames <- c("Date", "EWAO", "EWAH", "EWAL", "EWAC", "EWAV","EWAAC", 
"EWCO", "EWCH", "EWCL", "EWCC", "EWCV","EWAAC")
colnames(da) <- colnames
da$Date <- as.Date(da$Date, format = "%d/%m/%Y")
str(da)
library(urca)
plot(da$Date, da$EWAC, type = 'l', main = "Australian and Canadian ETF", 
     xlab = "Date", ylab = "Price", col = "Dark green")
lines(da$Date, da$EWCC, col = 'blue')
eq1 <- lm(da$EWAC ~ da$EWCC)
eq1$coefficients
dims <- c("Intercept", "EWCC")
print(eq1)

table <- data.frame(eq1$coefficients, dims = dims)
table
plot(eq1$residuals)

