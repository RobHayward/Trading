require(Quandl)
require(dplyr)
da <- Quandl("BNP/USDCAD",authcode="mUCjthkJFQDsYVrFh4Gh", start_date = "2007-07-01", end_date = "2012-03-31")
names(da) <- c("Date", "CAD")
da$lCAD <- log(da$CAD)
da$llrCAD <- lag(da$lCAD, k = -1)
nob <- length(da$CAD)
r <- log(da$CAD[2:nob])-log(da$CAD[1:(nob-1)])
# the following is not used because the dplyr package interfers with vrtest
da <- da %>% 
  mutate(rCAD =  lCAD - lag(lCAD))
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
b <- Lo.Mac(da$rCAD[2:length(da$CAD)], c(2, 5, 10))
b
Lo.Mac(r, c(2, 5, 10))
head(da)
str(da)
plot(da$Date, da$rCAD, type = 'l')
detach(package:dplyr)
2:nobs
