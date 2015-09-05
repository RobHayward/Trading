########################################################################
# ALMDemo script
#
# script file of the Asset and Liability Management chapter
#
# Authors: Daniel Havran and Istvan Margitai
# 2014 October
#
########################################################################




#initializing script
########################################################################
#Import datafile
portfolio <- read.csv("portfolio.csv", 
                      header = TRUE, 
                      sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
market <- read.csv("market.csv", 
                   header = TRUE, 
                   sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

#format date values
portfolio$issue    <-as.Date(portfolio$issue,    format = "%m/%d/%Y")
portfolio$maturity <-as.Date(portfolio$maturity, format = "%m/%d/%Y")
market$date        <-as.Date(market$date,        format = "%m/%d/%Y")
#########################################################################


#check data
#########################################################################
head(portfolio)
levels(portfolio$account_name)
head(market)
#########################################################################


#loading source file
#########################################################################
source.filename<-"2078OT_01_11_bankALM.R"
source(source.filename)
#########################################################################


#check basic functions
#########################################################################
NOW<-as.Date("09/30/2014",    format = "%m/%d/%Y")

cf(0.10,3,100,type="BULLET")
test.date<-seq(from= as.Date("09/30/2015", format= "%m/%d/%Y"),
               to= as.Date("09/30/2035", format= "%m/%d/%Y"), by="1 year")               
get.yieldcurve.spot(market, test.date, type="EUR01", now=NOW, showplot=TRUE)

test.reprice.date<-test.date[seq(from=1, to=20, by=2)]
test.forward<-get.yieldcurve.forward(market, test.reprice.date, 
                                     type="EUR01", now=NOW)
test.floating<-get.floating(market, test.date, test.reprice.date, 
                                       type="EUR01", now=NOW, showplot=TRUE)
#plot(test.forward, type="l", col="green")
#lines(test.floating, type="s", col="red")


rm(test.floating, test.forward, test.date, test.reprice.date)
#########################################################################


#cash-flow generation
#########################################################################

#get cashflows
cashflow.table     <- data.frame()

for (i in 1:NROW(portfolio))
{ cashflow.table <- rbind(cashflow.table, 
                          cf.table(portfolio, market, now=NOW, id=i) ) }

head(cashflow.table)
#write.csv(cashflow.table,file="~/Dropbox/ALM_R/R_code/cashflowtable.csv")
#########################################################################


#get present values
#########################################################################
presentvalue.table <- data.frame()
for (i in 1:NROW(portfolio))
{presentvalue.table  <- rbind(presentvalue.table, 
                              pv.table(cashflow.table[cashflow.table$id==portfolio$id[i],], market, now=NOW)) }

head(presentvalue.table)
#write.csv(presentvalue.table,file="~/Dropbox/ALM_R/R_code/presentvaluetable.csv")
#########################################################################


#some basic transformation
#########################################################################
#cash-flow for a given date
head(aggregate(. ~ date, data=subset(cashflow.table,select=-c(id, account)), FUN=sum))

#Market Value of Equity
pvt1<-aggregate(. ~ account, data=subset(presentvalue.table,select=-c(id,date)), FUN=sum)
head(pvt1)
sum(pvt1[,2])
#########################################################################


#IRR measurement
#########################################################################
#get re-pricing table
(repgap<-repricing.gap.table(portfolio, now=NOW))
#
barplot(repgap, col="red", xlab="Months", ylab="EUR")
title(main="Repricing gap table", 
      sub =paste("Actual date: ",as.character(as.Date(NOW))), cex=0.8)


#net interest income
nii     <- nii.table(cashflow.table, now=NOW)
#

#plot
plot.new()
par.backup<-par()
par(oma = c(1, 1, 1, 6), new=TRUE)
barplot(nii,col=heat.colors(NROW(nii)-1), 
        xlab="Maturity", cex.names=0.8, ylab="EUR", cex.axis=0.8,
        args.legend = list(x = "right"))

title(main="Net interest income table", 
      sub=paste("Actual date: ",as.character(as.Date(NOW))), cex=0.8 )
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0),mar = c(0, 0, 0, 0),  new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = row.names(nii[1:(NROW(nii)-1),]), 
       fill = heat.colors(NROW(nii)-1), bty = "n", cex=1)
par(par.backup)

#########################################################################


#liquidity measurement
#########################################################################
#get liquidity tables
lq      <- lq.table(cashflow.table, now=NOW)
#

#plot
plot.new()
par.backup<-par()
par(oma = c(1, 1, 1, 6), new=TRUE)
lq.bar<-barplot(lq[1:(NROW(lq)-1),], col=heat.colors(NROW(lq)-1),
                xlab="Maturity", cex.names=0.8,
                ylab="EUR", cex.axis=0.8,
                args.legend = list(x = "right")) 
title(main="Liquidity gap table", 
      sub=paste("Actual date: ",as.character(as.Date(NOW))), cex=0.8 )
lines(x=lq.bar,y=lq[NROW(lq),],lwd=4, col="red", lty=5, type="b", pch=0 )
lines(x=lq.bar,y=cumsum(lq[NROW(lq),]),lwd=4, col="black", type="b" )

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0),mar = c(0, 0, 0, 0),  new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right", legend = row.names(lq[1:(NROW(lq)-1),]), 
       fill = heat.colors(NROW(lq)-1), bty = "n", cex=1)
par(par.backup)

#########################################################################


#non-maturing deposits
#########################################################################
#Data import
nmd <- read.csv("ecb_nmd_data.csv", 
                header = TRUE, 
                sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
nmd$date <- as.Date(nmd$date, format = "%m/%d/%Y")

#Plotting deposit coupon and 1 month Euribor
library(car)
plot(nmd$eur1m ~ nmd$date, type="l", xlab="Time", ylab="Interest rate")
lines(nmd$cpn~ nmd$date, type="l", col="red")
title(main="Time series", cex=0.8 )
legend("topright", legend = c("Coupon","EUR 1M"), 
       fill =  c("red","black"), bty = "n", cex=1)

library(urca)
attach(nmd)

#Unit root test (ADF)
cpn.ur <- ur.df(cpn, type="none", lags=2)
dcpn.ur <- ur.df(diff(cpn), type="none", lags=1)
eur1m.ur <- ur.df(eur1m, type="none", lags=2)
deur1m.ur <- ur.df(diff(eur1m), type="none", lags=1)
sumtbl <- matrix(cbind(cpn.ur@teststat, cpn.ur@cval, dcpn.ur@teststat, dcpn.ur@cval, eur1m.ur@teststat, eur1m.ur@cval, deur1m.ur@teststat, deur1m.ur@cval), nrow=4)
colnames(sumtbl) <- c("cpn", "diff(cpn)", "eur1m", "diff(eur1m)")
rownames(sumtbl) <- c("Test stat", "1pct CV", "5pct CV", "10pct CV")

#Stationarty test (KPSS)
cpn.kpss <- ur.kpss(cpn, type="mu")
eur1m.kpss <- ur.kpss(eur1m, type="mu")

sumtbl <- matrix(cbind( cpn.kpss@teststat, cpn.kpss@cval, eur1m.kpss@teststat, eur1m.kpss@cval), nrow=5)
colnames(sumtbl) <- c("cpn", "eur1m")
rownames(sumtbl) <- c("Test stat", "10pct CV", "5pct CV", "2.5pct CV", "1pct CV")

print(cpn.ur@test.name)
print(sumtbl)
print(cpn.kpss@test.name)
print(sumtbl)

#Residual test of cointegrating equation
lr <- lm(cpn ~ eur1m)
res <- resid(lr)
lr$coefficients
res.ur <- ur.df(res, type="none", lags=1)
summary(res.ur)

library(dynlm)
res <- resid(lr)[2:length(cpn)]
dy <- diff(cpn)
dx <- diff(eur1m)
detach(nmd)
ecmdata <- c(dy, dx, res)
ecm <- dynlm(dy ~ L(dx, 1) + L(res, 1), data = ecmdata)
summary(ecm)

#ECB yield curve data import
ecb.yc <- read.csv("ecb_yc_data.csv", 
                   header = TRUE, 
                   sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")
ecb.yc$date <- as.Date(ecb.yc$date, format = "%d/%m/%Y")
plot(ecb.yc$EUR1M ~ ecb.yc$date, type="l", col="red", xlab="Time", ylab="Yield")
lines(ecb.yc$EUR3M ~ ecb.yc$date, type="l", col="blue")
lines(ecb.yc$EUR1Y ~ ecb.yc$date, type="l", col="yellow")
lines(ecb.yc$EUR5Y ~ ecb.yc$date, type="l", col="green")
lines(ecb.yc$EUR10Y ~ ecb.yc$date, type="l", col="black")
legend("bottomleft", legend = c("1M Euribor","3M Euribor", "1Y Gov Benchm", "5Y Gov Benchm", "10Y Gov Benchm"), 
       fill =  c("red","blue", "yellow", "green", "black"), bty = "n", cex=1)

#Solving linear optimization problem with constraints
library(quadprog)
b <- nmd$cpn[21:135]
A <- cbind(ecb.yc$EUR1M, ecb.yc$EUR3M, ecb.yc$EUR1Y, ecb.yc$EUR5Y, ecb.yc$EUR10Y)
m <- c(1, 3, 12, 60, 120)
l <- 60
stat.opt <- solve.QP( t(A) %*% A, t(b) %*% A, 
                      cbind( matrix(1, nr=5, nc=1),
                             matrix(m, nr=5, nc=1),
                             diag(5)),
                      c(1, l, 0,0,0,0,0),
                      meq=2 )
sumtbl <- matrix(round(stat.opt$solution*100, digits = 1), nr=1)
colnames(sumtbl) <- c("1M", "3M", "1Y", "5Y", "10Y")
cat("Portfolio weights in %")
print(sumtbl)

mrg <- nmd$cpn[21:135] - stat.opt$solution[2]*ecb.yc$EUR3M + stat.opt$solution[5]*ecb.yc$EUR10Y
plot(mrg ~ ecb.yc$date, type = "l", col="red", xlab="Time", ylab="%")
title(main="Margin of static replication", cex=0.8 )

#########################################################################
