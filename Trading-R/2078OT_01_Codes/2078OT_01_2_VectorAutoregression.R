###############################################################################
# An example VAR implementation using vars package
# 
# Author: Tamas Vadasz
###############################################################################

rm(list = ls()) #clear the whole workspace
install.packages('xts');library(xts)

#install and load required packages

install.packages('vars');library('vars')
install.packages('quantmod');library('quantmod')

#download the data
getSymbols('SNP', from='2004-01-02', to='2014-03-31')		#get S&P 500 index data
getSymbols('MSFT', from='2004-01-02', to='2014-03-31')		#get Microsoft data
getSymbols('DTB3', src='FRED')								#3-month T-Bill interest rates

#chart downloaded data
chartSeries(MSFT) 										#***2078OT__01_03.png***
chartSeries(MSFT, theme=chartTheme('white'))   									#***2078OT__01_03.png***


#obtain components of downloaded object
Cl(MSFT) 	#closing prices
Op(MSFT)	#open prices
Hi(MSFT)	#daily highest price
Lo(MSFT)	#daily lowest price
ClCl(MSFT)	#close-to-close daily return
Ad(MSFT)	#daily adjusted closing price

chartSeries(ClCl(MSFT))	#a plotting example with shortcuts #***2078OT__01_04.png***

#indexing time series data
DTB3.sub <- DTB3['2004-01-02/2014-03-31']

#Calculate returns
SNP.ret  <- diff(log(Ad(SNP)))
MSFT.ret <- diff(log(Ad(MSFT)))

#replace NA values
DTB3.sub[is.na(DTB3.sub)] <- 0
DTB3.sub <- na.omit(DTB3.sub)

#merge the three databases to get the same length
dataDaily <- na.omit(merge(SNP.ret,MSFT.ret,DTB3.sub), join='inner')

#obtain monthly data
SNP.M  <- to.monthly(SNP.ret)$SNP.ret.Close
MSFT.M <- to.monthly(MSFT.ret)$MSFT.ret.Close
DTB3.M <- to.monthly(DTB3.sub)$DTB3.sub.Close


#Fit a simple VAR model to the data
var1 <- VAR(dataDaily, lag.max=4, ic="AIC")
VARselect(dataDaily,lag.max=4)

summary(var1)
var1


#obtain the results
summary(var1)
var1
var1$varresult
var1$type
var1$p
var1$K
var1$obs
var1$totobs
var1$restrictions
var1$call

plot(var1) 		#Diagram of fit and residuals for each variables
coef(var1)		#concise summary of the estimated variables
residuals(var1)	#list of residuals (of the corresponding ~lm)
fitted(var1)	#list of fitted values
Phi(var1)		#coefficient matrices of VMA representation
plot(var1, plot.type='multiple')

#*****************************
#impulse responses

var.irf <- irf(var1, ci=0.9)
plot(var.irf)

#*****************************
#forecast based on a VAR model
var.pred <- predict(var1, n.ahead=10, ci=0.95)
plot(var.pred)

#*****************************
# SVAR

#if k=3, we need 3*4/2=6 additional retriction
amat <- diag(3)
amat[2, 1] <- NA
amat[2, 3] <- NA
amat[3, 1] <- NA

svar1 <- SVAR(var1, estmethod='direct', Amat = amat)
irf.svar1 <- irf(svar1)
plot(irf.svar1)

#*****************************
# Cointegrated VAR and VECM
#*****************************


#Load data
library('quantmod')
getSymbols('DTB3', src='FRED')			#3-months T-Bill secondary market rate
getSymbols('DTB6', src='FRED')			#6-months T-Bill secondary market rate
DTB3.sub = DTB3['1984-01-02/2014-03-31']
DTB6.sub = DTB6['1984-01-02/2014-03-31']



#plot data
plot(DTB3.sub)
lines(DTB6.sub, col='red')

#Unit root tests. Null: there is unit root.
summary(ur.df(na.omit(DTB3.sub),type="none"))	
summary(ur.df(na.omit(DTB6.sub),type="none"))	

#********************************************************
# Built-in methods: Phillips–Ouliaris Cointegration Test
#********************************************************
x1=as.numeric(na.omit(DTB3.sub))
x2=as.numeric(na.omit(DTB6.sub))
y = cbind(x1,x2)


#Null hypothesis: series are NOT cointegrated.
install.packages('tseries');library('tseries');
po.coint <- po.test(y, demean = TRUE, lshort = TRUE)


#*****************************
# Built-in methods: ca.jo
#*****************************
install.packages('urca');
library('urca');

#estimation using ca.jo() - Johansen approach
y = cbind(na.omit(DTB3.sub),na.omit(DTB6.sub))
yJoTest = ca.jo(y, type = c("trace"), ecdet = c("none"), K = 2)
yJoRegr = cajorls(dyTest, r=1)


#***********************************************
# Custom method: Engle-Granger step-by-step
#***********************************************

#Straight linear regression without constant ***on levels***
cregr <- lm(x1 ~ x2)

#OBtain residuals
r = cregr$residuals


#Obtain differenced series (numeric format)
y = cbind(x1,x2)
dy = cbind(diff(x1),diff(x2))

#Generate lagged variables, and adjust time series lengths respectively
dylag1 = lag(dy)
dylag2 = lag(lag(dy))
dy = dy[4:end(dy),]
dylag1 = dylag1[3:end(dylag1),]
dylag2 = dylag2[2:end(dylag2),]
r = r[4:(end(r)-1)]

#VECM model, two equations: regression on differenced lags and the error term

lmVecmLag1Var1 <- lm(dy[,1] ~  dylag1[,1] + dylag1[,2] + r )
lmVecmLag1Var2 <- lm(dy[,2] ~  dylag1[,1] + dylag1[,2] + r )


