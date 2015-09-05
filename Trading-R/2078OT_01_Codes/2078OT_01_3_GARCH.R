
###############################################################################
# GARCH modelling - stylized facts and intro to rugarch package
# 
# Author: Tamas Vadasz
###############################################################################

install.packages("quantmod");library("quantmod")

###################################
# *** EMPIRICAL FACTS REVISED *** #
###################################

#load the data from yahoofinance

getSymbols("SNP", from="2004-01-01", to="2014-10-15",)
chartSeries(Cl(SNP))

ret <- dailyReturn(Cl(SNP), type='log')

#return plots
plot.zoo(cbind(ret,ret^2, abs(ret)), ylab=c("return","squared.ret","abs.ret"), main='Return series')

#ACF
par(mfrow=c(2,2))
acf(ret, main="Return ACF");
pacf(ret, main="Return PACF");
acf(ret^2, main="Squared return ACF");
pacf(ret^2, main="Squared return PACF")
par(mfrow=c(1,1))


#histogram and return plot
plot(ret, main='Returns')

m=mean(ret);s=sd(ret);

par(mfrow=c(2,1))
hist(ret, nclass=100, freq=FALSE, main='Return histogram')
lines(density(ret),col='red')
par(mfrow=c(1,1))


# Histogram and EDF of returns
m=mean(ret);s=sd(ret);
par(mfrow=c(1,2))
hist(ret, nclass=40, freq=FALSE, main='Return histogram');curve(dnorm(x, mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")
plot(density(ret), main='Return empirical distribution');curve(dnorm(x, mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")
par(mfrow=c(1,1))

#kurtosis
install.packages('moments');library('moments')
kurtosis(ret)

# tail zoom
plot(density(ret), main='Return EDF - upper tail', xlim = c(0.1, 0.2), ylim=c(0,2));
curve(dnorm(x, mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")


par(mfrow=c(1,2))
# density plots on log-scale
plot(density(ret), xlim=c(-5*s,5*s),log='y', main='Density on log-scale')
curve(dnorm(x, mean=m,sd=s), from=-5*s, to=5*s, log="y", add=TRUE, col="red")

# QQ-plot
qqnorm(ret);qqline(ret);
par(mfrow=c(1,1))

###############################
# *** GARCH MODEL FITTING *** #
###############################

install.packages("rugarch"); library("rugarch")

#Load Apple data and calculate log-returns
getSymbols("AAPL", from="2006-01-01", to="2014-03-31")
ret.aapl <- dailyReturn(Cl(AAPL), type='log')
chartSeries(ret.aapl)

# specify GARCH(1,1) model with only constant in mean equation
garch11.spec = ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(1,1)), 
		mean.model = list(armaOrder=c(0,0)))

#Fit the model to the data
aapl.garch11.fit = ugarchfit(spec=garch11.spec, data=ret.aapl)

#obtain coefficients
coef(aapl.garch11.fit)

#obtain the results
aapl.garch11.fit

coef(aapl.garch11.fit)			#estimated coefficients
vcov(aapl.garch11.fit)			#covariance matrix of parameter estimates
infocriteria(aapl.garch11.fit)	#common information criteria list
newsimpact(aapl.garch11.fit)	#calculate news impact curve
signbias(aapl.garch11.fit)		#Engle - Ng sign bias test
fitted(aapl.garch11.fit)		#obtarchin the fitted data series
residuals(aapl.garch11.fit)		#obtain the residuals
uncvariance(aapl.garch11.fit)	#unconditional (long-run) variance
uncmean(aapl.garch11.fit)		#unconditional (long-run) mean

#SGARCH news impact curve
ni.garch11 <- newsimpact(aapl.garch11.fit) 
plot(ni.garch11$zx, ni.garch11$zy, type="l", lwd=2, col="blue", main="GARCH(1,1) - News Impact", ylab=ni.garch11$yexpr, xlab=ni.garch11$xexpr)


#EGARCH

# specify EGARCH(1,1) model with only constant in mean equation
egarch11.spec = ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1)), 
		mean.model = list(armaOrder=c(0,0)))
aapl.egarch11.fit = ugarchfit(spec=egarch11.spec, data=ret.aapl)
coef(aapl.egarch11.fit)

ni.egarch11 <- newsimpact(aapl.egarch11.fit)
par(font.axis=2, font.lab=2)
plot(ni.egarch11$zx, ni.egarch11$zy, type="l", lwd=2, col="blue", main="EGARCH(1,1) - News Impact", 
		ylab=ni.egarch11$yexpr, xlab=ni.egarch11$xexpr)


#TGARCH
# specify TGARCH(1,1) model with only constant in mean equation
tgarch11.spec = ugarchspec(variance.model = list(model="fGARCH", submodel="TGARCH", garchOrder=c(1,1)), 
		mean.model = list(armaOrder=c(0,0)))
aapl.tgarch11.fit = ugarchfit(spec=tgarch11.spec, data=ret.aapl)
coef(aapl.egarch11.fit)

ni.tgarch11 <- newsimpact(aapl.tgarch11.fit)
plot(ni.tgarch11$zx, ni.tgarch11$zy, type="l", lwd=2, col="blue", main="TGARCH(1,1) - News Impact", 
		 ylab=ni.tgarch11$yexpr, xlab=ni.tgarch11$xexpr)


 
 #SIMULATION
# specify GARCH(1,1) model
garch11sim.spec = ugarchspec(variance.model = list(garchOrder=c(1,1)), 
		mean.model = list(armaOrder=c(0,0)),
		fixed.pars=list(mu = 0, omega=0.1, alpha1=0.1,
				beta1 = 0.7))
garch11.sim = ugarchpath(garch11.spec, n.sim=1000)


#FORECASTING
aapl.garch11.fit = ugarchfit(spec=garch11.spec, data=ret.aapl, out.sample=20)
aapl.garch11.fcst = ugarchforecast(aapl.garch11.fit, n.ahead=10, n.roll=10)
plot(aapl.garch11.fcst, which=1)
plot(aapl.garch11.fcst, which='all')
