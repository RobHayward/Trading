###############################################################################
# Simulated example for Cointegration, based on Hamilton (1994)
# 
# Author: Tamas Vadasz
###############################################################################

#Modify plot parameters
par(font.axis=2, font.lab=2)


#generate the two time series of length 1000

set.seed(20140623)			#fix the random seed
N <- 1000					#define length of simulation
x <- cumsum(rnorm(N))		#simulate a normal random walk
gamma <- 0.7				#set an initial parameter value
y <- gamma * x + rnorm(N)	#simulate the cointegrating series
plot(x, col="blue", type='l', lwd=2, ylab='simulated values')	 		#plot the two series
lines(y,col="red", lwd=2)												#***2078OT__01_01.png***
legend(50,30,c("x","y"),lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","red")) 

#------------------------------

#statistical tests

install.packages('urca')
library('urca')

#ADF test for the simulated individual time series
	#NULL: unit root exists
	#reject NULL if test-statistic < critical value

summary(ur.df(x,type="none"))	
summary(ur.df(y,type="none"))	

#------------------------------

z <- y - gamma * x				#take a linear combination of the series
plot(z,type='l')				#***2078OT__01_02.png***
summary(ur.df(z,type="none"))	

#------------------------------


#Estimate the cointegrating relationship
coin <- lm(y ~ x -1)		#regression without constant
coin$resid					#obtain the residuals
summary(ur.df(coin$resid))	#ADF test of residuals


