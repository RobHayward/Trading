# Momentum file. 
# https://rbresearch.wordpress.com/2012/08/23/momentum-with-r-part-1/
require(quantstrat)

#Load ETFs from yahoo
currency("USD")
symbols = c("XLY", "XLP", "XLE", "XLF")
stock(symbols, currency="USD",multiplier=1)
getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from='2000-01-01')

#Convert to monthly and drop all columns except Adjusted Close
for(symbol in symbols) {
  x <- get(symbol)
  x <- to.monthly(x,indexAt='lastof',drop.time=TRUE)
  indexFormat(x) <- '%Y-%m-%d'
  colnames(x) <- gsub("x",symbol,colnames(x))
  x <- x[,6] #drops all columns except Adjusted Close which is 6th column
  assign(symbol,x)
}
symbols_close <- do.call(merge, lapply(symbols, get))     
head(symbols_close)
roc <- ROC(symbols_close, n = 3, type = "discrete")
head(roc)
# Apply the rank function across each row of the table
r <- as.xts(t(apply(-roc, 1, rank)))
head(r)
# ------------------------
# Part 2
library(FinancialInstrument)
library(TTR)
library(PerformanceAnalytics)

RankRB <- function(x){
  # Computes the rank of an xts object of ranking factors
  # ranking factors are the factors that are ranked (i.e. asset returns)
  #
  # args:
  #   x = xts object of ranking factors
  #
  # Returns:
  #   Returns an xts object with ranks
  #   (e.g. for ranking asset returns, the asset with the greatest return
  #    receives a  rank of 1)
  
  r <- as.xts(t(apply(-x, 1, rank, na.last = "keep")))
  return(r)
}

MonthlyAd <- function(x){
  # Converts daily data to monthly and returns only the monthly close 
  # Note: only used with Yahoo Finance data so far
  # Thanks to Joshua Ulrich for the Monthly Ad function
  # 
  # args:
  #   x = daily price data from Yahoo Finance
  #
  # Returns:
  #   xts object with the monthly adjusted close prices
  
  sym <- sub("\\..*$", "", names(x)[1])
  Ad(to.monthly(x, indexAt = 'lastof', drop.time = TRUE, name = sym))
}

CAGR <- function(x, m){
  # Function to compute the CAGR given simple returns
  #
  # args:
  #  x = xts of simple returns
  #  m = periods per year (i.e. monthly = 12, daily = 252)
  #
  # Returns the Compound Annual Growth Rate
  x <- na.omit(x)
  cagr <- apply(x, 2, function(x, m) prod(1 + x)^(1 / (length(x) / m)) - 1, m = m)
  return(cagr)
}

SimpleMomentumTest <- function(xts.ret, xts.rank, n = 1, ret.fill.na = 3){
  # returns a list containing a matrix of individual asset returns
  # and the comnbined returns
  # args:
  #  xts.ret = xts of one period returns
  #  xts.rank = xts of ranks
  #  n = number of top ranked assets to trade
  #  ret.fill.na = number of return periods to fill with NA
  #
  # Returns:
  #  returns an xts object of simple returns
  
  # trade the top n asset(s)
  # if the rank of last period is less than or equal to n,
  # then I would experience the return for this month.
  
  # lag the rank object by one period to avoid look ahead bias
  lag.rank <- lag(xts.rank, k = 1, na.pad = TRUE)
  n2 <- nrow(lag.rank[is.na(lag.rank[,1]) == TRUE])
  z <- max(n2, ret.fill.na)
  
  # for trading the top ranked asset, replace all ranks above n
  # with NA to set up for element wise multiplication to get
  # the realized returns
  lag.rank <- as.matrix(lag.rank)
  lag.rank[lag.rank > n] <- NA
  # set the element to 1 for assets ranked <= to rank
  lag.rank[lag.rank <= n] <- 1
  
  # element wise multiplication of the
  # 1 period return matrix and lagged rank matrix
  mat.ret <- as.matrix(xts.ret) * lag.rank
  
  # average the rows of the mat.ret to get the
  # return for that period
  vec.ret <- rowMeans(mat.ret, na.rm = TRUE)
  vec.ret[1:z] <- NA
  
  # convert to an xts object
  vec.ret <- xts(x = vec.ret, order.by = index(xts.ret))
  f <- list(mat = mat.ret, ret = vec.ret, rank = lag.rank)
  return(f)
}

currency("USD")
symbols <- c("XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLK", "XLB", "XLU", "EFA")#, "TLT", "IEF", "SHY")
stock(symbols, currency = "USD", multiplier = 1)

# create new environment to store symbols
symEnv <- new.env()

# getSymbols and assign the symbols to the symEnv environment
getSymbols(symbols, from = '2002-09-01', to = '2012-10-20', env = symEnv)

# xts object of the monthly adjusted close prices
symbols.close <- do.call(merge, eapply(symEnv, MonthlyAd))

# monthly returns
monthly.returns <- ROC(x = symbols.close, n = 1, type = "discrete", na.pad = TRUE)

#############################################################################
# rate of change and rank based on a single period for 3, 6, 9, and 12 months
#############################################################################

roc.three <- ROC(x = symbols.close , n = 3, type = "discrete")
rank.three <- RankRB(roc.three)

roc.six <- ROC(x = symbols.close , n = 6, type = "discrete")
rank.six <- RankRB(roc.six)

roc.nine <- ROC(x = symbols.close , n = 9, type = "discrete")
rank.nine <- RankRB(roc.nine)

roc.twelve <- ROC(x = symbols.close , n = 12, type = "discrete")
rank.twelve <- RankRB(roc.twelve)

num.assets <- 4

# simple momentum test based on 3 month ROC to rank
case1 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.three,
                            n = num.assets, ret.fill.na = 15)

# simple momentum test based on 6 month ROC to rank
case2 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.six,
                            n = num.assets, ret.fill.na = 15)

# simple momentum test based on 9 month ROC to rank
case3 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.nine,
                            n = num.assets, ret.fill.na = 15)

# simple momentum test based on 12 month ROC to rank
case4 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.twelve,
                            n = num.assets, ret.fill.na = 15)

returns <- cbind(case1$ret, case2$ret, case3$ret, case4$ret)
colnames(returns) <- c("3-Month", "6-Month", "9-Month", "12-Month")

charts.PerformanceSummary(R = returns, Rf = 0, geometric = TRUE, 
                          main = "Momentum Cumulative Return: Top 4 Assets")

table.Stats(returns)

CAGR(returns, m = 12)

print("End")
#--------------
# rank_test2.R

# script to run a simple backtest comparing different ranking methods
# for a momentum based trading system

# remove objects from workspace
rm(list = ls())

# load required packages
library(FinancialInstrument)
library(TTR)
library(PerformanceAnalytics)

MonthlyAd <- function(x){
  # Converts daily data to monthly and returns only the monthly close 
  # Note: only used with Yahoo Finance data so far
  # Thanks to Joshua Ulrich for the Monthly Ad function
  # 
  # args:
  #   x = daily price data from Yahoo Finance
  #
  # Returns:
  #   xts object with the monthly adjusted close prices
  
  sym <- sub("\\..*$", "", names(x)[1])
  Ad(to.monthly(x, indexAt = 'lastof', drop.time = TRUE, name = sym))
}

CAGR <- function(x, m){
  # Function to compute the CAGR given simple returns
  #
  # args:
  #  x = xts of simple returns
  #  m = periods per year (i.e. monthly = 12, daily = 252)
  #
  # Returns the Compound Annual Growth Rate
  x <- na.omit(x)
  cagr <- apply(x, 2, function(x, m) prod(1 + x)^(1 / (length(x) / m)) - 1, m = m)
  return(cagr)
}

RankRB <- function(x){
  # Computes the rank of an xts object of ranking factors
  # ranking factors are the factors that are ranked (i.e. asset returns)
  #
  # args:
  #   x = xts object of ranking factors
  #
  # Returns:
  #   Returns an xts object with ranks
  #   (e.g. for ranking asset returns, the asset with the greatest return
  #    receives a  rank of 1)
  
  r <- as.xts(t(apply(-x, 1, rank, na.last = "keep")))
  return(r)
}

SimpleMomentumTest <- function(xts.ret, xts.rank, n = 1, ret.fill.na = 3){
  # returns a list containing a matrix of individual asset returns
  # and the comnbined returns
  # args:
  #  xts.ret = xts of one period returns
  #  xts.rank = xts of ranks
  #  n = number of top ranked assets to trade
  #  ret.fill.na = number of return periods to fill with NA
  #
  # Returns:
  #  returns an xts object of simple returns
  
  # trade the top n asset(s)
  # if the rank of last period is less than or equal to n,
  # then I would experience the return for this month.
  
  # lag the rank object by one period to avoid look ahead bias
  lag.rank <- lag(xts.rank, k = 1, na.pad = TRUE)
  n2 <- nrow(lag.rank[is.na(lag.rank[,1]) == TRUE])
  z <- max(n2, ret.fill.na)
  
  # for trading the top ranked asset, replace all ranks above n
  # with NA to set up for element wise multiplication to get
  # the realized returns
  lag.rank <- as.matrix(lag.rank)
  lag.rank[lag.rank > n] <- NA
  # set the element to 1 for assets ranked <= to rank
  lag.rank[lag.rank <= n] <- 1
  
  # element wise multiplication of the
  # 1 period return matrix and lagged rank matrix
  mat.ret <- as.matrix(xts.ret) * lag.rank
  
  # average the rows of the mat.ret to get the
  # return for that period
  vec.ret <- rowMeans(mat.ret, na.rm = TRUE)
  vec.ret[1:z] <- NA
  
  # convert to an xts object
  vec.ret <- xts(x = vec.ret, order.by = index(xts.ret))
  f <- list(mat = mat.ret, ret = vec.ret, rank = lag.rank)
  return(f)
}

WeightAve3ROC <- function(x, n = c(1,3,6), weights = c(1/3, 1/3, 1/3)){
  # Computes the weighted average rate of change based on a vector of periods
  # and a vector of weights
  #
  # args:
  #   x = xts object of simple returns
  #   n = vector of periods to use n = (period1, period2, period3)
  #   weights = a vector of weights for computing the weighted average
  #
  # Returns:
  #   xts object of weighted average asset rate of change
  
  if((sum(weights) != 1) || (length(n) != 3) || (length(weights) != 3)){
    stop("The sum of the weights must equal 1 and the length of n and weights must be 3")
  } else{
    roc1 <- ROC(x, n = n[1], type = "discrete")
    roc2 <- ROC(x, n = n[2], type = "discrete")
    roc3 <- ROC(x, n = n[3], type = "discrete")
    wave <- (roc1 * weights[1] + roc2 * weights[2] + roc3 * weights[3]) / sum(weights)
    return(wave)
  }
}

currency("USD")
symbols <- c("XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLK", "XLB", "XLU", "EFA")
stock(symbols, currency = "USD", multiplier = 1)

# create new environment to store symbols
symEnv <- new.env()

# getSymbols and assign the symbols to the symEnv environment
getSymbols(symbols, from = '2002-09-01', to = '2012-10-20', env = symEnv)

# xts object of the monthly adjusted close prices
symbols.close <- do.call(merge, eapply(symEnv, MonthlyAd))

# monthly returns
monthly.returns <- ROC(x = symbols.close, n = 1, type = "discrete", na.pad = TRUE)

#############################################################################
# rate of change and rank based on a single period for 6, 9, and 12 months
#############################################################################

roc.six <- ROC(x = symbols.close , n = 6, type = "discrete")
rank.six <- RankRB(roc.six)

roc.nine <- ROC(x = symbols.close , n = 9, type = "discrete")
rank.nine <- RankRB(roc.nine)

roc.twelve <- ROC(x = symbols.close , n = 12, type = "discrete")
rank.twelve <- RankRB(roc.twelve)

#############################################################################
# rate of change and rank based on averaging 6, 9, and 12 month returns
#############################################################################
roc.ave <- WeightAve3ROC(x = symbols.close, n = c(6, 9, 12), 
                         weights = c(1/3, 1/3, 1/3))
rank.ave <- RankRB(roc.ave)

roc.weight.ave <- WeightAve3ROC(x = symbols.close, n = c(6, 9, 12), 
                                weights = c(1/6, 2/3, 1/6))
rank.weight.ave <- RankRB(roc.weight.ave)

#############################################################################
# run the backtest
#############################################################################

num.assets <- 4

# simple momentum test based on 6 month ROC to rank
case1 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.six,
                            n = num.assets, ret.fill.na = 15)

# simple momentum test based on 9 month ROC to rank
case2 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.nine,
                            n = num.assets, ret.fill.na = 15)

# simple momentum test based on 12 month ROC to rank
case3 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.twelve,
                            n = num.assets, ret.fill.na = 15)

# simple momentum test based on average of 6, 9, and 12 month ROC to rank
case4 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.ave,
                            n = num.assets, ret.fill.na = 15)

# simple momentum test based on weighted average of 6, 9, and 12 month ROC to rank
case5 <- SimpleMomentumTest(xts.ret = monthly.returns, xts.rank = rank.weight.ave,
                            n = num.assets, ret.fill.na = 15)

returns <- cbind(case1$ret, case2$ret, case3$ret, case4$ret, case5$ret)
colnames(returns) <- c("6-Month", "9-Month", "12-Month", "Ave", "Weighted Ave")

charts.PerformanceSummary(R = returns, Rf = 0, geometric = TRUE, 
                          main = "Momentum Cumulative Return: Top 4 Assets")

table.Stats(returns)

cagr <- CAGR(returns, m = 12)
max.dd <- maxDrawdown(returns)
print(cagr)
print(max.dd)

print("End")
#---------------Part 4
# run the backtest
bt <- qstratRank(symbols=symbols, init.equity=100000, top.N=2,
                 max.size=1000, max.levels=1)

# chart of returns
charts.PerformanceSummary(bt$returns[,"total"], geometric=FALSE, 
                          wealth.index=TRUE, main="Total Performance")