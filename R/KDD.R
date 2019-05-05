# KDA Asset Allocation
# https://quantstrattrader.wordpress.com/2019/01/24/right-now-its-kda-asset-allocation/
# Compute strategy statistics
stratStats <- function(rets) {
  stats <- rbind(table.AnnualizedReturns(rets), maxDrawdown(rets))
  stats[5,] <- stats[1,]/stats[4,]
  stats[6,] <- stats[1,]/UlcerIndex(rets)
  rownames(stats)[4] <- "Worst Drawdown"
  rownames(stats)[5] <- "Calmar Ratio"
  rownames(stats)[6] <- "Ulcer Performance Index"
  return(stats)
}
# required libraries
require(quantmod)
require(PerformanceAnalytics)
require(tseries)

# symbols
symbols <- c("SPY", "VGK",   "EWJ",  "EEM",  "VNQ",  "RWX",  
             "IEF",  "TLT",  "DBC",  "GLD", "VWO", "BND", "IJS")  


# get data.  thisw needs to be fixed so that data is collected and turned into returns
rets <- list()
for(i in 1:length(symbols)) {
  required_column <- paste0(i, ".Adjusted")
  temp <- getSymbols(symbols[i], from ="1990-12-31", auto.assign = FALSE)
  returns <- Return.calculate(temp[,required_column])
  colnames(returns) <- symbols[i]
  rets[[i]] <- returns
}
# this is mine from 
# https://stackoverflow.com/questions/24377590/getsymbols_downloading-data-for-multiple-symbols-and-calculate-returns

stocks <- lapply(symbols, function(sym){
  dailyReturn(na.omit(getSymbols(sym, from="2005-01-01", auto.assign = FALSE)))
})
rets <- do.call(merge, stocks)
colnames(rets) <- symbols
head(rets)
# algorithm
KDA <- function(rets, offset = 0, leverageFactor = 1.5) {
  
  # get monthly endpoints, allow for offsetting ala AllocateSmartly/Newfound Research
  ep <- endpoints(rets) + offset
  ep[ep < 1] <- 1
  ep[ep > nrow(rets)] <- nrow(rets)
  ep <- unique(ep)
  
  # initialize vector holding zeroes for assets
  emptyVec <- data.frame(t(rep(0, 10)))
  colnames(emptyVec) <- symbols[1:10]
  
  
  allWts <- list()
  # we will use the 13612F filter
  for(i in 1:(length(ep)-12)) {
    
    # 12 assets for returns -- 2 of which are our crash protection assets
    retSubset <- rets[c((ep[i]+1):ep[(i+12)]),]
    epSub <- endpoints(retSubset)
    sixMonths <- retSubset[(epSub[7]+1):epSub[13],]
    threeMonths <- retSubset[(epSub[10]+1):epSub[13],]
    oneMonth <- retSubset[(epSub[12]+1):epSub[13],]
    
    # computer 13612 fast momentum
    moms <- Return.cumulative(oneMonth) * 12 + Return.cumulative(threeMonths) * 4 + 
      Return.cumulative(sixMonths) * 2 + Return.cumulative(retSubset)
    assetMoms <- moms[,1:10] # Adaptive Asset Allocation investable universe
    cpMoms <- moms[,11:12] # VWO and BND from Defensive Asset Allocation
    
    # find qualifying assets
    highRankAssets <- rank(assetMoms) >= 6 # top 5 assets
    posReturnAssets <- assetMoms > 0 # positive momentum assets
    selectedAssets <- highRankAssets & posReturnAssets # intersection of the above
    
    # perform mean-variance/quadratic optimization
    investedAssets <- emptyVec
    if(sum(selectedAssets)==0) {
      investedAssets <- emptyVec
    } else if(sum(selectedAssets)==1) {
      investedAssets <- emptyVec + selectedAssets 
    } else {
      idx <- which(selectedAssets)
      # use 1-3-6-12 fast correlation average to match with momentum filter  
      cors <- (cor(oneMonth[,idx]) * 12 + cor(threeMonths[,idx]) * 4 + 
                 cor(sixMonths[,idx]) * 2 + cor(retSubset[,idx]))/19
      vols <- StdDev(oneMonth[,idx]) # use last month of data for volatility computation from AAA
      covs <- t(vols) %*% vols * cors
      
      # do standard min vol optimization
      minVolRets <- t(matrix(rep(1, sum(selectedAssets))))
      minVolWt <- portfolio.optim(x=minVolRets, covmat = covs)$pw
      names(minVolWt) <- colnames(covs)
      investedAssets <- emptyVec
      investedAssets[,selectedAssets] <- minVolWt
    }
    
    # crash protection -- between aggressive allocation and crash protection allocation
    pctAggressive <- mean(cpMoms > 0)
    investedAssets <- investedAssets * pctAggressive 
    
    pctCp <- 1-pctAggressive
    
    if(pctCp == 0) {}
    
    # if IEF momentum is positive, invest all crash protection allocation into it
    # otherwise stay in cash for crash allocation
    if(assetMoms["IEF"] > 0) {
      investedAssets["IEF"] <- investedAssets["IEF"] + pctCp
    }
    
    # leverage portfolio if desired in cases when both risk indicator assets have positive momentum
    if(pctAggressive == 1) {
      investedAssets = investedAssets * leverageFactor
    }
    
    # append to list of monthly allocations
    wts <- xts(investedAssets, order.by=last(index(retSubset)))
    allWts[[i]] <- wts
    
  }
  
  # put all weights together and compute cash allocation
  allWts <- do.call(rbind, allWts)
  allWts$CASH <- 1-rowSums(allWts)
  
  # add cash returns to universe of investments
  investedRets <- rets[,1:10]
  investedRets$CASH <- 0
  
  # compute portfolio returns
  out <- Return.portfolio(R = investedRets, weights = allWts)
  return(out)
}

# different leverages
KDA_100 <- KDA(rets, leverageFactor = 1)
KDA_150 <- KDA(rets, leverageFactor = 1.5)
KDA_200 <- KDA(rets, leverageFactor = 2)

# compare
compare <- na.omit(cbind(KDA_100, KDA_150, KDA_200))
colnames(compare) <- c("KDA_base", "KDA_lev_150%", "KDA_lev_200%")
charts.PerformanceSummary(compare, colorset = c('black', 'purple', 'gold'), 
                          main = "KDA AA with various offensive leverages")
