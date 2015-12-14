#Daily prices from Yahoo
library(quantmod)
startDate = "2000-01-01"
thePath = "C://Users/Toshiba/Google Drive/Research/GIT/Trading/Database/"
source(paste(thePath, "/listofInstruments.r", sep = ""))
for(ii in theInstruments){
  print(ii)
  data = getSymbols(Symbols = ii, 
                    scr = "yahoo", 
                    from = startDate, 
                    auto.assign = FALSE)
colnames(data) <- c("open", "high", "low", "close", "volume", "adj")
write.zoo(data, paste(thePath, ii, ".csv", sep = ""), sep = ",", row.names = FALSE)
}

       
       