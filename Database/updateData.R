# Update datafiles
library(quantmod)
lookback = 60
startDate = Sys.Date() - lookback
thePath = "C://Users/Toshiba/Google Drive/Research/GIT/Trading/Database/"
theFiles = list.files(path = thePath, pattern = ".csv")

for(ii in theFiles){
  data = read.csv(paste(thePath, ii, sep = ""))
  data = xts(data[ ,c("open", "high", "low", "close", "volume", "adj")],
             order.by = as.Date(data[,"Index"], format = "%Y-%m-%d"))
  lastHistoricalDate = index(data[nrow(data), ])
  
  recent = getSymbols(Symbols = substr(ii, 1, nchar(ii) -4), 
                      scr = "yahoo", 
                      from = startDate, 
                      auto.assign = FALSE)
  colnames(recent) = c("open", "high", "low", "close", "volume", "adj")
  pos = match(as.Date(lastHistoricalDate, format = "%Y-%m-%d"), index(recent))
  
  if(!is.na(pos)){
    if(pos == nrow(recent))
    print("file already up to date")
    
    if(pos < nrow(recent)){
      dt = NULL
      dt = rbind(data, recent[(pos + 1):nrow(recent), ])
      write.zoo(dt, paste(thePath, ii, sep = ""), sep = ",", row.names = FALSE)
    }
    
  }  
if(is.na(pos))
  print("Error: dates do not match")
}
