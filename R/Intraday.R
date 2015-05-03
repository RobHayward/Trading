###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
# This will use the windows download from SIT site
###############################################################################
con = gzcon(url('https://github.com/systematicinvestor/
SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)
# or http://systematicinvestor.wordpress.com/systematic-investor-toolbox/ 
    #*****************************************************************
    # Load historical data
    #****************************************************************** 
    require('quantmod')   
 
    # data from http://thebonnotgang.com/tbg/historical-data/
    # Username RobHayward password set.     
spath = 'Data/'
    # http://stackoverflow.com/questions/14440661/dec-argument-in-data-tablefread
    # I don't think that this is necessary
    # Sys.setlocale("LC_NUMERIC", "decimal_point")
    # Sys.setlocale("LC_NUMERIC", "French_France.1252")
    # Sys.localeconv()[]
    data <- new.env()
    data$SPY <- read.csv2(paste0(spath,'SPY_1m.csv'),sep = ';', 
                          stringsAsFactors= FALSE)
    data$SPY$timestamp <- as.POSIXlt(data$SPY$Timestamp, 
                                     format = "%Y-%m-%d %H:%M:%S")
    data$SPY <- as.xts(data$SPY, date.column = 3, format='%Y-%m-%d %H:%M:%S', 
                       index.class = c("POSIXlt", "POSIXt"))
 # this needs work.  It will not convert
    data$GLD = read.xts(paste0(spath,'GLD_1m.csv'), 
        sep = ';', date.column = 3, format='%Y-%m-%d %H:%M:%S', index.class = c("POSIXlt", "POSIXt"))
  head(data$SPY)
  str(data$SPY)
    #*****************************************************************
    # Create plot for Nov 1, 2012 and 2013
    #****************************************************************** 
    layout(c(1,1,2))        
    plota(data$SPY['2012:11:01'], type='candle', main='SPY on Nov 1st, 2012', plotX = F)
    plota(plota.scale.volume(data$SPY['2012:11:01']), type = 'volume')  
 
    layout(c(1,1,2))        
    plota(data$SPY['2013:11:01'], type='candle', main='SPY on Nov 1st, 2013', plotX = F)
    plota(plota.scale.volume(data$SPY['2013:11:01']), type = 'volume')  
