########################################################################
# bankALM library script
#
# pre-defined functions for the Asset and Liability Management chapter
#
# Authors: Daniel Havran and Istvan Margitai
# 2014 October
#
########################################################################



########################################################################
#required libraries
# xts, zoo, YieldCurve, reshape

if(require("xts")){
  print("xts is loaded correctly")
} else {
  print("trying to install xts")
  install.packages("xts")
  if(require("xts")){
    print("xts installed and loaded")
  } else {
    stop("could not install xts")
  }
}

if(require("zoo")){
  print("zoo is loaded correctly")
} else {
  print("trying to install zoo")
  install.packages("zoo")
  if(require("zoo")){
    print("zoo installed and loaded")
  } else {
    stop("could not install zoo")
  }
}

if(require("YieldCurve")){
  print("YieldCurve is loaded correctly")
} else {
  print("trying to install YieldCurve")
  install.packages("YieldCurve")
  if(require("YieldCurve")){
    print("YieldCurve installed and loaded")
  } else {
    stop("could not install YieldCurve")
  }
}

if(require("reshape")){
  print("reshape is loaded correctly")
} else {
  print("trying to install reshape")
  install.packages("reshape")
  if(require("reshape")){
    print("reshape installed and loaded")
  } else {
    stop("could not install reshape")
  }
}

if(require("car")){
  print("car is loaded correctly")
} else {
  print("trying to install car")
  install.packages("car")
  if(require("car")){
    print("car installed and loaded")
  } else {
    stop("could not install reshape")
  }
}

########################################################################



#####################################################
cf<-function(rate=0, maturity=1, volume=1, type)
{
  if (type=="BULLET")
  {
    interest          <- rep_len(rate,maturity)*volume    
    capital           <- rep_len(0, maturity)
    capital[maturity] <- volume
    remaining         <- c(volume, volume - capital)
    cashflow          <- capital + interest  
  }
  if (type=="LINEAR")
  {
    capital           <- rep_len(1/maturity,maturity)*volume
    remaining         <- c(volume,volume-cumsum(capital))
    interest          <- head(remaining,maturity)*rate
    cashflow          <- capital + interest
  }
  if (type=="ANNUITY")
  {
    interest          <- rep_len(0, maturity)    
    capital           <- rep_len(0, maturity)
    cashflow          <- rep_len(0, maturity)
    remaining         <- volume
    if (length(rate==1))  {rate <- rep_len(rate, maturity)}    
    for (i in 1:maturity)
    {
      cashflow[i]     <- (rate[i] / (1 - (1+rate[i])^(-(maturity-i+1) )))*remaining[i]
      if (rate[i]==0){cashflow[i]<-1/(maturity-i+1)*remaining[i]}
      interest[i]     <- remaining[i]*rate[i]
      capital[i]      <- cashflow[i] -  interest[i]
      remaining[i+1]  <- remaining[i] - capital[i]
    }
  }
  cf <- structure(list(cashflow=cashflow, interest=interest, 
                       capital=capital, remaining=remaining[2:(maturity+1)]))
  return(cf)
}  
#####################################################


#####################################################
#here dates consist the payment dates
get.yieldcurve.spot<- function(market, dates, type="EUR01", now=Sys.Date(), showplot=FALSE)
{
  market.maturity  = as.vector( (market$date[market$type==type]-now)/365.25 )
  market.yields    = xts( t(market$rate[market$type==type]), now )  
  maturity         = as.vector( (as.Date(dates)-now)/365.25 )
  coeffs           = Svensson(market.yields, market.maturity)
  yieldcurve.spot  = data.frame(date=as.Date(dates),
                                rate=as.numeric(t(Srates(coeffs, maturity, "Spot" ))) )
  
  if (is.na(yieldcurve.spot$rate[1])==TRUE) {yieldcurve.spot$rate[1]=yieldcurve.spot$rate[2]}
  
  if (showplot==TRUE) {
    plot(market.maturity, market.yields,
          xlab=c("Pillars in years"), ylab="rate", type="p", col=3)
    title(main="Fitted yield curve (Svensson method)",
          sub =paste("Actual date: ",as.character(as.Date(NOW))),
          cex=0.8)
    lines(maturity, yieldcurve.spot$rate,col=2, type="l")
    legend("topleft",
           legend=c("observed yield curve","fitted yield curve"), 
           col=c(3,2),lty=c(0,1), pch=c("o",""), bty = "n")
    grid()            }
  
  return(yieldcurve.spot)
}
#####################################################


#####################################################
#here dates consist the repricing dates
get.yieldcurve.forward<- function(market, dates, type="EUR01", now=Sys.Date())
{    
  yieldcurve.spot    = get.yieldcurve.spot(market, dates, type=type, now=now)
  
  maturity           = as.vector((as.Date(dates)-now)/365.25)  
  yieldcurve.forward = data.frame(date=dates,
                                  rate=  (
                                    (1+yieldcurve.spot$rate/10000)^maturity /
                                      c(1,((1+yieldcurve.spot$rate/10000)^maturity)[1:length(maturity)-1])  
                                    -1)*10000  )
  
  return(yieldcurve.forward)
}  
#####################################################


#####################################################
#modify yieldcurve according to the repricing periods
#here dates constist the payment dates
get.floating <- function(market, payment.dates, reprice.dates, 
                         type="EUR01", now=Sys.Date(), showplot=FALSE)
{
  
  yieldcurve.forward.original = get.yieldcurve.forward(market, reprice.dates, 
                                              type="EUR01", now=Sys.Date())
  
  #and merge vector of forwards with these dates (with imputation)
  yieldcurve.forward = merge(payment.dates, 
                             yieldcurve.forward.original, by=1, all.x=TRUE, all.y=TRUE)
  
  yieldcurve.spot = get.yieldcurve.spot(market, reprice.dates, 
                                        type="EUR01", now=Sys.Date())
  yieldcurve.forward$rate[1]=yieldcurve.spot$rate[1]
  for (i in 1:NROW(yieldcurve.forward)) 
  { if (is.na(yieldcurve.forward$rate[i])==TRUE) 
  { yieldcurve.forward$rate[i]<-yieldcurve.forward$rate[i-1] }
  }
  
  floating = merge(payment.dates, 
                             yieldcurve.forward, by=1, all.x=TRUE)
  
  if (showplot==TRUE) {
    plot(yieldcurve.forward.original, type="b", col="green",
         xlab="Maturity", ylab="rate")
    lines(floating, type="s", col="red")
    title(main="Forward curve and floating rate forecast",
          sub =paste("Actual date: ",as.character(as.Date(NOW))),
          cex=0.8)
    legend("topleft",
           legend=c("forward yield curve","forecasted floating rate"), 
           col=c(3,2),lty=c(1,1), pch=c("o",""), bty = "n")
    grid()            }
  
  return(floating)
}
#####################################################

#ok
#####################################################
cf.table<-function(portfolio, market, now=Sys.Date(), id)
{  
  
  payment.periods = sort(seq(from = as.Date(portfolio$maturity[id],    format = "%m/%d/%Y"), 
                             to   = now+1,
                             by   = "-1 month") )  
  payment.freq  = ifelse(is.na(portfolio$payment_freq[id]),1,portfolio$payment_freq[id])
  payment.dates = payment.periods[seq(from=1, 
                                      to  =length(payment.periods), 
                                      by  =payment.freq)]   
  
  if (portfolio$ir_binding[id]=="FIX") {
    r = portfolio$spread[id]
  }  else  {
    
    reprice.periods  =  seq(from = as.Date(portfolio$issue[id],    format = "%m/%d/%Y"), 
                            to   = as.Date(portfolio$maturity[id],    format = "%m/%d/%Y"),
                            by   = "1 month")
    reprice.freq = ifelse(is.na(portfolio$reprice_freq[id]),1,portfolio$reprice_freq[id])    
    reprice.dates = reprice.periods[seq(from=1, 
                                        to  =length(reprice.periods), 
                                        by  =reprice.freq)] 
    reprice.dates =reprice.dates[reprice.dates>=now]
    
    if (length(reprice.dates)>0) {
      floating = get.floating(market, payment.dates, reprice.dates) 
      r        = portfolio$spread[id] + floating$rate
    } else{ r = portfolio$spread[id] }
  }
  
  cft = cf(rate     = r/10000*(payment.freq/12),  #rate is in basis points
           maturity = length(payment.dates), 
           volume   = portfolio$volume[id], 
           type     = as.character(portfolio$repayment[id]))  
  
  cf.table <- data.frame(
    id      =portfolio$id[id],
    account =portfolio$account[id],
    date    =payment.dates,
    cf      =cft$cashflow,
    interest=cft$interest,
    capital =cft$capital, 
    remaining=cft$remaining)   
  return(cf.table)
}
#####################################################



#present value
#####################################################
pv.table<-function(cashflow.table, market, now=Sys.Date())
{
  #payment.dates = (cashflow.table[cashflow.table$id==id,])$date
  payment.dates   = cashflow.table$date
  yieldcurve.spot = get.yieldcurve.spot(market, payment.dates, 
                                        type="EUR01", now=Sys.Date())
  
  table= merge(cashflow.table,yieldcurve.spot, x.by=date, y.by=date, all.x=TRUE)
  #table= table[table$date>=now,]
  
  ts = as.vector((table$date-now)/365.25)
  df = (1 + table$rate/10000)^-ts   #rate is in basis points
  
  pv.table<- data.frame(id           =cashflow.table$id[1],
                        account      =cashflow.table$account[1],
                        date         =now,
                        presentvalue =sum(table$cf * df) )
  
  return(pv.table) 
}
#####################################################


#library(reshape)
#####################################################
#Liquidity forecast
lq.table<-function(cashflow.table, now=Sys.Date()) #id-t atirtam accountra mindenhol!!
{
  periods.days  <- c(0,30,90,180,360,720,1800,3600,7200)
  periods.names <- c("1M","2-3M","3-6M","6-12M","1-2Y","2-5Y","5-10Y",">10Y")
  
  N           <- nlevels(as.factor(cashflow.table$account))
  table       <- cast(cashflow.table, date ~ account,  value="cf", fun=sum)
  table       <- table[table$date>=now,]  
  table$total <- as.vector(t(rowSums(table[,2:(2+N-1)], na.rm=TRUE)))
  table$days  <- table$date-now
  table$period  <- cut(as.numeric(table$days),
                       breaks=as.numeric(periods.days))
  
  lq.table<-aggregate(table[,2:(2+N)], 
                      list(time=table$period), sum, na.rm=TRUE)
  rownames(lq.table)<-periods.names[1:length(rownames(lq.table))]
  lq.table          <- t(subset(lq.table, select = -c(time) ))
  
  lqt<-data.frame(id=row.names(lq.table),                  
                  M1=lq.table[,2],
                  M2=lq.table[,3],
                  M3=lq.table[,4],
                  M6=lq.table[,5],
                  M7=lq.table[,6],
                  M8=lq.table[,7],
                  M9=lq.table[,8])
  return(lq.table)
}
#####################################################


#####################################################
#net interest income forecast
nii.table<-function(cashflow.table, now=Sys.Date())
{
  N            <- nlevels(as.factor(cashflow.table$account))
  table        <- cast(cashflow.table, date ~ account,  value="interest", fun=sum)
  table        <- table[table$date>=now,]
  table$total  <- as.vector(t(rowSums(table[,2:(2+N-1)],  na.rm=TRUE)))
  table$year   <- format(as.Date(table$date), "%Y")
  nii.table    <- aggregate(table[,2:(2+N)], 
                            list(time=table$year), sum, na.rm=TRUE)
  rownames(nii.table)<-nii.table$time
  nii.table<-t(subset(nii.table, select = -c(time) ))  
  return(nii.table)
}
#####################################################


#####################################################
repricing.gap<-function(portfolio,id, now=Sys.Date())
{
  payment.periods = sort(seq(from = as.Date(portfolio$maturity[id],    format = "%m/%d/%Y"), 
                             to   = now+1,
                             by   = "-1 month") )  
  payment.freq  = ifelse(is.na(portfolio$payment_freq[id]),1,portfolio$payment_freq[id])
  payment.dates = payment.periods[seq(from=1, 
                                      to  =length(payment.periods), 
                                      by  =payment.freq)]   
  
  if (portfolio$ir_binding[id]=="FIX") { 
    volume=rep(0,12)
  }  else  {    
    reprice.periods  =  seq(from = as.Date(portfolio$issue[id],    format = "%m/%d/%Y"), 
                            to   = as.Date(portfolio$maturity[id],    format = "%m/%d/%Y"),
                            by   = "1 month")
    reprice.freq  = ifelse(is.na(portfolio$reprice_freq[id]),1,portfolio$reprice_freq[id])    
    reprice.dates = reprice.periods[seq(from=1, 
                                        to  =length(reprice.periods), 
                                        by  =reprice.freq)] 
    reprice.dates=reprice.dates[reprice.dates>=now]      
    reprice.days =as.numeric(reprice.dates-now)  
    periods.days =as.numeric(c(0,1,2,3,4,5,6,7,8,9,10,11,12)*30)
    idx          =findInterval(reprice.days, periods.days)
    volume       =tabulate(idx[idx<=12],nbins=12 )*portfolio$volume[id]
  }
  repricing.gap  <-data.frame(volume=volume)
  return(repricing.gap)
}
#####################################################


#####################################################
repricing.gap.table<-function(portfolio, now=Sys.Date())
{
  repricing.gap.table<-data.frame(volume=rep(0,12))
  for (i in 1:NROW(portfolio))
  { repricing.gap.table <- repricing.gap.table +repricing.gap(portfolio, i, now)
  } 
  periods.names = c("1M","2M","3M","4M","5M","6M","7M","8M","9M","10M","11M","12M")
  repricing.gap.table<-t(repricing.gap.table)
  colnames(repricing.gap.table)<-periods.names
  return(repricing.gap.table)
}
#####################################################



