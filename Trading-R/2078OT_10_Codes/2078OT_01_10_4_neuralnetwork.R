# load data 
data<-read.csv("Bitcoin.csv", header=T, sep = ",")
data2<-data[order(as.Date(data$Date, format="%Y-%m-%d")),]

price<-(data2$Close)
HLC<-matrix(c(data2$High, data2$Low, data2$Close),nrow=length(data2$High))

# calculate log returns
bitcoin.lr<-diff(log(price))

# install and load TTR package 
install.packages("TTR")
library(TTR)

# generate technical indicators 
rsi<-RSI(price)
MACD <- MACD(price)
macd<-MACD[,1]
will<-williamsAD(HLC)
cci<-CCI(HLC)
STOCH<-stoch(HLC)
stochK<-STOCH[,1]
stochD<-STOCH[,1]

# create the Input and Target matrix for training and validation dataset
Input<-matrix(c(rsi[700:939], cci[700:939], macd[700:939], will[700:939], stochK[700:939], stochD[700:939]),nrow=240)
Target<-matrix(c(bitcoin.lr[701:940]), nrow=240)

trainingdata <- cbind(Input,Target)
colnames(trainingdata) <- c("RSI","CCI","MACD","WILL","STOCHK","STOCHD", "Return")

# install and load caret package
install.packages("caret")
library(caret)

# split the dataset 90-10% ratio
trainIndex <- createDataPartition(bitcoin.lr[701:940], p=.9, list=F)
bitcoin.train <- trainingdata[trainIndex, ]
bitcoin.test <- trainingdata[-trainIndex, ]

# install and load nnet package
install.packages("nnet")
library(nnet)

# derive the best neural network model using rmse criteria 
best.network<-matrix(c(5,0.5))
best.rmse<-1
for (i in 5:15) for (j in 1:3) {
bitcoin.fit <- nnet(Return ~ RSI + CCI + MACD + WILL + STOCHK + STOCHD, data = bitcoin.train, 
                    maxit=1000, size=i, decay=0.01*j, linout = 1)

bitcoin.predict <- predict(bitcoin.fit, newdata = bitcoin.test)
bitcoin.rmse <- sqrt(mean((bitcoin.predict - bitcoin.lr[917:940])^2)) 
if (bitcoin.rmse<best.rmse) {
  best.network[1,1]<-i
  best.network[2,1]<-j
  best.rmse<-bitcoin.rmse  
}
}

# create the Input and Target matrix for test
InputTest<-matrix(c(rsi[940:969], cci[940:969], macd[940:969], will[940:969], stochK[940:969], stochD[940:969]),nrow=30)
TargetTest<-matrix(c(bitcoin.lr[941:970]), nrow=30)

Testdata <- cbind(InputTest,TargetTest)
colnames(Testdata) <- c("RSI","CCI","MACD","WILL","STOCHK","STOCHD", "Return")

# fit the best model on test data
bitcoin.fit <- nnet(Return ~ RSI + CCI + MACD + WILL + STOCHK + STOCHD, data = trainingdata, 
                    maxit=1000, size=best.network[1,1], decay=0.1*best.network[2,1], linout = 1) 

bitcoin.predict1 <- predict(bitcoin.fit, newdata = Testdata)

# repeat and average the model 20 times  
for (i in 1:20) {
bitcoin.fit <- nnet(Return ~ RSI + CCI + MACD + WILL + STOCHK + STOCHD, data = trainingdata, 
                      maxit=1000, size=best.network[1,1], decay=0.1*best.network[2,1], linout = 1) 

bitcoin.predict<- predict(bitcoin.fit, newdata = Testdata)
bitcoin.predict1<-(bitcoin.predict1+bitcoin.predict)/2
}

# calculate the buy-and-hold benchmark strategy and neural network profit on the test dataset
money<-matrix(0,31)
money2<-matrix(0,31)
money[1,1]<-100
money2[1,1]<-100
for (i in 2:31) {
  if (bitcoin.predict1[i-1]<0) {
  direction1<--1  
  } else {
  direction1<-1}
  if (TargetTest[i-1]<0) {
    direction2<--1  
  } else {
    direction2<-1 }
  if ((direction1-direction2)==0) {
  money[i,1]<-money[i-1,1]*(1+abs(TargetTest[i-1]))  
  } else {
  money[i,1]<-money[i-1,1]*(1-abs(TargetTest[i-1])) }
  money2[i,1]<-100*(price[940+i-1]/price[940])
}

#plot benchmark and neural network profit on the test dataset
x<-1:31
matplot(cbind(money, money2), type = "l", xaxt = "n", ylab = "")
legend("topright", legend = c("Neural network","Benchmark"), pch = 19, col = c("black", "red"))
axis(1, at = c(1,10,20,30), lab  = c("2014-04-08", "2014-04-17", "2014-04-27", "2014-05-07" ))

box()
mtext(side = 1, "Test dataset", line = 2)
mtext(side = 2, "Investment value", line = 2)