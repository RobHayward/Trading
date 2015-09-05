install.packages("bigmemory")
install.packages("biganalytics")

library(bigmemory)
library(biganalytics)

x<-read.big.matrix("C:/Users/Julia Molnar-BDP/Desktop/R/Flight/Flight_ticket3.csv", type='integer', header=TRUE, backingfile="data.bin",descriptorfile="data.desc")
xm<-as.matrix(x)
nrow(x)

res_bigkmeans <- lapply(1:10, function(i) {
 bigkmeans(x, centers=i,iter.max=50,nstart=1)
 })
lapply(res_bigkmeans, function(x) x$withinss)
var <- sapply(res_bigkmeans, function(x) sum(x$withinss))
plot(1:10, var, type = "b", xlab = "Number of clusters", ylab = "Percentage of variance explained")


res_big<-bigkmeans(x, centers=3,iter.max=50,nstart=1)
res_big


size<-round(seq(10,2500000,length=20))
nsize<-length(size)
calc.time <- matrix(NA, nrow=nsize, ncol=2)
for (i in 1:nsize) {
 size.i<-size[i]
 xm.i<-xm[1:size.i,]
vec1=rep(0,10)
vec2=rep(0,10)
for (j in 1:10) {
vec1[j]<-system.time(kmeans(xm.i,centers=3,iter.max=50,nstart=1))[3]
vec2[j]<-system.time(bigkmeans(xm.i,centers=3,iter.max=50,nstart=1))[3]
}
calc.time[i,1]<-mean(vec1)
calc.time[i,2]<-mean(vec2)
}

matplot(size, calc.time, type="l", ylab="calc time")



