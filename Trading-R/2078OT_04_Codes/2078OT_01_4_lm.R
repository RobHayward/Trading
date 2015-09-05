install.packages("ff")
install.packages(“biglm”)

library(ff)
library(biglm)

download.file("http://www.irs.gov/file_source/pub/irs-soi/12zpallagi.csv","soi.csv")

x <- read.csv.ffdf(file="soi.csv",header=TRUE)

require(biglm)

mymodel<-biglm(A02300 ~  A00200+AGI_STUB+NUMDEP+MARS2,data=x)
summary(mymodel)

