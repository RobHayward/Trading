da <- read.csv("C:/Users/Toshiba/Desktop/f_year.txt", stringsAsFactors = 
                 FALSE)
# extract the contact names. 
names <- unique(da[,1])
daE <- da[which(da[,1] == names[5]),]
str(daE[,3])


     