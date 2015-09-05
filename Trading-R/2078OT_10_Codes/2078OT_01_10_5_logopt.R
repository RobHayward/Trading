

all_files <- list.files("data")
d <- read.table( paste("data",all_files[[1]], sep = "\\"), sep = ",", header = F)
colnames(d) = c("date", substr(all_files[[1]], 1, nchar(all_files[[1]])-4))
for ( i in 2:length(all_files) ) {
d2 <- read.table( paste("data",all_files[[i]], sep = "\\"), sep = ",", header = F)
colnames(d2) = c("date", substr(all_files[[i]], 1, nchar(all_files[[i]])-4))
d <- merge(d, d2, sort = F)
}

simulation <- function(d){

a <- Position( function(x) substr(x,1,2) == "96", d[,1])
b <- Position( function(x) substr(x,1,2) == "97", d[,1])

result <- log_optimization(d[b:a,])
result <- cbind(result, 1- sum(result))
result <- cbind(result, sum(result * d[b+1,2:6]), sum(rep(1/5,5) * d[b+1,2:6]))
colnames(result) = c("w1","w2","w3","w4","w5","Total return", "Benchmark")

N <- 25
for (i in 1:2490){
print(i)
h <- log_optimization(d[b:a+i,])
h <- cbind(h, 1- sum(h))
h <- cbind(h, sum(h * d[b+1+i,2:6]), sum(rep(1/5,5) * d[b+1+i,2:6]))
result <- rbind(result,h)
}

return(result)
}







log_opt <- function(x,d,r = NA){
x <- c(x, 1-sum(x))
n <- ncol(d)-1
d["distance"] <- c(1,dist(d[2:ncol(d)])[1:(nrow(d)-1)])
if(is.na(r)) r <- quantile(d$distance, 0.05)
d["similarity"] <- d$distance <= r
d["similarity"] <- c(d[2:nrow(d),"similarity"],0)
d <- d[d["similarity"]==1,]
log_return <- log(as.matrix(d[,2:(n+1)]) %*% x)
return(sum(log_return))
}

log_optimization <- function(d,r = NA){
today <- d[1,1]
m <- ncol(d)
constr_mtx <- rbind(diag(m-2), rep(-1,m-2))
b <- c(rep(0,m-2),-1)
opt <- constrOptim(rep(1/(m-1),m-2),function(x) -1*log_opt(x,d), NULL, constr_mtx, b)
result <- rbind(opt$par)
rownames(result) <- today
return(result)
}


A <- simulation(d)

print(A)



