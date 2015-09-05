


d <- read.csv2("data.csv", stringsAsFactors = F)
for (i in c(3:17,19)){d[,i] = as.numeric(d[,i])}

##########################################################################################################################################

# 1. Boxplot

boxplot_data <- split( d$Total.Return.YTD..I., d$BICS.L1.Sect.Nm )
windows()
par(mar = c(10,4,4,4))
boxplot(boxplot_data, las = 2, col = "grey")


##########################################################################################################################################

# 2. Scatter

model <- lm(" Total.Return.YTD..I. ~ Market.Cap.Y.1", data = d)
a <- model$coefficients[1]
b <- model$coefficients[2]
windows()
plot(d$Market.Cap.Y.1,d$Total.Return.YTD..I., xlim = c(0, 400000000000), xlab = "Market Cap Y-1", ylab = "Total Return YTD (I).")
abline(a,b, col = "red")

##########################################################################################################################################

# 3. Correl

d_filt <- na.omit(d)[,setdiff(1:19, c(1,2,18))]
cor_mtx <- cor(d_filt)
print(round(cor_mtx, 3))

##########################################################################################################################################

# 4. linear regression
library(MASS)
vars <- colnames(d_filt)
m <- length(vars)
lin_formula <- paste(vars[m], paste(vars[-m], collapse = " + "), sep = " ~ ")

fit <- lm(formula = lin_formula, data = d_filt)
fit <- stepAIC(object = fit, direction = "backward", k = 4)
summary(fit)

fit2 <- lm(formula =  "Total.Return.YTD..I. ~ 1", data = d_filt)  
fit2 <- stepAIC(object = fit2, direction = "forward", k = 2)
summary(fit2)


############################################################################################################################################

# 5. dendrogram

library(stats)
library(matrixStats)
h_clust <- hclust(dist(d[,19]))
windows()
plot(h_clust, labels = F, xlab = "")

###############################################################################################################################################

# 6. cluster

k_clust <- kmeans(d[,19], 3)
K_means_results <- cbind(k_clust$centers, k_clust$size)
colnames(K_means_results) = c("Cluster center", "Cluster size")
print(K_means_results)

###############################################################################################################################################

# 7. ANOVA

for(i in c(3,4,6,10,12,14,16,17)) { print(colnames(d)[i]); print(summary(aov(d[,i]~k_clust$cluster  , d))) }

###############################################################################################################################################

# 8. kmeans_check

f <- function(x) c(mean = mean(x, na.rm = T), N = length(x[!is.na(x)]), sd = sd(x, na.rm = T))
output <- aggregate(d[c(19,3,4,6,10,12,14,16,17)], list(k_clust$cluster), f)
rownames(output) = output[,1]; output[,1] <- NULL
output <- t(output)
output <- output[,order(output[1,])]
output <- cbind(output, as.vector(apply(d[c(19,3,4,6,10,12,14,16,17)], 2, f)))
colnames(output) <- c("Underperformers", "Midrange", "Overperformers", "Total")
options(scipen=999)
print(round(output,3))

###############################################################################################################################################

# 9. decision tree

d_tree <- d[,c(3:17,19)]

vars <- colnames(d_tree)
m <- length(vars)
tree_formula <- paste(vars[m], paste(vars[-m], collapse = " + "), sep = " ~ ")

library(rpart)
tree <- rpart(formula = tree_formula, data = d_tree, maxdepth = 5 ,cp = 0.001)
tree <- prune(tree, cp = 0.003)
par(xpd = T)
windows()
plot(tree)
text(tree, cex = .5, use.n = T, all = T)


###############################################################################################################################################

# 10. backtest

# tree

d$condition1 <- (d[,3]  >   1.6) 
d$condition2 <- (d[,4]  >  12.3) 
d$condition3 <- (d[,5]  <   398) 
d$condition4 <- (d[,10] <  1.66) 
d$condition5 <- (d[,13] >  43.5)
d$selected1 <- d$condition1 & d$condition2 & d$condition3 & d$condition4 & d$condition5
d$condition6 <- (d[,3]  <   1.6)
d$condition7 <- (d[,5]  >  2156) 
d$selected2  <- d$condition6 & d$condition7
d$tree <- d$selected1 | d$selected2

f <- function(x) c(mean(x), length(x), sd(x), median(x))
report <- aggregate( x = d[,19], by = list(d$tree), FUN = f )$x
colnames(report) = c("mean","N","standard deviation","median")
report <- rbind(report, f(d[,19]))
rownames(report) <- c("Not selected","Selected","Total")
print(report)

# cluster

d$cluster = k_clust$cluster
z <- round(cbind( 
t(aggregate(d[,c(19,3,4,6,10,12,14,16,17)], list(d$cluster) ,function(x) mean(x, na.rm = T))),
t(aggregate(d[,c(19,3,4,6,10,12,14,16,17)], list(d$cluster) ,function(x) median(x, na.rm = T))))[-1,], 2)
colnames(z) = c("1-mean","2-mean","3-mean","1-median", "2-median", "3-median")
print(z)

# cluster check

d$selected <- (d[,3] <= 14) & (d[,4] >= 23) & (d[,10] <= 1.7) & (d[,12] >= 11) & (d[17] <= 20)
d$selected[is.na(d$selected)] <- FALSE
h <- function(x) c(mean(x, na.rm = T), length(x[!is.na(x)]), sd(x, na.rm = T), median(x, na.rm = T))
backtest <- aggregate(d[,19], list(d$selected), h)
backtest <- backtest$x
backtest <- rbind(backtest, h(d[,19]))
colnames(backtest) = c("mean", "N", "Stdev", "Median")
rownames(backtest) = c("Not selected", "Selected", "Total")
print(backtest)

# cluster vs. tree 

d$tree <- tree$where %in% c(13,17)
crosstable <- table(d$selected, d$tree)
rownames(crosstable) = c("cluster-0","cluser-1")
colnames(crosstable) = c("tree-0","tree-1")
crosstable <- addmargins(crosstable)
print(crosstable)

# 11. industry specific investment

# tree

d_comm <- d[d[,18] == "Communications",c(3:17,19)]
vars <- colnames(d_comm)
m <- length(vars)
tree_formula <- paste(vars[m], paste(vars[-m], collapse = " + "), sep = " ~ ")
library(rpart)
tree <- rpart(formula = tree_formula, data = d_comm, maxdepth = 5 ,cp = 0.01, control = rpart.control(minsplit = 100))
tree <- prune(tree, cp = 0.006)
par(xpd = T)
windows()
plot(tree)
text(tree, cex = .5, use.n = T, all = T)
print(tree)

# crosstab

t1 <- aggregate(d[ d$tree,19], list(d[ d$tree,18]), function(x) c(mean(x), median(x))) 
t2 <- aggregate(d[!d$tree,19], list(d[!d$tree,18]), function(x) c(mean(x), median(x)))
industry_crosstab <- round(cbind(t1$x, t2$x),4)
colnames(industry_crosstab) <- c("mean-1","median-1","mean-0","median-0")
rownames(industry_crosstab) <- t1[,1]
print(industry_crosstab)
















