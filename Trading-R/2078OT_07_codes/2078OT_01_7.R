library(fOptions)
library(fExoticOptions)
a <- GBSOption("c", 100, 100, 1, 0.02, -0.02, 0.3, title = NULL,
    description = NULL)
(z <- a@price)
[1] 10.62678
a <- GeometricAverageRateOption("c", 100, 100, 1, 0.02, -0.02, 0.3,
    title = NULL, description = NULL)
(z <- a@price)




library(fExoticOptions)
a <- StandardBarrierOption("cuo", 100, 90, 130, 0, 1, 0.02, -0.02, 0.30,
    title = NULL, description = NULL)
x <- a@price
b <- StandardBarrierOption("cui", 100, 90, 130, 0, 1, 0.02, -0.02, 0.30, 
    title = NULL, description = NULL)
y <- b@price
c <- GBSOption("c", 100, 90, 1, 0.02, -0.02, 0.3, title = NULL, 
    description = NULL)
z <- c@price
(v <- z - x – y)



vanilla <- GBSOption(TypeFlag = "c", S = 100, X = 90, Time = 1,
    r = 0.02, b = -0.02, sigma = 0.3)
KO <- sapply(100:300, FUN = StandardBarrierOption, TypeFlag = "cuo",
    S = 100, X = 90, K = 0, Time = 1, r = 0.02, b = -0.02, sigma = 0.30)
plot(KO[[1]]@price, type = "l",
    xlab = "barrier distance from spot",
    ylab = "price of option",
    main = "Price of KO converges to plain vanilla")
abline(h = vanilla@price, col = "red")




BS_surface <- function(S, Time, FUN, ...) {
    require(plot3D)
    n <- length(S)
    k <- length(Time) 
    m <- matrix(0, n, k)
    for (i in 1:n){
        for (j in 1:k){
            l <- list(S = S[i], Time = Time[j], ...)
             m[i,j] <- max(do.call(FUN, l)@price, 0)
        }
    }
    persp3D(z = m, xlab = "underlying", ylab = "Remaining time",
        zlab = "option price", phi = 30, theta = 20, bty = "b2")
}

BS_surface(seq(1, 200,length = 200), seq(0, 2, length = 200),
    GBSOption, TypeFlag = "c", X = 90, r = 0.02, b = 0, sigma = 0.3)



BS_surface(seq(1,200,length = 200), seq(0, 2, length = 200),
    StandardBarrierOption, TypeFlag = "cuo", H = 130, X = 90, K = 0,
    r = 0.02, b = -0.02, sigma = 0.30)




GetGreeks <- function(FUN, arg, epsilon,...) {
    all_args1 <- all_args2 <- list(...)
    all_args1[[arg]] <- as.numeric(all_args1[[arg]] + epsilon)
    all_args2[[arg]] <- as.numeric(all_args2[[arg]] - epsilon)
    do.call(FUN, all_args1)@price -
        do.call(FUN, all_args2)@price / (2 * epsilon)
}



x <- seq(10, 200, length = 200)
delta <- vega <- theta <- rho <- rep(0, 200)
for(i in 1:200){
    delta[i] <- GetGreeks(FUN = FloatingStrikeLookbackOption, arg = 2,
        epsilon = 0.01, "p", x[i], 100, 1, 0.02, -0.02, 0.2)
    vega[i]  <- GetGreeks(FUN = FloatingStrikeLookbackOption, arg = 7, 
        epsilon = 0.0005, "p", x[i], 100, 1, 0.02, -0.02, 0.2)
    theta[i] <- GetGreeks(FUN = FloatingStrikeLookbackOption, arg = 4, 
        epsilon = 1/365, "p", x[i], 100, 1, 0.02, -0.02, 0.2)
    rho[i]   <- GetGreeks(FUN = FloatingStrikeLookbackOption, arg = 5, 
        epsilon = 0.0001, "p", x[i], 100, 1, 0.02, -0.02, 0.2)
}
par(mfrow = c(2, 2))
plot(x, delta, type = "l", xlab = "S", ylab = "", main = "Delta")
plot(x, vega,  type = "l", xlab = "S", ylab = "", main = "Vega")
plot(x, theta, type = "l", xlab = "S", ylab = "", main = "Theta")
plot(x, rho,   type = "l", xlab = "S", ylab = "", main = "Rho")




dnt1 <- function(S, K, U, L, sigma, T, r, b, N = 20, ploterror = FALSE){
    if ( L > S | S > U) return(0)
    Z <- log(U/L)
    alpha <- -1/2*(2*b/sigma^2 - 1)
    beta <- -1/4*(2*b/sigma^2 - 1)^2 - 2*r/sigma^2
    v <- rep(0, N)
    for (i in 1:N)
        v[i] <- 2*pi*i*K/(Z^2) * (((S/L)^alpha - (-1)^i*(S/U)^alpha ) /
            (alpha^2+(i*pi/Z)^2)) * sin(i*pi/Z*log(S/L)) *
             exp(-1/2 * ((i*pi/Z)^2-beta) * sigma^2*T)
    if (ploterror) barplot(v, main = "Formula Error");
    sum(v)
}
print(dnt1(100, 10, 120, 80, 0.1, 0.25, 0.05, 0.03, 20, TRUE))



dnt1 <- function(S, K, U, L, sigma, Time, r, b) {
    if ( L > S | S > U) return(0)
    Z <- log(U/L)
    alpha <- -1/2*(2*b/sigma^2 - 1)
    beta <- -1/4*(2*b/sigma^2 - 1)^2 - 2*r/sigma^2
    p <- 0
    i <- a <- 1
    while (abs(a) > 0.0001){
        a <- 2*pi*i*K/(Z^2) * (((S/L)^alpha - (-1)^i*(S/U)^alpha ) / 
            (alpha^2 + (i *pi / Z)^2) ) * sin(i * pi / Z * log(S/L)) * 
            exp(-1/2*((i*pi/Z)^2-beta) * sigma^2 * Time)
        p <- p + a
        i <- i + 1
    }
    p
}



x <- seq(0.92, 0.96, length = 2000)
y <- z <- rep(0, 2000)

for (i in 1:2000){
    y[i] <- dnt1(x[i], 1e6, 0.96, 0.92, 0.06, 0.25, 0.0025, -0.0250)
    z[i] <- dnt1(x[i], 1e6, 0.96, 0.92, 0.065, 0.25, 0.0025, -0.0250)
}
matplot(x, cbind(y,z), type = "l", lwd = 2, lty = 1,
    main = "Price of double barrier option with volatility 6% and 6.5%",    
    cex.main = 0.8, xlab = "Price of underlying" )




gamma <- function(FUN, epsilon, S, ...) {
    arg1 <- list(S, ...)
    arg2 <- list(S + 2 * epsilon, ...)
    arg3 <- list(S - 2 * epsilon, ...)
    y1 <- (do.call(FUN, arg2) - do.call(FUN, arg1)) / (2 * epsilon)
    y2 <- (do.call(FUN, arg1) - do.call(FUN, arg3)) / (2 * epsilon)
    (y1 - y2) / (2 * epsilon)
}

delta <- vega <- theta <- gamma <- rep(0, 200)

for(i in 1:200){
    delta[i] <- GetGreeks(FUN = dnt1, arg = 2, epsilon = 0.0001, x[i],
        1000000, 0.96, 0.92, 0.2, 0.5, 0.02, -0.02)
    vega[i]  <-   GetGreeks(FUN = dnt1, arg = 5, epsilon = 0.0005, x[i], 
        1000000, 0.96, 0.92, 0.06, 0.5, 0.0025, -0.025)
    theta[i] <- - GetGreeks(FUN = dnt1, arg = 6, epsilon = 1/365, x[i], 
        1000000, 0.96, 0.92, 0.06, 0.5, 0.0025, -0.025)
    gamma[i] <- gamma(FUN = dnt1, epsilon = 0.0001, S = x[i], K = 1e6,
       U = 0.96, L = 0.92, sigma = 0.2, T = 0.5, r = 0.02, b = -0.02) 
}
plot(x, vega, type = "l", xlab = "S",ylab = "", main = "Vega")

plot(x, delta, type = "l", xlab = "S",ylab = "", main = "Delta")

plot(x, gamma, type = "l", xlab = "S",ylab = "", main = "Gamma")

plot(x, theta, type = "l", xlab = "S",ylab = "", main = "Theta")


BS_surf <- function(S, Time, FUN, ...) {
    n <- length(S)
    k <- length(Time) 
    m <- matrix(0, n, k)
    for (i in 1:n) {
        for (j in 1:k) {
            l <- list(S = S[i], Time = Time[j], ...)
            m[i,j] <- do.call(FUN, l)
        }
     }
     persp3D(z = m, xlab = "underlying", ylab = "Time",
         zlab = "option price", phi = 30, theta = 30, bty = "b2")
}
BS_surf(seq(0.92,0.96,length = 200), seq(1/365, 1/48, length = 200),
    dnt1, K = 1000000, U = 0.96, L = 0.92, r = 0.0025, b = -0.0250,
    sigma = 0.2)



dnt2 <- function(S, K, U, L, sigma, T, r, b) {

    a <- DoubleBarrierOption("co", S, L, L, U, T, r, b, sigma, 0,
        0,title = NULL, description = NULL)
    z <- a@price
 
    b <- DoubleBarrierOption("po", S, U, L, U, T, r, b, sigma, 0,
        0,title = NULL, description = NULL)
    y <- b@price
 
   (z + y) / (U - L) * K
}



dnt1(0.9266, 1000000, 0.9600, 0.9200, 0.06, 0.25, 0.0025, -0.025)

dnt2(0.9266, 1000000, 0.9600, 0.9200, 0.06, 0.25, 0.0025, -0.025)



d <- read.csv2("audusd.csv")
underlying <- as.vector(t(d[, 2:5]))
t <- rep( d[,6], each = 4)
n <- length(t)
option_price <- rep(0, n)

for (i in 1:n) {
    option_price[i] <- dnt1(S = underlying[i], K = 1000000, U = 0.9600,
        L = 0.9200, sigma = 0.06, T = t[i]/(60*24*365), r = 0.0025,
        b = -0.0250)
}
a <- min(option_price)
b <- max(option_price)
option_price_transformed = (option_price - a) * 0.03 / (b - a) + 0.92

par(mar = c(6, 3, 3, 5))
matplot(cbind(underlying,option_price_transformed), type = "l",
    lty = 1, col = c("grey", "red"),
    main = "Price of underlying and DNT",
    xaxt = "n", yaxt = "n",  ylim = c(0.91,0.97),
    ylab = "", xlab = "Remaining time")
abline(h = c(0.92, 0.96), col = "green")
axis(side = 2, at = pretty(option_price_transformed),
    col.axis = "grey", col = "grey")
axis(side = 4, at = pretty(option_price_transformed),
    labels = round(seq(a/1000,1000,length = 7)), las = 2,
    col = "red", col.axis = "red")
axis(side = 1, at = seq(1,n, length=6),
    labels = round(t[round(seq(1,n, length=6))]/60/24))



dnt1(0.9203, 1000000, 0.9600, 0.9200, 0.06, 59/365, 0.0025, -0.025)



library(matrixStats)
DNT_sim <- function(S0 = 0.9266, mu = 0, sigma = 0.06, U = 0.96,
  L = 0.92, N = 5) {
    dt <- 5 / (365 * 24 * 60)
    t <- seq(0, 0.25, by = dt)
    Time <- length(t)

    W <- matrix(rnorm((Time - 1) * N), Time - 1, N)
    W <- apply(W, 2, cumsum)
    W <- sqrt(dt) * rbind(rep(0, N), W)
    S <- S0 * exp((mu - sigma^2 / 2) * t + sigma * W )
    option_price <- matrix(0, Time, N)

    for (i in 1:N)
       for (j in 1:Time)
          option_price[j,i] <- dnt1(S[j,i], K = 1000000, U, L, sigma,
               0.25-t[j], r = 0.0025,
               b = -0.0250)*(min(S[1:j,i]) > L & max(S[1:j,i]) < U)

    survivals <- sum(option_price[Time,] > 0)
    dev.new(width = 19, height = 10)

    par(mfrow = c(1,2))
    matplot(t,S, type = "l", main = "Underlying price",
        xlab = paste("Survived", survivals, "from", N), ylab = "")
    abline( h = c(U,L), col = "blue")
    matplot(t, option_price, type = "l", main = "DNT price",
        xlab = "", ylab = "")}
 
set.seed(214) 
system.time(DNT_sim())



dnt1(0.9266, 1000000, 0.9600, 0.9200, 0.06, 90/365, 0.0025, -0.025)


dnt1(0.9266, 1000000, 0.9600, 0.9195, 0.06, 94/365, 0.0025, -0.025)



dnt1(0.9266, 1000000, 1.0600, 0.9200, 0.06, 94/365, 0.0025, -0.025)


a <- BinaryBarrierOption(9, 0.9266, 0, 0.9200, 1000000, 94/365, 0.0025,
    -0.025, 0.06, 1, 1, title = NULL, description = NULL)
(z <- a@price)




dnt1(0.9266, 100, 0.9705, 0.9095, 0.06, 90/365, 0.0025, -0.025)




implied_DNT_image <- function(S = 0.9266, K = 1000000, U = 0.96,
  L = 0.92, sigma = 0.06, Time = 0.25, r = 0.0025, b = -0.0250) {

    S_ <- seq(L,U,length = 300)
    K_ <- seq(800000, 1200000, length = 300)
    U_ <- seq(L+0.01, L + .15, length = 300) 
    L_ <- seq(0.8, U - 0.001, length = 300) 
    sigma_ <- seq(0.005, 0.1, length = 300) 
    T_ <- seq(1/365, 1, length = 300)
    r_ <- seq(-10, 10, length = 300) 
    b_ <- seq(-0.5, 0.5, length = 300) 

    p1 <- lapply(S_, dnt1, K = 1000000, U = 0.96, L = 0.92,
        sigma = 0.06, Time = 0.25, r = 0.0025, b = -0.0250)
    p2 <- lapply(K_, dnt1, S = 0.9266, U = 0.96, L = 0.92,
        sigma = 0.06, Time = 0.25, r = 0.0025, b = -0.0250)
    p3 <- lapply(U_, dnt1, S = 0.9266, K = 1000000, L = 0.92,
        sigma = 0.06, Time = 0.25, r = 0.0025, b = -0.0250)
    p4 <- lapply(L_, dnt1, S = 0.9266, K = 1000000, U = 0.96,
        sigma = 0.06, Time = 0.25, r = 0.0025, b = -0.0250)
    p5 <- lapply(sigma_, dnt1, S = 0.9266, K = 1000000, U = 0.96,
        L = 0.92, Time = 0.25, r = 0.0025, b = -0.0250)
    p6 <- lapply(T_, dnt1, S = 0.9266, K = 1000000, U = 0.96, L = 0.92,
        sigma = 0.06, r = 0.0025, b = -0.0250)
    p7 <- lapply(r_, dnt1, S = 0.9266, K = 1000000, U = 0.96, L = 0.92, 
        sigma = 0.06, Time = 0.25,  b = -0.0250)
    p8 <- lapply(b_, dnt1, S = 0.9266, K = 1000000, U = 0.96, L = 0.92, 
        sigma = 0.06, Time = 0.25, r = 0.0025)
    
 dev.new(width = 20, height = 10)

    par(mfrow = c(2, 4), mar = c(2, 2, 2, 2))
    plot(S_, p1, type = "l", xlab = "", ylab = "", main = "S")
    plot(K_, p2, type = "l", xlab = "", ylab = "", main = "K")
    plot(U_, p3, type = "l", xlab = "", ylab = "", main = "U")
    plot(L_, p4, type = "l", xlab = "", ylab = "", main = "L")
    plot(sigma_, p5, type = "l", xlab = "", ylab = "", main = "sigma")
    plot(T_, p6, type = "l", xlab = "", ylab = "", main = "Time")
    plot(r_, p7, type = "l", xlab = "", ylab = "", main = "r")
    plot(b_, p8, type = "l", xlab = "", ylab = "", main = "b")
}

implied_vol_DNT <- function(S = 0.9266, K = 1000000, U = 0.96, L = 0.92,
  Time = 0.25, r = 0.0025, b = -0.0250, price) {
    f <- function(sigma)
        dnt1(S, K, U, L, sigma, Time, r, b) - price
    uniroot(f, interval = c(0.001, 100))$root

}

implied_U_DNT <- function(S = 0.9266, K = 1000000, L = 0.92,
  sigma = 0.06, Time = 0.25, r = 0.0025, b = -0.0250, price = 4) {
    f <- function(U)
        dnt1(S, K, U, L, sigma, Time, r, b) - price
    uniroot(f, interval = c(L+0.01, L + 100))$root
}
implied_T_DNT <- function(S = 0.9266, K = 1000000, U = 0.96, L = 0.92,
  sigma = 0.06, r = 0.0025, b = -0.0250, price = 4){
    f <- function(Time)
        dnt1(S, K, U, L, sigma, Time, r, b) - price
    uniroot(f, interval = c(1/365, 100))$root
}
library(rootSolve)

implied_DNT_image()
print(implied_vol_DNT(price = 6))
print(implied_U_DNT(price = 4))
print(implied_T_DNT(price = 30))
