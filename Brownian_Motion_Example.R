bm <- cumsum(rnorm(1000,0,1))
 bm <- bm - bm[1]
windows()
plot(bm, main = "Brownian Motion", col = "blue", type = "l")

windows()
acf(diff(bm), main = "Autocorrelation of Wt")

windows()
par(mfrow = c(2,1))
hist(diff(bm), col = "orange", breaks = 100, main = "Wt-s Distribution")
windows()
qqnorm(diff(bm))
qqline(diff(bm))