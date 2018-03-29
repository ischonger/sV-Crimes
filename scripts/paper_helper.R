#3.1
c <- crimes.data$crimes/max(crimes.data$crimes)
p <- rnegbin(90, theta = 0.4)
p <- p/max(p)
n <- rnorm(90)
plot(n)

par(mfrow=c(1,2))
plot(c, col = "red", pch = 16)
points(p, col = "blue", pch = 16)

plot(c, col = "red", pch = 16)
points(n, col = 3, pch = 16)

# 3.2
par(mfrow=c(1,1))
cd <- crimes.data.central$crimes/max(crimes.data$crimes)
wd <- crimes.data.central$crimes/max(crimes.data$crimes)
od <- crimes.data.central$crimes/max(crimes.data$crimes)
plot(crimes.data.central$crimes, pch = 16, col = 1, xlab = "counties", ylab = "crimes")
points(crimes.data.west$crimes, pch = 16, col = 2)
points(crimes.data.other$crimes, pch = 16, col = 3)

plot(crimes~(1+density), data = crimes.data, col = region, pch = 16)

# 3.3
par(mfrow=c(2,2))
plot(aics$AIC, pch = 16, col = "red", ylab = "AIC")
plot(cvC, pch = 16, ylab = "rel. SPSE")
plot(cvB7r, pch = 16, ylab = "rel. SPSE")
plot(cvD7[1,], pch = 16, col = 3, ylab = "Devienz")

# 3.4
par(mfrow=c(1,3))
plot(allA$AIC, pch = 16, ylab = "AIC", col = "red")
plot(allCV[1,], pch = 16, ylab = "rel. SPSE")
plot(allD[1,], pch = 16, ylab = "Devienz", col = "green")

# 3.5
par(mfrow=c(1,1))
plot(aics$AIC, pch = 16, xlab = "Modelle", ylab = "AIC", col = "red")
plot(cvC, pch =16, xlab = "Modelle", ylab = "rel. SPSE")

# 3.6
par(mfrow=c(1,3))
plot(aics$AIC, pch = 16, ylab = "AIC", col = "red")
plot(cvC[1,], pch = 16, ylab = "rel. SPSE" )
plot(cvD[1,], col = "green", pch = 16, ylab = "Devienz")
