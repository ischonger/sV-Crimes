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
