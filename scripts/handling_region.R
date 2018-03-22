mRegion <- glm.nb(crimes~region, data = crimes.data)
cv(mRegion)

# west
crimes.data.west <- crimes.data[crimes.data$region == "west", ]
plot(crimes.data.west$crimes)
plot(crimes~(1+density), data = crimes.data.west, col = region, pch = 16)
pseudo.data <- rnegbin(dim(crimes.data.west)[1], mu = predict(mStep, type = "response"),
                       theta = 0.9)
points(crimes.data.west$crimes, pseudo.data, col = 15, pch = 16)
# 0.6 sieht ganz gut aus

pseudoWest.data <- rnegbin(21, mu = predict(mStep, type = "response"),
                           theta = 0.7566)
points(crimes.data.west$crimes, pseudoWest.data, col = 10)


crimes.data.west <- crimes.data[crimes.data$region == "west"]
plot(crimes.data.west$crimes, col = 1)
dim(crimes.data.west) # 21
mean(crimes.data.west$crimes) #1027.381
median(crimes.data.west$crimes) #513
mean(crimes.data.west$density) # 86
median(crimes.data.west$density) # 78

crimes.data.central <- crimes.data[crimes.data$region == "central", ]
points(crimes.data.central$crimes, col = 2)
dim(crimes.data.central) # 34
mean(crimes.data.central$crimes) #4764.882
median(crimes.data.central$crimes) #2172
mean(crimes.data.central$density) # 196
median(crimes.data.central$density) # 143


crimes.data.other <- crimes.data[crimes.data$region == "other", ]
points(crimes.data.other$crimes, col = 3)
dim(crimes.data.other) # 35
mean(crimes.data.other$crimes) #2250.514
median(crimes.data.other$crimes) #1235
mean(crimes.data.other$density) # 101
median(crimes.data.other$density) # 65


crimes.data.noWest <- rbind(crimes.data.central, crimes.data.other)
plot(crimes.data.noWest$crimes, col = 2)
points(pseudo.data2)
points(pseudo.data1, col = "green")
points(pseudo.data, col = "blue")

plot(crimes.data$crimes, crimes.data$density)
crimes.noOutliers <- crimes.data[crimes.data$crimes <= 1000,]
crimes.noOutliers
plot(crimes.noOutliers$crimes, crimes.noOutliers$density)
cor(crimes.data$crimes, as.factor(crimes.data$region))

crimes.matrix <- matrix(unlist(crimes.rl), ncol = 16, byrow = FALSE)
cov(crimes.matrix[,1], crimes.matrix[,7])

mR <- glm.nb(crimes~region+density, data = crimes.data)
mRd <- glm.nb(crimes~region+density+region:density, data = crimes.data)
cv(mR); cv(mDensity); cv(mRd)
# 3003165717
# 3003072715
# 3003120609
AIC(mDensity, mR, mRd)
#          df      AIC
# mDensity  3 1494.419
# mR       16 1405.772
# mRd       7 1473.191

mDensity <- glm.nb(crimes~density, data = crimes.data.central)
plot(crimes.data.central$crimes, crimes.data.central$density)
abline(mDensity$coefficients[1], mDensity$coefficients[2])
cv(mDensity) # 3003195285

mDensity <- glm.nb(crimes~density, data = crimes.data.west)
plot(crimes.data.west$crimes, crimes.data.west$density)
abline(mDensity$coefficients[1], mDensity$coefficients[2])
cv(mDensity) # 3001824612

mDensity <- glm.nb(crimes~density, data = crimes.data.other)
plot(crimes.data.other$crimes, crimes.data.other$density)
abline(mDensity$coefficients[1], mDensity$coefficients[2])
cv(mDensity) # 3002760812


coefficients(mRd)
test(model = mRd, seed = 541543213, theta = 0.6)
test(model = mRd, seed = 685135, theta = 2.802896879)
