### Pseudos ###
#mPseudo5 <- glm(normY ~ density, data = pseudo.data)
require(MASS)
pseudo.data <- rnegbin(90, mu = predict(mStep, type = "response"),
                       theta = 0.7566)
head(pseudo.data)

plot(crimes~(1+density), data = crimes.data, col = region, pch = 16)
points(crimes.data$crimes, pseudo.data, col = 9)

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


crimes.data.central <- crimes.data[crimes.data$region == "central", ]
plot(crimes.data.central$crimes)

crimes.data.other <- crimes.data[crimes.data$region == "other", ]
plot(crimes.data.other$crimes)

