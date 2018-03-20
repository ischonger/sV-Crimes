### Pseudos ###
#mPseudo5 <- glm(normY ~ density, data = pseudo.data)
require(MASS)
pseudo.data1 <- rnegbin(90, mu = predict(mStep, type = "response"),
                       theta = 0.7566)

pseudo.data2 <- rpois(90, lambda = predict(mStepO, type = "response"))
head(pseudo.data)
pseudo.data2
plot(pseudo.data2)

plot(crimes~(1+density), data = crimes.data, col = region, pch = 16)
points(crimes.data$crimes, pseudo.data, col = 9)
points(crimes.data$crimes)
points(pseudo.data2, col = 3)

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

crimes.data.noWest <- rbind(crimes.data.central, crimes.data.other)
plot(crimes.data.noWest$crimes, col = 2)
points(pseudo.data2)
points(pseudo.data1, col = "green")
points(pseudo.data, col = "blue")

## gegenüberstellung:
# daten ohne ausreißer
plot(crimes.data.noWest$crimes)
# inkl pseudos
points(pseudo.data1, col = "green")

# daten mit ausreißern
plot(crimes.data$crimes, col = "blue")
points(pseudo.data1, col = "red")


### wähle randomisiert 30 elemente aus crimes.data$crimes aus 
### und vergleiche diese mit 30 randomisierten zufallszahlen von rpois(), wenn theta = -1
### oder rnegbin(), wenn theta >= 0
test <- function(seed = 1234, amount = 30, model = mStepO, theta = -1) {
  set.seed(seed)
  
  if(theta == -1) {
    sP <- rpois(amount, lambda = predict(model, type = "response"))
  } else {
    sP <- rnegbin(amount, mu = predict(mStep, type = "response"),
                  theta = theta)
  }
  sP <- sP[!is.na(sP)]
  sC <- sample(crimes.data$crimes, length(sP))
    
  plot(sC)
  points(sP, col = "blue")
  i <- 3
  for(c in m3O2$coefficients[2:length(m3O2$coefficients)]) {
    abline(model$coefficients[[1]], c, col = i)
    i <- i+1
  }
  #abline(model$coefficients[[1]], model$coefficients[[2]], col = "red", pch = 16)
  
  dist <- sC-sP
  dist
  print(median(dist))
  
  tm <- cbind(rep(1,amount), sP)
  am <- cbind(rep(1,amount), sC)
  c  <- cov(tm, am)
  print(c)
  
  #"deviance: "
  #deviance(model)
  
  print(cor(tm, am))
}
test(seed = 6153134)
test(seed = 2165464, model = m3O2)
test(seed = 6548, model = m3O2)
test(seed = 6548, model = mStepO)
test(seed = 6546512, model = m3O2)
test(seed = 6546512, model = mStepO)
test(seed = 6546512, model = m3O2, theta = -1)
test(seed = 6546512, model = mStepO, theta = 0.6289)
# -114.0778
test(seed = 6546512, model = m3O2, theta = 0.6289)

mDensity <- glm(crimes~density, data = crimes.data)
test(seed = 6546512, model = mDensity, theta = 0.54)
test(seed = 6546512, model = mDensity, theta = -1)

test(model = mDensity)
deviance(m3O2)
deviance(mDensity)
