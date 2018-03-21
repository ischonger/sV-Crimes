### Pseudos ###
require(MASS)
# models from winners.R

pseudo.data1 <- rnegbin(90, mu = predict(m1, type = "response"),
                        theta = 0.7566)

pseudo.data2 <- rpois(90, lambda = predict(m1, type = "response"))
head(pseudo.data1)
pseudo.data2
plot(pseudo.data2)

plot(crimes~(1+density), data = crimes.data, col = region, pch = 16)
points(crimes.data$crimes, pseudo.data, col = 9)
points(crimes.data$crimes)
points(pseudo.data2, col = 3)



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
    sP <- rnegbin(amount, mu = predict(model, type = "response"),
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
  
  dist <- sC-sP
  dist
  print(median(dist))
  
  tm <- cbind(rep(1,amount), sP)
  am <- cbind(rep(1,amount), sC)
  c  <- cov(tm, am)
  print(c)
  
  print(cor(tm, am))
}
test(seed = 6153134) #-431
test(seed = 2165464, model = m2) #460 ## m2 aus winners.R!
test(seed = 6548, model = m2) #566.5
test(seed = 6548, model = m1) #648.5
test(seed = 6546512, model = m2) #-94.5
test(seed = 6546512, model = m1) #207
test(seed = 6546512, model = m2, theta = -1) #-94.5
test(seed = 6546512, model = m1, theta = 0.6289) #742
# 742
test(seed = 6546512, model = m2, theta = 0.6289)

mDensity <- glm.nb(crimes~density, data = crimes.data)
test(seed = 6546512, model = mDensity, theta = 0.54)
test(seed = 6546512, model = mDensity, theta = -1)

test(model = mDensity)
deviance(m3O2)
deviance(mDensity)

test(seed = 74158352, model = m2, theta = 3.416778004)
# immer das theta aus dem modell nehmen!
# gibt gute werte.