### Pseudos ###
require(MASS)
library(nlmeODE)
require(nlmeODE)
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
test <- function(seed = 1234, amount = 30, model = m1, theta = -1) {
  set.seed(seed)
  
  # decide whether to use rnegbin or rpois distribution for computing pseudo-values (sP)
  # and get random values from crimes.data$crimes
  if(theta == -1) {
    sP <- rpois(amount, lambda = predict(model, type = "response"))
  } else {
    sP <- rnegbin(amount, mu = predict(model, type = "response"),
                  theta = 2.103304731)
  }
  sP <- sP[!is.na(sP)]
  #sC <- sample(crimes.data$crimes, length(sP))
  sC <- crimes.data[sample(1:90, amount), ]
  
  # plot real data from crimes.data$crimes in black and pseudos in blue
  # and draw linear function per coefficients 
  plot(sC$crimes)
  points(sP, col = "blue")
  i <- 3
  for(c in model$coefficients[2:length(model$coefficients)]) {
    abline(model$coefficients[[1]], c, col = i)
    i <- i+1
  }
  
  # print median distance values between the real and the estimated value
  dist <- sC-sP
  dist
  print(abs(median(dist)))
  
  
  # print correlation coefficient
  print(cor(tm, am))
  
  # compute covariance matrix
  sC.matrix <- matrix(unlist(sC), ncol = 16, byrow = FALSE) # sC as a matrix
  sC.tcov <- cov(sC.matrix)
  sC.acov <- vcov(mDensity)
}

x <- cbind(1, crimes.data$crimes)
head(x)

beta.hat <- solve(t(x) %*% x) %*% t(x) %*% crimes.data$crimes
beta.hat

plot(crimes.data$crimes)
abline(beta.hat)

x <- crimes.data$density
y <- crimes.data$crimes
plot(x,y)
abline(beta.hat, col = 2, lwd = 3)
abline(c(mDensity$coefficients[1], mDensity$coefficients[2]), col = 3, lwd = 3)

## deviance:
# zitat faustregel:
# ein modell M ist ein geeignetes modell, falls die Devienz von M so groß ist, wie die ugf
# anzahl der parameter von M
dt <- function(model) {
  deviance(model) - abs(model$rank)
}
dt(m1); dt(m2); dt(m3); dt(m4); dt(m5)

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


mDensity <- glm.nb(crimes~density, data = crimes.data)

simulation <- function(model = mDensity, maxAmount = 30, repeats = 20) {
  # wähle stichprobenumfang
  c <- crimes.data[sample(1:amount), ]
  # baue design-matrix
  c.dm <- data.frame(cbind(c$crimes, c$density))
  colnames(c.dm) <- c("crimes", "density")
  
  # simuliere mehrfache pseudobeobachtungen
  betas <- matrix(ncol = 2, nrow = repeats); colnames(betas) <- c("beta0.hat", "beta1.hat")
  for(i in 1:repeats) {
    c.tdm <- c.dm[sample(1:dim(c.dm)[1],sample(5:dim(c.dm)[1])), ] # test design-matrix
    m <- glm.nb(crimes~density, data = c.tdm)
    betas[i,] <- m$coefficients
    p <- rnegbin(amount, mu = predict(m, type = "response"), theta = m$call$init.theta)
  }
  var(betas)
  cov(betas)
}
