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
  
  # decide whether to use rnegbin or rpois distribution for computing pseudo-values (sP)
  # and get random values from crimes.data$crimes
  if(theta == -1) {
    sP <- rpois(amount, lambda = predict(model, type = "response"))
  } else {
    sP <- rnegbin(amount, mu = predict(model, type = "response"),
                  theta = theta)
  }
  sP <- sP[!is.na(sP)]
  #sC <- sample(crimes.data$crimes, length(sP))
  sC <- crimes.data[sample(1:90, amount), ]
  
  # plot real data from crimes.data$crimes in black and pseudos in blue
  # and draw linear function per coefficients 
  plot(sC$crimes)
  points(sP, col = "blue")
  i <- 3
  for(c in m3O2$coefficients[2:length(m3O2$coefficients)]) {
    abline(model$coefficients[[1]], c, col = i)
    i <- i+1
  }
  
  # print median distance values between the real and the estimated value
  dist <- sC-sP
  dist
  print(abs(median(dist)))
  
  ### ???? brauch ich das wirklich?, das geht doch nur für modelle mit einem eingabevektor
  tm <- cbind(rep(1,amount), sP)
  am <- cbind(rep(1,amount), sC)
  c  <- cov(tm, am)
  c <- cov(sP, sC)
  print(c)
  
  # print correlation coefficient
  print(cor(tm, am))
  
  # compute covariance matrix
  #acm <- vcov(model)
  mv <- c()
  sC.matrix <- matrix(unlist(sC), ncol = 16, byrow = FALSE)
  for(col in sC) {
    append(mv, mean(col))
  
  
  sC_mean <- matrix(data=1, nrow=n) %*% cbind(mean(a),mean(b),mean(c),mean(d),mean(e)) 
}

x <- cbind(1, crimes.data$crimes)
head(x)

beta.hat <- solve(t(x) %*% x) %*% t(x) %*% crimes.data$crimes
beta.hat

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