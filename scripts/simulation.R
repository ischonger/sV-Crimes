### Pseudos ###
require(MASS)
library(nlmeODE)
require(nlmeODE)
library(numDeriv)
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
# y = 6.38743*density+ 0.00756

simulation <- function(model = mDensity, amount = 30, repeats = 20, seed = 26031409) {
  set.seed(seed)
  
  # wähle stichprobenumfang
  c <- crimes.data[sample(1:amount), ]
  # baue design-matrix
  c.dm <- data.frame(cbind(c$crimes, c$density))
  colnames(c.dm) <- c("crimes", "density")
  
  # simuliere mehrfache pseudobeobachtungen
  betas <- matrix(ncol = 2, nrow = repeats); colnames(betas) <- c("beta0.hat", "beta1.hat")
  for(i in 1:repeats) {
    c.tdm <- c.dm[sample(1:dim(c.dm)[1],sample(3:dim(c.dm)[1])), ] # test design-matrix
    m <- glm.nb(crimes~density, data = c.tdm)
    betas[i,] <- m$coefficients
    p <- rnegbin(dim(c.tdm)[1], mu = predict(m, type = "response"), theta = m$call$init.theta)
    plot(c.tdm$density, p)
    points(c.tdm$density, c.tdm$crimes, col = 2)
  }
  var(betas)
  c.cov <- cov(betas)
  
  # berechne asymptotische kovarianzmatrix
  ## berechne Designmatrix X: X = cbind(beta0, beta1)
  betas.dm  <- cbind(c.cov[,1], c.cov[,2])
  ## berechne fisher-informationsmatrix: I(beta) = X^t V X, X ist Designmatrix, V ist diag(Vars)
  betas.dvm <- c(var(betas)[1,1], var(betas)[2,2]) # diagonale varianz-matrix
  betas.I  <- t(betas.dm)%*%diag(betas.dvm)%*%betas.dm
  
  
  md <- abs(as.matrix(c.cov) - betas.I) # matrix deviation
  #print(md)
  return(cbind(md[1,1], md[2,2]))
}



# compare vergleicht zwei simulationen miteinander
# simulation() gibt den betrag des abstandes von 
# tatsächlicher kovarianzmatrix - asymptotischer kovarianzmatrix
# jeweils für beta0 und für beta1 aus
# compare vergleicht diese simulationen loops mal (default = 10)
# und gibt das verhältnis von beta0 aus erster und zweiter simulation
# sowie das verhältnis von beta1 aus erster und zweiter simulation aus
# ist eine der zahlen also größer als 1, so hat die erste simulation 
# die größeren abstandswerte.
# also ist die erste simulation besser,
# ist eine der zahlen kleiner als 1, so ist die zweite simulation besser
# credentials of simulation 1, credentials of simulation 2
compare <- function(loops = 10) { 
  rm <- matrix(ncol = 4, nrow = loops)
  for(i in 1:loops) {
    betas1 <- simulation(seed = sample(1:100000, 1), amount = 90)
    Sys.sleep(2)
    betas2 <- simulation(seed = sample(1:100000, 1), amount = 3, repeats = )
    rm[i,1] <- betas1[1]
    rm[i,2] <- betas1[2]
    rm[i,3] <- betas2[1]
    rm[i,4] <- betas2[2]
  }
  
  return(c(mean(rm[,3]) / mean(rm[,1]),  mean(rm[,4]) / mean(rm[,2])))
}

# 

# results:
compare(repeats1 = 1, amount1 = 10, repeats2 = 100, amount2 = 90)
# 0.09919219 0.03302115
# 1.292789 3.225211
# 1.271198 4.022766
# 
## => die werte aus der ersten simulation sind wesentlich größer als die aus der zweiten
## das entspricht den erwartungen -> wenn weniger wiederholungen angeboten werden, müsste 
## der relative größenunterschied zwischen den betas aus den unterschiedlichen cov-matrizen
## größer sein, da ja nicht so gut approximiert werden kann, wie wenn die anzahl an wiederholungen
## und die größe des testdatensatzes wesentlich kleiner ist.

compare(simulation(), simulation())
## hier wird kein wesentlicher unterschied der beta-verhältnis-werte erwartet
# 0.0002056119e-04 0.00004563244 ## #hashtag glück
# 0.0001837114     0.0001022125
# 0.0004178568     0.0001637638
rm     <- matrix(nrow = 10, ncol = 2)# result matrix
for(i in 1:10) {
  rm[i,] <- compare(simulation(), simulation())
}
b0m <- mean(rm[,1])# beta0 mean
b1m <- mean(rm[,2])# beta1 mean
b0m
b1m
