setwd(dir = "~/Dokumente/Master/3. Fachsemester/statistische Verfahren/WS 17⁄18/projekt/crimes/")
crimes.data <- read.csv("crimes.csv")
head(crimes.data)
plot(crimes.data$crimes~crimes.data$prbarr)
plot(crimes.data$crimes, crimes.data$prbarr)

cpa <- crimes.data$crimes/crimes.data$area # crimes per area

m0 <- glm(crimes~1+prbarr, data = crimes.data)
plot(m0)

mAll <- glm(crimes~1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir, data = crimes.data)
plot(mAll)
step(mAll, ~1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir)

mAll2 <- glm((crimes~(1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir)^2), data = crimes.data)
plot(mAll2)
step(mAll2, ~(1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir)^2)

anova(m0, mAll, test = "LRT")
AIC(m0, mAll)

m1 <- glm(crimes~1+prbarr:prbpris, data = crimes.data)
plot(m1)
AIC(m0, mAll, m1)

mAll2 <- glm(crimes~(1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir)^2, data = crimes.data)
add1(m1, scope = ~(1+prbarr+prbpris+polpc+density+area+taxpc)^2)

m2 <- glm(crimes~(1+prbarr+prbpris)^2, data = crimes.data)
add1(m2, prbarr)
AIC(m0, mAll, mAll2, m1, m2)
BIC(m0, mAll, mAll2, m1, m2)
?AIC

mTrade <- glm(crimes~(1+wsta+wser+wtrd+wfir)^2, data = crimes.data)
AIC(m0, mAll, mAll2, m1, m2, mTrade)
plot(mTrade, which = 1)

plot(crimes.data$crimes, crimes.data$prbarr)
plot(crimes.data$crimes, crimes.data$prbpris)

mArrest <- glm(crimes~(1+prbarr), data = crimes.data)
plot(mArrest, which = 1)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest)
add1(mArrest, scope = ~(1+crimes.data$prbarr))

maxY <- max(crimes.data$crimes)
maxY
normY <- crimes.data$crimes/maxY
normY
plot(normY, crimes.data$prbarr)
mArrestNorm <- glm(normY~(1+prbarr), data = crimes.data)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm)

mArea <- glm(crimes~1+area, data = crimes.data)
plot(mArea, which = 1)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm, mArea)
plot(crimes.data$crimes, crimes.data$area)
# wtf: the smaller the area the more crimes?

plot(crimes.data$crimes, crimes.data$density)
mDensity <- glm(crimes~1+density, data = crimes.data)
plot(mDensity, which = 1)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm, mArea, mDensity)

m3 <- glm(crimes~1+area+density+area:density, data = crimes.data)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm, mArea, mDensity, m3)
# crimes~1+area+density+area:density => 1549
# cpa~1+area:density => 581
# cpa~1+area+density+area:density => 426. bis jetzt beste!
plot(m3, which = 1)
coef(m3)
?coef


m3Wcon <- glm(crimes~1+area+density+area:density+wcon, data = crimes.data)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm, mArea, mDensity, m3, m3Wcon)

m3Wsta <- glm(crimes~1+area+density+area:density+wsta, data = crimes.data)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm, mArea, mDensity, m3, m3Wsta)

m3Wser <- glm(crimes~1+area+density+area:density+wser, data = crimes.data)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm, mArea, mDensity, m3, m3Wser)
# klein wenig besser

m3Wtrd <- glm(crimes~1+area+density+area:density+wtrd, data = crimes.data)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm, mArea, mDensity, m3, m3Wtrd)

m3Wfir <- glm(crimes~1+area+density+area:density+wfir, data = crimes.data)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm, mArea, mDensity, m3, m3Wfir)

crimes.data.west <- crimes.data[crimes.data$region == "west", ]
crimes.data.central <- crimes.data[crimes.data$region == "central", ]
crimes.data.other <- crimes.data[crimes.data$region == "other", ]

plot(crimes.data$crimes, crimes.data$area)
plot(crimes.data$crimes, crimes.data$region)

m4 <- glm(normY~(1+prbarr+region), data = crimes.data)
AIC(m4)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm, mArea, mDensity, m3, m3Wfir, m4)

m4Wfir <- glm(normY~(1+prbarr+region+area+density+area:density+wfir), data = crimes.data)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm, mArea, mDensity, m3, m3Wfir, m4, m4Wfir)
anova(mDensity, m3, m3Wfir, m4, m4Wfir)

m4Wfir <- glm(normY~(1+prbarr+region+area+density+area:density+wfir), data = crimes.data, family = poisson)
m4Wfir2 <- glm(normY~(1+prbarr+region+area+density+area:density+wfir), data = crimes.data, family = binomial)
BIC(m4Wfir, m4Wfir2)

# single models #
mPrbarr <- glm(normY~(1+prbarr), data = crimes.data)
mPrbpris <- glm(normY~(1+prbpris), data = crimes.data)
mPolpc <- glm(normY~(1+prbpris), data = crimes.data)
mDensity <- glm(normY~(1+density), data = crimes.data)
mArea <- glm(normY~(1+area), data = crimes.data)
mTaxpc <- glm(normY~(1+taxpc), data = crimes.data)
mRegion <- glm(normY~(1+region), data = crimes.data)
mPctmin <- glm(normY~(1+pctmin), data = crimes.data)
mPctymale <- glm(normY~(1+pctymale), data = crimes.data)
mWcon <- glm(normY~(1+wcon), data = crimes.data)
mWsta <- glm(normY~(1+wsta), data = crimes.data)
mWser <- glm(normY~(1+wser), data = crimes.data)
mWtrd <- glm(normY~(1+wtrd), data = crimes.data)
mWfir <- glm(normY~(1+wfir), data = crimes.data)
allA <- AIC(mPrbarr, mPrbpris, mPolpc, mDensity, mArea, 
           mTaxpc, mRegion, mPctmin, mPctymale, mWcon, mWsta, mWser, mWtrd, mWfir)
allA
plot(allA$AIC)

allB <- BIC(mPrbarr, mPrbpris, mPolpc, mDensity, mArea, 
            mTaxpc, mRegion, mPctmin, mPctymale, mWcon, mWsta, mWser, mWtrd, mWfir)
allB
plot(allB$BIC)

mLohn <- glm(normY~(1+wcon+wsta+wser+wtrd+wfir), data = crimes.data, family = poisson(link="log"))
plot(mLohn, which = 1)
AIC(mLohn)
BIC(mLohn)
anova(mLohn)
step(mLohn, ~(1+wcon+wsta+wser+wtrd+wfir))

m5 <- glm(normY~(1+prbarr+region+area+density+area:density+wcon+wsta+wser+wtrd+wfir), data = crimes.data, family = binomial)
plot(m5, which = 1)
AIC(m5)
BIC(m5)
anova(m5)
summary(m5)

m5T <- glm(normY~(1+prbarr+region+area+density+wcon+wsta+wser+wtrd+wfir), data = crimes.data, family = binomial)
AIC(m5T)
BIC(m5T)
anova(m5T)
summary(m5T)
coef(m5T)




### cross validation ###

lm5 <- lm(normY~(1+prbarr+region+area+density+wcon+wsta+wser+wtrd+wfir), data = crimes.data)
lm4 <- lm(normY~(1+prbarr+region), data = crimes.data)
lm3 <- lm(crimes~1+area+density+area:density, data = crimes.data)
lm2 <- lm(crimes~(1+prbarr+prbpris)^2, data = crimes.data)
lm1 <- lm(crimes~1+prbarr:prbpris, data = crimes.data)

cross_validation <- function(repeats = 10) {
  index <- rep(1:7, each = 6)
  index <- index[-(dim(crimes.data)[1]+1)]
  index <- sample(index)
  
  SPSE1 <- SPSE2 <- SPSE3 <- SPSE4 <- SPSE5 <- 0
  
  for(i in 1:repeats) {
    crimes.test <- crimes.data[index==i,]
    crimes.train <- crimes.data[-index!=i,]
    
    # schätzung der parameter
    lm5 <- lm(normY~(1+prbarr+region+area+density+wcon+wsta+wser+wtrd+wfir), data = crimes.data)
    lm4 <- lm(normY~(1+prbarr+region), data = crimes.data)
    lm3 <- lm(crimes~1+area+density+area:density, data = crimes.data)
    lm2 <- lm(crimes~(1+prbarr+prbpris)^2, data = crimes.data)
    lm1 <- lm(crimes~1+prbarr:prbpris, data = crimes.data)
    
    # schätzung des prognosefehlers
    SPSE1 <- SPSE1 + sum((crimes.test$crimes - predict(lm1, newdata=crimes.test))^2)
    SPSE2 <- SPSE2 + sum((crimes.test$crimes - predict(lm2, newdata=crimes.test))^2)
    SPSE3 <- SPSE3 + sum((crimes.test$crimes - predict(lm3, newdata=crimes.test))^2)
    SPSE4 <- SPSE4 + sum((crimes.test$crimes - predict(lm4, newdata=crimes.test))^2)
    SPSE5 <- SPSE5 + sum((crimes.test$crimes - predict(lm5, newdata=crimes.test))^2)
  }
  
  cbind(SPSE1, SPSE2, SPSE3, SPSE4, SPSE5, min(SPSE1, SPSE2, SPSE3, SPSE4, SPSE5))
}


### using step ###
step(m5T,scope = ~(1+prbarr+region+area+density+wcon+wsta+wser+wtrd+wfir)^2)
m5opt <- glm(formula = normY ~ density, family = binomial, data = crimes.data)
AIC(m5opt)
plot(m5opt, which = 1)
plot(m5T, which = 1)

mAll2 <- glm((crimes~(1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir)^2), data = crimes.data)
plot(mAll2)
step(mAll2, ~(1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir)^2)

mStep <- glm(formula = crimes ~ prbarr + prbpris + polpc + density + area + 
               taxpc + region + pctmin + pctymale + wcon + wsta + wser + 
               wtrd + wfir + prbarr:prbpris + prbarr:polpc + prbarr:density + 
               prbarr:area + prbarr:taxpc + prbarr:region + prbarr:pctmin + 
               prbarr:pctymale + prbarr:wcon + prbarr:wsta + prbarr:wser + 
               prbarr:wtrd + prbarr:wfir + prbpris:polpc + prbpris:density + 
               prbpris:area + prbpris:taxpc + prbpris:region + prbpris:pctmin + 
               prbpris:pctymale + prbpris:wcon + prbpris:wsta + prbpris:wser + 
               prbpris:wtrd + prbpris:wfir + polpc:density + polpc:area + 
               polpc:taxpc + polpc:region + polpc:pctmin + polpc:pctymale + 
               polpc:wcon + polpc:wsta + polpc:wser + polpc:wtrd + polpc:wfir + 
               density:area + density:taxpc + density:region + density:pctmin + 
               density:pctymale + density:wcon + density:wsta + density:wser + 
               density:wtrd + density:wfir + area:taxpc + area:region + 
               area:pctmin + area:pctymale + area:wcon + area:wsta + area:wser + 
               area:wtrd + area:wfir + taxpc:region + taxpc:pctmin + taxpc:pctymale + 
               taxpc:wcon + taxpc:wsta + taxpc:wser + taxpc:wtrd + region:pctmin + 
               region:pctymale + region:wcon, data = crimes.data)
plot(mStep, which = 1)


### Pseudos ###
pseudo.data <- rbinom(90, seq(0, 90), pi/10)
pseudo.data
plot(pseudo.data)
plot(crimes~density, data = crimes.data, col = region)
points(crimes.data$crimes, pseudo.data, col = 14)

#mPseudo5 <- glm(normY ~ density, data = pseudo.data)
