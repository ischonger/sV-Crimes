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

installed.packages("bestglm")
library("bestglm")

m4Wfir <- glm(normY~(1+prbarr+region+area+density+area:density+wfir), data = crimes.data, family = poisson)

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
AIC(mPrbarr, mPrbpris, mPolpc, mDensity, mArea, 
    mTaxpc, mRegion, mPctmin, mPctymale, mWcon, mWsta, mWser, mWtrd, mWfir)

