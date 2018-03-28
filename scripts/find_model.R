#setwd(dir = "~/Dokumente/Master/3. Fachsemester/statistische Verfahren/WS 17⁄18/projekt/crimes/")
crimes.data <- read.csv("crimes.csv")
head(crimes.data)
plot(crimes.data$crimes~crimes.data$prbarr)
plot(crimes.data$crimes, crimes.data$prbarr)



#---------------------------------------------------------------------------------##
##----------------------- 1. ansatz: ausprobieren -------------------------------##

cpa <- crimes.data$crimes/crimes.data$area # crimes per area

m0 <- glm.nb(crimes~1+prbarr, data = crimes.data)
plot(m0)

mAll <- glm.nb(crimes~1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir, data = crimes.data)
plot(mAll)
step(mAll, ~1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir)

mAllP <- glm.nb(crimes~1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir, 
            data = crimes.data)


mAll2 <- glm((crimes~(1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir)^2), data = crimes.data)
plot(mAll2)
step(mAll2, ~(1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir)^2)

mAll2P <- glm.nb((crimes~(1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir)^2), 
              data = crimes.data)
plot(mAll2, which = 1)
step(mAll2, ~(1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir)^2)


anova(m0, mAll, test = "LRT")
AIC(m0, mAll, mAllP)
cv(m0); cv(mAll); cv(mAllP)
# family = poisson verschlechtert das ergebnis ... und zwar erheblich
# !!! nicht family = poisson benützen!
# sonder glm.nb()!!!!

m1 <- glm(crimes~1+prbarr:prbpris, data = crimes.data)
plot(m1)
AIC(m0, mAll, m1)

m1Nb <- glm.nb(crimes~1+prbarr:prbpris, data = crimes.data)
plot(m1Nb)
AIC(m0, mAll, m1Nb)

AIC(m1, m1Nb)

mAll2 <- glm.nb(crimes~(1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir)^2, data = crimes.data)
add1(m1, scope = ~(1+prbarr+prbpris+polpc+density+area+taxpc)^2)


m2 <- glm.nb(crimes~(1+prbarr+prbpris)^2, data = crimes.data)
add1(m2, prbarr)
AIC(mAll, m1, m2)
BIC(m0, mAll, m1, m2)
?AIC

AIC(m0, mAll, m1Nb, m2)

mSpatial1 <- glm.nb(crimes~(density+area), data = crimes.data)
mSpatial2 <- glm.nb(crimes~(density+area)^2, data = crimes.data)
mSpatial3 <- glm.nb(crimes~(density+area+region), data = crimes.data)
mSpatial4 <- glm.nb(crimes~(density+area+region)^2, data = crimes.data)
AIC(mSpatial1, mSpatial2, mSpatial3, mSpatial4)
cv(mSpatial1); cv(mSpatial2); cv(mSpatial3); cv(mSpatial4)

mTrade <- glm.nb(crimes~(1+wsta+wser+wtrd+wfir)^2, data = crimes.data)
AIC(m0, mAll, mAll2, m1, m1Nb, m2, m2Nb, mTrade)
cv(m0); cv(mAll); cv(m1); cv(m1Nb); cv(m2); cv(m2Nb); cv(mTrade)
plot(mTrade, which = 1)

plot(crimes.data$crimes, crimes.data$prbarr)
plot(crimes.data$crimes, crimes.data$prbpris)

mArrest <- glm.nb(crimes~(1+prbarr), data = crimes.data)
plot(mArrest, which = 1)
AIC(m0, mAll, m1, m1Nb, m2, m2Nb, mTrade, mArrest)
cv(m0); cv(mAll); cv(m1); cv(m1Nb); cv(m2); cv(m2Nb); cv(mTrade)
add1(mArrest, scope = ~(1+crimes.data$prbarr))

#maxY <- max(crimes.data$crimes)
#maxY
#normY <- crimes.data$crimes/maxY
#normY
#plot(normY, crimes.data$prbarr)
#mArrestNorm <- glm.nb(normY~(1+prbarr), data = crimes.data)
#AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm)

mArea <- glm.nb(crimes~1+area, data = crimes.data)
plot(mArea, which = 1)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArea)
cv(m0); cv(mAll); cv(m1); cv(m1Nb); cv(m2); cv(m2Nb); cv(mTrade); cv(mArrest); cv(mArea)
plot(crimes.data$crimes, crimes.data$area)

plot(crimes.data$crimes, crimes.data$density)
mDensity <- glm.nb(crimes~1+density, data = crimes.data)
plot(mDensity, which = 1)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm, mArea, mDensity)

m3 <- glm.nb(crimes~1+area+density+area:density, data = crimes.data)
AIC(m0, mAll, m1,m1Nb, m2, m2Nb, mTrade, mArrest, mArea, mDensity, m3)
cv(m0); cv(mAll); cv(m1); cv(m1Nb); cv(m2); cv(m2Nb); cv(mTrade); cv(mArrest); cv(mArea); cv(mDensity); cv(m3)
# crimes~1+area+density+area:density => 1549
# cpa~1+area:density => 581
# cpa~1+area+density+area:density => 426. bis jetzt beste!
plot(m3, which = 1)
coef(m3)
anova(m3)

## bestes modell, wenn ein lohn-eingabevektor hinzugenommen wird:
m3Wser <- glm.nb(crimes~1+area+density+area:density+wser, data = crimes.data)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArea, mDensity, m3, m3Wser)
cv(m0); cv(mAll); cv(m1); cv(m1Nb); cv(m2); cv(m2Nb); cv(mTrade); cv(mArrest); cv(mArea); cv(mDensity); cv(m3); cv(m3Wser)
step(m3Wser, ~(1+area+density+wser)^2)

m3Opt <- glm.nb(formula = crimes ~ area + density + wser + area:density + 
               density:wser, data = crimes.data)
AIC(m3Opt)
cv(m3Opt); cv(m3)

m3Opt <- glm.nb(formula = crimes ~ area + density + wser + area:density + 
               density:wser, data = crimes.data)

m3Opt2 <- glm.nb(formula = crimes ~ area + density + wser + area:density + 
                density:wser + prbarr + prbpris, data = crimes.data)
cv(m3Opt2)
cv(m3Opt)
step(m3Opt, ~(1 + area + density + wser + prbarr + prbpris)^2)
m3O2 <- glm.nb(formula = crimes ~ area + density + wser + prbarr + area:density + 
                 density:wser + density:prbarr + wser:prbarr + area:wser, 
               data = crimes.data, init.theta = 5.146530532, link = log)
AIC(m3O2, m3Opt2, m3Opt)
cv(m3O2); cv(m3Opt2); cv(m3Opt)
anova(m3O2, m3Opt2, m3Opt)

m3O2_p <- glm.nb(formula = crimes ~ area + density + wser + prbarr + area:density + 
              density:wser + density:prbarr + wser:prbarr + area:prbarr,
              data = crimes.data)
cv(m3O2); cv(m3Opt2); cv(m3Opt); cv(m3O2_p)
AIC(m3O2, m3Opt2, m3Opt, m3O)

step(m3O2_p, ~(1+area+density+wser+prbarr+taxpc)^2)
mT <- glm.nb(formula = crimes ~ area + density + wser + prbarr + taxpc + 
               area:density + density:wser + density:prbarr + wser:prbarr + 
               area:prbarr + prbarr:taxpc, data = crimes.data)
cv(m3O2); cv(m3Opt2); cv(m3Opt); cv(mT)
AIC(m3O2, m3Opt2, m3Opt, mT)

step(m3O2_p, ~(1+area+density+wser+prbarr+region)^2)
mR <- glm.nb(formula = crimes ~ area + density + wser + prbarr + region + 
               area:density + density:wser + density:prbarr + wser:prbarr + 
               area:prbarr + density:region + area:wser, data = crimes.data, 
             init.theta = 6.810512192, link = log)
cv(m3O2); cv(m3Opt2); cv(m3Opt); cv(mT); cv(mR)
AIC(m3O2, m3Opt2, m3Opt, mT, mR)

#---------------------------------------------------------------------------------#
### frage:
### AIC eignet sich nicht gut als absolutes maß zur beurteilung der güte eines modells.
### kann ich die cv dafür benutzen?
### m3Wser recht gut - 
## vergleiche m3 mit einflussgröße crimes / cpa mittels cv
m3WserCPA <- glm.nb(cpa~1+area+density+area:density+wser, data = crimes.data)
AIC(m3WserCPA)
cvM3 <- cbind(cv(m3Wser), cv(m3WserCPA))
cvM3[1,]
# der AIC-wert von cpa-basierten werten war immer recht klein.
# im vergleich mit cv sind solche modelle aber wesentlich schlechter.
cvTest <- cbind(cv(mAll2), cv(m1))
cvTest[1,]
# ein sehr genaues modell hat einen wesentlich kleineren fehler als
# ein sehr ungenaues. ==> passt!
plot(crimes.data$crimes, crimes.data$area)
plot(crimes.data$crimes, crimes.data$region)

aics <- AIC(m0, mAll, m1, m2, mTrade, mArrest, mArea, mDensity, 
            m3, m3Wser, m3Opt, m3Opt2, m3O2, mT, mR)
plot(aics$AIC)
aics$AIC
min(aics$AIC)

# vergleiche die empirisch gefundenen modelle mittels kreuzvalidierung:
cvE <- cbind(cv(m0), cv(mAll), cv(m1), cv(m2), cv(mTrade), cv(mArrest),
             cv(mArea), cv(mDensity),
             cv(m3), cv(m3Wser), cv(m3Opt), cv(m3Opt2), cv(m3O2), 
             cv(mT), cv(mR))
cvC <- cvE[1,]/max(cvE[1,])
plot(cvC)
cvB7 <- cbind(cv(m3Wser), cv(m3Opt), cv(m3Opt2), cv(m3O2), 
              cv(m3O2_p), cv(mT), cv(mR))
cvB7r<-cvB7[1,]/max(cvB7[1,]) 
cvB7r
plot(cvB7r)
### kann man cv hier trauen?


########## beliebtestes modell in diesem ansatz nach akaike: mR