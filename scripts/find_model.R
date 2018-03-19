#setwd(dir = "~/Dokumente/Master/3. Fachsemester/statistische Verfahren/WS 17⁄18/projekt/crimes/")
crimes.data <- read.csv("crimes.csv")
head(crimes.data)
plot(crimes.data$crimes~crimes.data$prbarr)
plot(crimes.data$crimes, crimes.data$prbarr)



#---------------------------------------------------------------------------------##
##----------------------- 1. ansatz: ausprobieren -------------------------------##

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

#maxY <- max(crimes.data$crimes)
#maxY
#normY <- crimes.data$crimes/maxY
#normY
#plot(normY, crimes.data$prbarr)
#mArrestNorm <- glm(normY~(1+prbarr), data = crimes.data)
#AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm)

mArea <- glm(crimes~1+area, data = crimes.data)
plot(mArea, which = 1)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm, mArea)
plot(crimes.data$crimes, crimes.data$area)

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
anova(m3)

## bestes modell, wenn ein lohn-eingabevektor hinzugenommen wird:
m3Wser <- glm(crimes~1+area+density+area:density+wser, data = crimes.data)
AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArrestNorm, mArea, mDensity, m3, m3Wser)
step(m3Wser, ~(1+area+density+wser)^2)

m3Opt <- glm(formula = crimes ~ area + density + wser + area:density + 
               density:wser, data = crimes.data)
AIC(m3Opt)
cv(m3Opt); cv(m3)

m3Opt <- glm(formula = crimes ~ area + density + wser + area:density + 
               density:wser, data = crimes.data)

m3Opt2 <- glm(formula = crimes ~ area + density + wser + area:density + 
                density:wser + prbarr + prbpris, data = crimes.data)
cv(m3Opt2)
cv(m3Opt)
step(m3Opt2, ~(1 + area + density + wser + prbarr + prbpris)^2)
m3O2 <- glm(formula = crimes ~ area + density + wser + prbarr + area:density + 
            density:wser + density:prbarr + wser:prbarr + area:prbarr, 
            data = crimes.data)
AIC(m3O2, m3Opt2, m3Opt)
cv(m3O2); cv(m3Opt2); cv(m3Opt)
anova(m3O2, m3Opt2, m3Opt)

m3O2_t <- glm(formula = crimes ~ area + density + wser + prbarr + area:density + 
              density:wser + density:prbarr + wser:prbarr + area:prbarr
              data = crimes.data)
cv(m3O2); cv(m3Opt2); cv(m3Opt); cv(m3O2_p)

step(m3O2_p, ~(1+area+density+wser+prbarr+taxpc)^2)
mT <- glm(formula = crimes ~ area + density + wser + prbarr + taxpc + 
               area:density + density:wser + density:prbarr + wser:prbarr + 
               area:prbarr + prbarr:taxpc, data = crimes.data)
cv(m3O2); cv(m3Opt2); cv(m3Opt); cv(mT)
AIC(m3O2, m3Opt2, m3Opt, mTest)

step(m3O2_p, ~(1+area+density+wser+prbarr+region)^2)
mR <- glm(formula = crimes ~ area + density + wser + prbarr + region + 
            area:density + density:wser + density:prbarr + wser:prbarr + 
            density:region + wser:region, data = crimes.data)
cv(m3O2); cv(m3Opt2); cv(m3Opt); cv(mT); cv(mR)
AIC(m3O2, m3Opt2, m3Opt, mT, mR)

#---------------------------------------------------------------------------------#
### frage:
### AIC eignet sich nicht gut als absolutes maß zur beurteilung der güte eines modells.
### kann ich die cv dafür benutzen?
### m3Wser recht gut - 
## vergleiche m3 mit einflussgröße crimes / cpa mittels cv
m3WserCPA <- glm(cpa~1+area+density+area:density+wser, data = crimes.data)
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

AIC(m0, mAll, mAll2, m1, m2, mTrade, mArrest, mArea, mDensity, 
    m3, m3Wser, m3Opt, m3Opt2, m3O2, mT, mR)

# vergleiche die empirisch gefundenen modelle mittels kreuzvalidierung:
cvE <- cbind(cv(m0), cv(mAll), cv(m1), cv(m2), cv(mTrade), cv(mArrest),
             cv(mArea), cv(mDensity),
             cv(m3), cv(m3Wser), cv(m3Opt), cv(m3Opt2), cv(m3O2), 
             cv(m3O2_p), cv(mT), cv(mR))
cvC <- cvE[1,]/max(cvE[1,])
plot(cvC)
cvB7 <- cbind(cv(m3Wser), cv(m3Opt), cv(m3Opt2), cv(m3O2), 
              cv(m3O2_p), cv(mT), cv(mR))
cvB7r<-cvB7[1,]/max(cvB7[1,]) 
cvB7r
plot(cvB7r)

########## beliebtestes modell in diesem ansatz: mR


