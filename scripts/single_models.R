#---------------------------------------------------------------------------------##
## 2. ansatz: betrachte AIC von allen einzelnen modellen und wähle die besten aus ##

mPrbarr <- glm.nb(crimes~(1+prbarr), data = crimes.data)
mPrbpris <- glm.nb(crimes~(1+prbpris), data = crimes.data)
mPolpc <- glm.nb(crimes~(1+prbpris), data = crimes.data)
mDensity <- glm.nb(crimes~(1+density), data = crimes.data)
mArea <- glm.nb(crimes~(1+area), data = crimes.data)
mTaxpc <- glm.nb(crimes~(1+taxpc), data = crimes.data)
mRegion <- glm.nb(crimes~(1+region), data = crimes.data)
mPctmin <- glm.nb(crimes~(1+pctmin), data = crimes.data)
mPctymale <- glm.nb(crimes~(1+pctymale), data = crimes.data)
mWcon <- glm.nb(crimes~(1+wcon), data = crimes.data)
mWsta <- glm.nb(crimes~(1+wsta), data = crimes.data)
mWser <- glm.nb(crimes~(1+wser), data = crimes.data)
mWtrd <- glm.nb(crimes~(1+wtrd), data = crimes.data)
mWfir <- glm.nb(crimes~(1+wfir), data = crimes.data)

# vergleiche modelle
allA <- AIC(mPrbarr, mPrbpris, mPolpc, mDensity, mArea, 
            mTaxpc, mRegion, mPctmin, mPctymale, mWcon, mWsta, mWser, mWtrd, mWfir)
allA
plot(allA$AIC)
allB <- BIC(mPrbarr, mPrbpris, mPolpc, mDensity, mArea, 
            mTaxpc, mRegion, mPctmin, mPctymale, mWcon, mWsta, mWser, mWtrd, mWfir)
allB
plot(allB$BIC)

allCV <- cbind(cv(mPrbarr), cv(mPrbpris), cv(mPolpc), cv(mDensity), cv(mArea), 
               cv(mTaxpc), cv(mRegion), cv(mPctmin), cv(mPctymale), cv(mWcon), cv(mWsta), 
               cv(mWser), cv(mWtrd), cv(mWfir))
allCV
plot(allCV[1,])

mSingles <- glm.nb(crimes~(1+density+wsta+wser+wtrd+wfir), data = crimes.data)
plot(mSingles, which = 1)
AIC(mSingles)
step(mSingles, ~(1+density+wsta+wser+wtrd+wfir)^2)

mSinglesOpt <- glm.nb(formula = crimes ~ density + wser + wtrd + wfir + wser:wtrd + 
                        density:wser, data = crimes.data, init.theta = 2.993441708, 
                      link = log)
plot(mSinglesOpt, which = 1)
cv(mSinglesOpt); cv(mR);
AIC(mSinglesOpt, m3O2, mR)
# aic-werte liegen wesentlich näher aneinander als cv-werte.

######### ermitteltes modell: mSinglesOpt
### (aber gar nicht gut)
mSinglesOpt
