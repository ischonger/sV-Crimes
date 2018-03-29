require(MASS)
# die drei 'siegermodelle':

## durch probieren
m1 <- glm.nb(formula = crimes ~ area + density + wser + prbarr + region + 
               area:density + density:wser + density:prbarr + wser:prbarr + 
               area:prbarr + density:region + area:wser, data = crimes.data, 
             init.theta = 6.810511885, link = log)

mExplorativ <- m1
# mit step(), dann modellvereinfachung
m2 <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + density:area + 
               density:pctmin + density:wsta, data = crimes.data, init.theta = 3.416777454, 
             link = log)
mStep <- m2
### andere methoden, aber ergebnisse nicht so gut:

# durch Überprüfen der einzelnen Werte als modell (aic und cv)
m3 <- glm.nb(formula = crimes ~ density + wser + wtrd + wfir + wser:wtrd + 
               density:wser, data = crimes.data, init.theta = 2.993441765, 
             link = log)
mEinzeln <- m3
# mit hilfe von cor()
m4 <- glm.nb(formula = crimes ~ (density + wser + wfir + wtrd + pctymale), 
             data = crimes.data, 
             init.theta = 2.836620662, 
             link = log)
mCor <- m4


cv(mExplorativ); cv(mStep); cv(mEinzeln); cv(mCor);
AIC(mExplorativ, mStep, mEinzeln, mCor)

### finale auswertung:
aics <- AIC(mExplorativ, mStep, mEinzeln, mCor)$AIC
plot(aics, pch = 16, ylab = "AIC", col = "red")

cvC <- cbind(cv(mExplorativ), cv(mStep), cv(mEinzeln), cv(mCor))
cvC <- cvC/max(cvC)
min(cvC)
plot(cvC[1,])

cvD <- cbind(deviance(mExplorativ), deviance(mStep), deviance(mEinzeln), deviance(mCor))
cvD <- cvD[1,]/max(cvD[1,])
min(cvD)
plot(cvC[1,], col = "green", pch = 16, ylab = "rel. SPSE")

##### gewinner: #####
m2

