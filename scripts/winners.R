#require(MASS)
# die drei 'siegermodelle':

## durch probieren
m1 <- glm.nb(formula = crimes ~ area + density + wser + prbarr + region + 
               area:density + density:wser + density:prbarr + wser:prbarr + 
               area:prbarr + density:region + area:wser, data = crimes.data, 
             init.theta = 6.810511885, link = log)

# mit step(), dann modellvereinfachung
m2 <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + density:area + 
               density:pctmin + density:wsta, data = crimes.data, init.theta = 3.416777454, 
             link = log)

### andere methoden, aber ergebnisse nicht so gut:

# durch Überprüfen der einzelnen Werte als modell (aic und cv)
m3 <- glm.nb(formula = crimes ~ density + wser + wtrd + wfir + wser:wtrd + 
               density:wser, data = crimes.data, init.theta = 2.993441765, 
             link = log)

# mit hilfe von cor()
m4 <- glm.nb(formula = crimes ~ (density + wser + wfir + wtrd + pctymale), 
             data = crimes.data, 
             init.theta = 2.836620662, 
             link = log)

# stepwise_modelselection
m5 <- glm.nb(formula = crimes ~ (1 + density + area + wtrd + pctmin + prbarr), 
             data = crimes.data, 
             init.theta = 3.465133653, 
             link = log)

cv(m1); cv(m2); cv(m3); cv(m4); cv(m5);
AIC(m1, m2, m3, m4, m5)

### finale auswertung:
aics <- AIC(m1, m2, m3, m4, m5)$AIC
plot(aics)

cvC <- cbind(cv(m1), cv(m2), cv(m3), cv(m4), cv(m5))
cvC <- cvC/max(cvC)
min(cvC)
plot(cvC)

##### gewinner: #####
m2
