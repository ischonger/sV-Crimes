# die drei 'siegermodelle':

## durch probieren
m1 <- glm(formula = crimes ~ area + density + wser + prbarr + region + 
            area:density + density:wser + density:prbarr + wser:prbarr + 
            density:region + wser:region, data = crimes.data)

# mit step(), dann modellvereinfachung
m2 <- glm(formula = crimes ~ prbarr + prbpris + density + wcon + prbarr:prbpris + 
            prbarr:density + prbarr:wcon + prbpris:density + prbpris:wcon + 
            polpc:density + density:wcon + region:wcon, data = crimes.data)


### andere methoden, aber ergebnisse nicht so gut:

# durch Überprüfen der einzelnen Werte als modell (aic und cv)
m3 <- glm(formula = crimes ~ density + wser + wtrd + wfir + density:wfir + 
                     density:wser + density:wtrd, data = crimes.data)

# mit hilfe von cor()
m4 <- glm(formula = crimes ~ (density + wser + wfir + wtrd + wcon), 
          data = crimes.data)

# stepwise_modelselection
m5 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris), 
          data = crimes.data)

cv(m1); cv(m2); cv(m3); cv(m4); cv(m5)
AIC(m1, m2, m3, m4, m5)