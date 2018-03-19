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

m1 <- glm(formula = crimes ~ prbarr + prbpris + polpc + density + area + region + pctmin + pctymale + wcon + wsta + wser + 
            wtrd + wfir + prbarr:prbpris + prbarr:polpc + prbarr:density + 
            prbarr:area + prbarr:region + prbarr:pctmin + 
            prbarr:pctymale + prbarr:wcon + prbarr:wsta + prbarr:wser + 
            prbarr:wtrd + prbarr:wfir + prbpris:polpc + prbpris:density + 
            prbpris:area + prbpris:region + prbpris:pctmin + 
            prbpris:pctymale + prbpris:wcon + prbpris:wsta + prbpris:wser + 
            prbpris:wtrd + prbpris:wfir + polpc:density + polpc:area + polpc:region + polpc:pctmin + polpc:pctymale + 
            polpc:wcon + polpc:wsta + polpc:wser + polpc:wtrd + polpc:wfir + 
            density:area + density:region + density:pctmin + 
            density:pctymale + density:wcon + density:wsta + density:wser + 
            density:wtrd + density:wfir + area:region + 
            area:pctmin + area:pctymale + area:wcon + area:wsta + area:wser + 
            area:wtrd + area:wfir + region:pctmin + 
            region:pctymale + region:wcon, data = crimes.data)
plot(mStep, which = 1)

m2 <- glm(formula = crimes ~ prbarr + prbpris + polpc + density + area + region + wcon + wsta + wser + 
            wtrd + wfir + prbarr:prbpris + prbarr:polpc + prbarr:density + 
            prbarr:area + prbarr:region + prbarr:wcon + prbarr:wsta + prbarr:wser + 
            prbarr:wtrd + prbarr:wfir + prbpris:polpc + prbpris:density + 
            prbpris:area + prbpris:region + prbpris:wcon + prbpris:wsta + prbpris:wser + 
            prbpris:wtrd + prbpris:wfir + polpc:density + polpc:area + polpc:region + 
            polpc:wcon + polpc:wsta + polpc:wser + polpc:wtrd + polpc:wfir + 
            density:area + density:region + density:wcon + density:wsta + density:wser + 
            density:wtrd + density:wfir + area:region + 
            area:wcon + area:wsta + area:wser + 
            area:wtrd + area:wfir + region:wcon, data = crimes.data)
plot(m2, which = 1)

m3 <- glm(formula = crimes ~ prbarr + prbpris + polpc + density + area + region + wcon + wsta + wser + 
            wtrd + prbarr:prbpris + prbarr:polpc + prbarr:density + 
            prbarr:area + prbarr:region + prbarr:wcon + prbarr:wsta + prbarr:wser + 
            prbarr:wtrd + prbpris:polpc + prbpris:density + 
            prbpris:area + prbpris:region + prbpris:wcon + prbpris:wsta + prbpris:wser + 
            prbpris:wtrd + polpc:density + polpc:area + polpc:region + 
            polpc:wcon + polpc:wsta + polpc:wser + polpc:wtrd + 
            density:area + density:region + density:wcon + density:wsta + density:wser + 
            density:wtrd + area:region + 
            area:wcon + area:wsta + area:wser + 
            area:wtrd + region:wcon, data = crimes.data)
plot(m3, which = 1)
# 168426156
# 23534114
# 3083311
# 6063010
# 7175119

# -taxpc, -wfir, -wtrd
m4 <- glm(formula = crimes ~ prbarr + prbpris + polpc + density + area + region + wcon + wsta + wser +
            prbarr:prbpris + prbarr:polpc + prbarr:density + 
            prbarr:area + prbarr:region + prbarr:wcon + prbarr:wsta + prbarr:wser + 
            prbpris:polpc + prbpris:density + 
            prbpris:area + prbpris:region + prbpris:wcon + prbpris:wsta + prbpris:wser + 
            polpc:density + polpc:area + polpc:region + 
            polpc:wcon + polpc:wsta + polpc:wser +  
            density:area + density:region + density:wcon + density:wsta + density:wser + 
            area:region + 
            area:wcon + area:wsta + area:wser + 
            region:wcon, 
          data = crimes.data)
plot(m4, which = 1)
cv(mSinglesOpt); cv(mR); cv(m1); cv(m2); cv(m3); cv(m4)
AIC(mSinglesOpt, m3O2, m1, m2, m3, m4)

m5 <- glm(formula = crimes ~ prbarr + prbpris + polpc + density + area + region + wcon + wser +
            prbarr:prbpris + prbarr:polpc + prbarr:density + 
            prbarr:area + prbarr:region + prbarr:wcon + prbarr:wser + 
            prbpris:polpc + prbpris:density + 
            prbpris:area + prbpris:region + prbpris:wcon + prbpris:wser + 
            polpc:density + polpc:area + polpc:region + 
            polpc:wcon + polpc:wser +  
            density:area + density:region + density:wcon + density:wser + 
            area:region + 
            area:wcon + area:wser + 
            region:wcon, 
          data = crimes.data)
plot(m5, which = 1)
cv(mSinglesOpt); cv(mR); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5)
AIC(mSinglesOpt, m3O2, m1, m2, m3, m4, m5)

# -wcon
m6Wcon <- glm(formula = crimes ~ prbarr + prbpris + polpc + density + area + region + wser +
            prbarr:prbpris + prbarr:polpc + prbarr:density + 
            prbarr:area + prbarr:region + prbarr:wser + 
            prbpris:polpc + prbpris:density + 
            prbpris:area + prbpris:region + prbpris:wser + 
            polpc:density + polpc:area + polpc:region + 
            polpc:wser +  
            density:area + density:region + density:wser + 
            area:region + 
            area:wser, 
          data = crimes.data)
plot(m6Wcon, which = 1)
cv(mSinglesOpt); cv(mR); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5);cv(m6Wcon)
AIC(mSinglesOpt, m3O2, m1, m2, m3, m4, m5, m6Wcon)
# prbpris und prparr haben als einzelne einflussgrößen keinen großen einfluss.
# jedoch wechselwirken sie mit anderen einflussgrößen, sodass recht gute merkmale 
# gebildet werden können
# egal, ob wcon oder wser entfernt wird:
# der fehler vergrößert sich sehr stark bei cv. bei aic weniger spürbar merkbar.
# dann ist das modell noch etwas besser als m3O2.

# -wser
m6Wser <- glm(formula = crimes ~ prbarr + prbpris + polpc + density + area + region + wcon +
            prbarr:prbpris + prbarr:polpc + prbarr:density + 
            prbarr:area + prbarr:region + prbarr:wcon + 
            prbpris:polpc + prbpris:density + 
            prbpris:area + prbpris:region + prbpris:wcon + 
            polpc:density + polpc:area + polpc:region + 
            polpc:wcon +  
            density:area + density:region + density:wcon + 
            area:region + 
            area:wcon + 
            region:wcon, 
          data = crimes.data)
cv(mSinglesOpt); cv(mR); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5);cv(m6Wcon); cv(m6Wser)
AIC(mSinglesOpt, m3O2, m1, m2, m3, m4, m5, m6Wcon, m6Wser)

# wcon muss bleiben, - region
m7 <- glm(formula = crimes ~ prbarr + prbpris + polpc + density + area + wcon +
                prbarr:prbpris + prbarr:polpc + prbarr:density + 
                prbarr:area + prbarr:wcon + 
                prbpris:polpc + prbpris:density + 
                prbpris:area + prbpris:wcon + 
                polpc:density + polpc:area + 
                polpc:wcon +  
                density:area + density:wcon + 
                area:wcon + 
                region:wcon, 
              data = crimes.data)
cv(mSinglesOpt); cv(mR); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5);cv(m6Wcon); cv(m6Wser); cv(m7)
AIC(mSinglesOpt, m3O2, m1, m2, m3, m4, m5, m6Wcon, m6Wser, m7)

# - polpc
m8 <- glm(formula = crimes ~ prbarr + prbpris + density + area + wcon +
            prbarr:prbpris + prbarr:density + 
            prbarr:area + prbarr:wcon + 
            prbpris:density + 
            prbpris:area + prbpris:wcon + 
            polpc:density + 
            density:area + density:wcon + 
            area:wcon + 
            region:wcon, 
          data = crimes.data)
cv(mSinglesOpt); cv(mR); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5);cv(m6Wcon); cv(m6Wser); cv(m7); cv(m8)
AIC(mSinglesOpt, m3O2, m1, m2, m3, m4, m5, m6Wcon, m6Wser, m7, m8)

# - area
m9 <- glm(formula = crimes ~ prbarr + prbpris + density + wcon +
            prbarr:prbpris + prbarr:density + 
            prbarr:wcon + 
            prbpris:density + 
            prbpris:wcon + 
            polpc:density + 
            density:wcon + 
            region:wcon, 
          data = crimes.data)
cv(mSinglesOpt); cv(mR); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5);cv(m6Wcon); cv(m6Wser); cv(m7); cv(m8);cv(m9)
AIC(mSinglesOpt, m3O2, m1, m2, m3, m4, m5, m6Wcon, m6Wser, m7, m8, m9)

mStepO <- m9

# beste modell, indem man mStep 'verjüngt':
# es wurden solange einflussgrößen weggenommen solange wie der fehler noch geringer war, als der,
# der in find_model() gefunden wurde.
# beide modelle (m3O2 und mStepO) sind sich auch recht ähnlich
m3O2 <- glm(formula = crimes ~ area + density + wser + prbarr + area:density + 
             density:wser + density:prbarr + wser:prbarr + area:prbarr, 
           data = crimes.data)
mStepO <- glm(formula = crimes ~ prbarr + prbpris + density + wcon + prbarr:prbpris + 
                prbarr:density + prbarr:wcon + prbpris:density + prbpris:wcon + 
                polpc:density + density:wcon + region:wcon, data = crimes.data)
step(mStepO, ~(prbarr+prbpris+density+wcon+prbarr:prbpris+prbarr:density + prbarr:wcon + prbpris:density + prbpris:wcon + 
                 polpc:density + density:wcon + region:wcon))

mStepO2 <- glm(formula = crimes ~ prbarr + prbpris + density + wcon + prbarr:density + 
                 prbpris:density + prbpris:wcon + density:polpc + density:wcon + 
                 wcon:region, data = crimes.data)
cv(mSinglesOpt); cv(mR); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5);cv(m6Wcon); cv(m6Wser); cv(m7); cv(m8);cv(m9);cv(mStepO);cv(mStepO2)
AIC(mSinglesOpt, m3O2, m1, m2, m3, m4, m5, m6Wcon, m6Wser, m7, m8, m9, mStepO, mStepO2)
# trotz optimierung ist der cv-fehler von mStepO geringer als der von mStepO2!!

## gewinnermodell nach dieser methode:
mStepO
