### using step ###
step(m5T,scope = ~(1+prbarr+region+area+density+wcon+wsta+wser+wtrd+wfir)^2)
m5opt <- glm.nb(formula = crimes ~ density, family = binomial, data = crimes.data, model = TRUE, )
AIC(m5opt)
plot(m5opt, which = 1)
plot(m5T, which = 1)

mAll2 <- glm.nb((crimes~(1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir)^2), 
                data = crimes.data,
                link = log)
plot(mAll2)
step(mAll2, ~(1+prbarr+prbpris+polpc+density+area+taxpc+region+pctmin+pctymale+wcon+wsta+wser+wtrd+wfir)^2)
mStep <- glm.nb(formula = crimes ~ prbarr + prbpris + polpc + density + area + 
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

m1 <- glm.nb(formula = crimes ~ prbarr + prbpris + polpc + density + area + region + pctmin + pctymale + wcon + wsta + wser + 
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
cv(mSinglesOpt); cv(mR); cv(m1); cv(m2); cv(m3); cv(m4)
AIC(mSinglesOpt, m3O2, m1, m2, m3, m4)

# - prbarr
m2 <- glm.nb(formula = crimes ~ prbpris + polpc + density + area + region + pctmin + pctymale + wcon + wsta + wser + 
               wtrd + wfir + prbpris:polpc + prbpris:density + 
               prbpris:area + prbpris:region + prbpris:pctmin + 
               prbpris:pctymale + prbpris:wcon + prbpris:wsta + prbpris:wser + 
               prbpris:wtrd + prbpris:wfir + polpc:density + polpc:area + polpc:region + polpc:pctmin + polpc:pctymale + 
               polpc:wcon + polpc:wsta + polpc:wser + polpc:wtrd + polpc:wfir + 
               density:area + density:region + density:pctmin + 
               density:pctymale + density:wcon + density:wsta + density:wser + 
               density:wtrd + density:wfir + area:region + 
               area:pctmin + area:pctymale + area:wcon + area:wsta + area:wser + 
               area:wtrd + area:wfir + region:pctmin + 
               region:pctymale + region:wcon, 
             data = crimes.data)
plot(mStep, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2);
AIC(mSinglesOpt, m1, m2)

# - prbpris
m3 <- glm.nb(formula = crimes ~ polpc + density + area + region + pctmin + pctymale + wcon + wsta + wser + 
               wtrd + wfir + polpc:density + polpc:area + polpc:region + polpc:pctmin + 
               polpc:pctymale + 
               polpc:wcon + polpc:wsta + polpc:wser + polpc:wtrd + polpc:wfir + 
               density:area + density:region + density:pctmin + 
               density:pctymale + density:wcon + density:wsta + density:wser + 
               density:wtrd + density:wfir + area:region + 
               area:pctmin + area:pctymale + area:wcon + area:wsta + area:wser + 
               area:wtrd + area:wfir + region:pctmin + 
               region:pctymale + region:wcon, 
             data = crimes.data)
plot(m3, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3)
AIC(mSinglesOpt, m1, m2, m3)

# - region
m4 <- glm.nb(formula = crimes ~ polpc + density + area + pctmin + pctymale + wcon + wsta + wser + 
               wtrd + wfir + polpc:density + polpc:area + polpc:pctmin + 
               polpc:pctymale + 
               polpc:wcon + polpc:wsta + polpc:wser + polpc:wtrd + polpc:wfir + 
               density:area + density:pctmin + 
               density:pctymale + density:wcon + density:wsta + density:wser + 
               density:wtrd + density:wfir + 
               area:pctmin + area:pctymale + area:wcon + area:wsta + area:wser + 
               area:wtrd + area:wfir +  
               region:pctymale, 
             data = crimes.data)
plot(m4, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4)
AIC(mSinglesOpt, m1, m2, m3, m4)

# - polpc
m5 <- glm.nb(formula = crimes ~ density + area + pctmin + pctymale + wcon + wsta + wser + 
               wtrd + wfir + 
               density:area + density:pctmin + 
               density:pctymale + density:wcon + density:wsta + density:wser + 
               density:wtrd + density:wfir + 
               area:pctmin + area:pctymale + area:wcon + area:wsta + area:wser + 
               area:wtrd + area:wfir +  
               region:pctymale, 
             data = crimes.data)
plot(m5, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5)
AIC(mSinglesOpt, m1, m2, m3, m4, m5)

# - pctymale
m6 <- glm.nb(formula = crimes ~ density + area + pctmin + wcon + wsta + wser + 
               wtrd + wfir + 
               density:area + density:pctmin + 
               density:wcon + density:wsta + density:wser + 
               density:wtrd + density:wfir + 
               area:pctmin + area:wcon + area:wsta + area:wser + 
               area:wtrd + area:wfir, 
             data = crimes.data)
plot(m6, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6)

# - wcon
m7Wcon <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + wser + 
               wtrd + wfir + 
               density:area + density:pctmin + 
               density:wsta + density:wser + 
               density:wtrd + density:wfir + 
               area:pctmin + area:wsta + area:wser + 
               area:wtrd + area:wfir, 
             data = crimes.data)
plot(m7, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wcon)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wcon)

# - wsta
m7Wsta <- glm.nb(formula = crimes ~ density + area + pctmin + wcon + wser + 
               wtrd + wfir + 
               density:area + density:pctmin + 
               density:wcon + density:wser + 
               density:wtrd + density:wfir + 
               area:pctmin + area:wcon + area:wser + 
               area:wtrd + area:wfir, 
             data = crimes.data)
plot(m7Wsta, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wcon); cv(m7Wsta)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wcon, m7Wsta)

# - wser
m7Wser <- glm.nb(formula = crimes ~ density + area + pctmin + wcon + wsta + 
               wtrd + wfir + 
               density:area + density:pctmin + 
               density:wcon + density:wsta + 
               density:wtrd + density:wfir + 
               area:pctmin + area:wcon + area:wsta + 
               area:wtrd + area:wfir, 
             data = crimes.data)
plot(m7Wser, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wcon); cv(m7Wsta); cv(m7Wser)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wcon, m7Wsta, m7Wser)

# - wtrd
m7Wtrd <- glm.nb(formula = crimes ~ density + area + pctmin + wcon + wsta + wser + 
               wfir + 
               density:area + density:pctmin + 
               density:wcon + density:wsta + density:wser + 
               density:wfir + 
               area:pctmin + area:wcon + area:wsta + area:wser + 
               area:wfir, 
             data = crimes.data)
plot(m7Wtrd, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wcon); cv(m7Wsta); cv(m7Wser); cv(m7Wtrd)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wcon, m7Wsta, m7Wser, m7Wtrd)

# - wfir
m7Wfir <- glm.nb(formula = crimes ~ density + area + pctmin + wcon + wsta + wser + 
               wtrd + 
               density:area + density:pctmin + 
               density:wcon + density:wsta + density:wser + 
               density:wtrd +
               area:pctmin + area:wcon + area:wsta + area:wser + 
               area:wtrd, 
             data = crimes.data)
plot(m7Wfir, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wcon); cv(m7Wsta); cv(m7Wser); cv(m7Wtrd); cv(m7Wfir)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wcon, m7Wsta, m7Wser, m7Wtrd, m7Wfir)

# - wtrd
m8Wtrd <- glm.nb(formula = crimes ~ density + area + pctmin + wcon + wsta + wser + 
                   density:area + density:pctmin + 
                   density:wcon + density:wsta + density:wser + 
                   area:pctmin + area:wcon + area:wsta + area:wser, 
                 data = crimes.data)
plot(m8Wtrd, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wcon, m7Wsta, m7Wser, m7Wtrd, m7Wfir)

# - wsta
m8Wsta <- glm.nb(formula = crimes ~ density + area + pctmin + wcon + wtrd + 
                   density:area + density:pctmin + 
                   density:wcon + density:wser + 
                   area:pctmin + area:wcon + area:wser, 
                 data = crimes.data)
plot(m7Wfir, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m8Wsta)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wcon, m7Wsta, m7Wser, m7Wtrd, m7Wfir, m8Wtrd, m8Wsta)

# - wcon
m8Wcon <- glm.nb(formula = crimes ~ density + area + pctmin + wcon + wsta + wser + 
                   wtrd + 
                   density:area + density:pctmin + 
                   density:wsta + density:wser + 
                   density:wtrd +
                   area:pctmin + area:wsta + area:wser + 
                   area:wtrd, 
                 data = crimes.data)
plot(m8Wcon, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m8Wsta); cv(m8Wcon)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wcon, m7Wsta, m7Wser, m7Wtrd, m7Wfir, m8Wtrd, m8Wsta, m8Wcon)

# - pctmin
m8Pctmin <- glm.nb(formula = crimes ~ density + area + wcon + wsta + wser + 
                   wtrd + 
                   density:area + 
                   density:wcon + density:wsta + density:wser + 
                   density:wtrd +
                   area:wcon + area:wsta + area:wser + 
                   area:wtrd, 
                 data = crimes.data)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m8Wsta); cv(m8Wcon); cv(m8Pctmin)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wcon, m7Wsta, m7Wser, m7Wtrd, m7Wfir, m8Wtrd, m8Wsta, m8Wcon, m8Pctmin)

# - wser
m9Wser <- glm.nb(formula = crimes ~ density + area + pctmin + wcon + wsta + 
                   density:area + density:pctmin + 
                   density:wcon + density:wsta + 
                   area:pctmin + area:wcon + area:wsta, 
                 data = crimes.data)
plot(m9Wtrd, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wser)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wser)

# - wsta
m9Wsta <- glm.nb(formula = crimes ~ density + area + pctmin + wcon + wser + 
                   density:area + density:pctmin + 
                   density:wcon + density:wser + 
                   area:pctmin + area:wcon + area:wser, 
                 data = crimes.data)
plot(m9Wsta, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wsta)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wser, m9Wsta)

# - wcon
m9Wcon <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + wser + 
                   density:area + density:pctmin + 
                   density:wsta + density:wser + 
                   area:pctmin + area:wsta + area:wser, 
                 data = crimes.data)
plot(m9Wcon, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wsta); cv(m9Wcon)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wser, m9Wsta, m9Wcon)

# - wtrd
m9Pctmin <- glm.nb(formula = crimes ~ density + area + wcon + wsta + wser + 
                   density:area + 
                   density:wcon + density:wsta + density:wser + 
                   area:wcon + area:wsta + area:wser, 
                 data = crimes.data)
plot(m9Pctmin, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wsta); cv(m9Wcon); cv(m9Pctmin)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wser, m9Wsta, m9Wcon, m9Pctmin)

# - wcon
m9Wcon <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + wser + 
                   density:area + density:pctmin + 
                   density:wsta + density:wser + 
                   area:pctmin + area:wsta + area:wser, 
                 data = crimes.data)
plot(m9Wcon, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon)

# - wser
m10Wser <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + 
                   density:area + density:pctmin + 
                   density:wsta + 
                   area:pctmin + area:wsta, 
                 data = crimes.data)
plot(m9Wcon, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser)

# - wsta
m10Wsta <- glm.nb(formula = crimes ~ density + area + pctmin + wser + 
                   density:area + density:pctmin + 
                   density:wser + 
                   area:pctmin + area:wser, 
                 data = crimes.data)
plot(m10Wsta, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m10Wsta)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m10Wsta)

# - pctmin
m10Pctmin <- glm.nb(formula = crimes ~ density + area + wsta + wser + 
                   density:area +  
                   density:wsta + density:wser + 
                   area:wsta + area:wser, 
                 data = crimes.data)
plot(m10Pctmin, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m10Wsta); cv(m10Pctmin)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m10Wsta, m10Pctmin)

# - wsta
m11Wsta <- glm.nb(formula = crimes ~ density + area + pctmin + 
                    density:area + density:pctmin + 
                    area:pctmin, 
                  data = crimes.data)
plot(m11Wsta, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m11Wsta)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m11Wsta)

# - pctmin
m11Pctmin <- glm.nb(formula = crimes ~ density + area + wsta + 
                    density:area + 
                    density:wsta + 
                    area:wsta, 
                  data = crimes.data)
plot(m11Pctmin, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m11Wsta); cv(m11Pctmin)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m11Wsta, m11Pctmin)

# keine verbesserung mehr:
# entferne intersections
# - area:wsta
m12Aw <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + 
                    density:area + density:pctmin + 
                    density:wsta + 
                    area:pctmin, 
                  data = crimes.data)
plot(m12Aw, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m12Aw)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m12Aw)

# - area:pctmin
m12Ap <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + 
                    density:area + density:pctmin + 
                    density:wsta + 
                    area:wsta, 
                  data = crimes.data)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m12Aw); cv(m12Ap)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m12Aw, m12Ap)

# - density:wsta
m12Dw <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + 
                    density:area + density:pctmin + 
                    area:pctmin + area:wsta, 
                  data = crimes.data)
plot(m12Dw, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m12Aw); cv(m12Ap); cv(m12Dw)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m12Aw, m12Ap, m12Dw)

# - density:pctmin
m12Dp <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + 
                    density:area + 
                    density:wsta + 
                    area:pctmin + area:wsta, 
                  data = crimes.data)
plot(m12Dp, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m12Aw); cv(m12Ap); cv(m12Dw); cv(m12Dp)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m12Aw, m12Ap, m12Dw, m12Dp)

# - density:area
m12Da <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + 
                    density:pctmin + 
                    density:wsta + 
                    area:pctmin + area:wsta, 
                  data = crimes.data)
plot(m12Da, which = 1)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m12Ap);
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m12Ap)

# - area:wsta
m13Aw <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + 
                  density:area + density:pctmin + 
                  density:wsta, 
                data = crimes.data)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m12Ap); cv(m13Aw)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m12Aw, m12Ap, m13Aw)

# - density:wsta
m13Dw <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + 
                  density:area + density:pctmin + 
                  area:wsta, 
                data = crimes.data)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m12Ap); cv(m13Aw); cv(m13Dw)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m12Ap, m13Aw, m13Dw)

# - density:pctmin
m13Dp <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + 
                  density:area + 
                  density:wsta + 
                  area:wsta, 
                data = crimes.data)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m12Ap); cv(m13Aw); cv(m13Dw); cv(m13Dp)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m12Ap, m13Aw, m13Dw, m13Dp)

# - density:area
m13Da <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + 
                  density:pctmin + 
                  density:wsta + 
                  area:wsta, 
                data = crimes.data)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m12Ap); cv(m13Aw);
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m12Ap, m13Aw)

# - density:wsta
m14Dw <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + 
                  density:area + density:pctmin, 
                data = crimes.data)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m12Ap); cv(m13Aw); cv(m14Dw)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m12Ap, m13Aw, m14Dw)

# - density:pctmin
m14Dp <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + 
                  density:area + 
                  density:wsta, 
                data = crimes.data)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m12Ap); cv(m13Aw); cv(m14Dw); cv(m14Dp)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m12Ap, m13Aw, m14Dw, m14Dp)

# - area:wsta
m14Da <- glm.nb(formula = crimes ~ density + area + pctmin + wsta + 
                  density:pctmin + 
                  density:wsta, 
                data = crimes.data)
cv(mSinglesOpt); cv(m1); cv(m2); cv(m3); cv(m4); cv(m5); cv(m6); cv(m7Wfir); cv(m8Wtrd); cv(m9Wcon); cv(m10Wser); cv(m12Ap); cv(m13Aw); cv(m14Dw); cv(m14Dp); cv(m14Da)
AIC(mSinglesOpt, m1, m2, m3, m4, m5, m6, m7Wfir, m8Wtrd, m9Wcon, m10Wser, m12Ap, m13Aw, m14Dw, m14Dp, m14Da)

cvC <- cbind(cv(mSinglesOpt), cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6),cv(m7Wfir),cv(m8Wtrd),cv(m9Wcon),cv(m10Wser),cv(m12Ap),cv(m13Aw))
plot(cvC[1,])  
cvC <- cvC[1,]/max(cvC[1,])
plot(cvC)
min(cvC)

# bestes ohne intersect-entfernung: m9Wcon
# ansonsten:
m13Aw
