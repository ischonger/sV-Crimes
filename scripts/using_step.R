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
