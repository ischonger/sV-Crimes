mD1 <- glm.nb(crimes~(1+density), data = crimes.data)
cv(mD1); 
AIC(mD1)

mD2 <- glm.nb(crimes~(1+density+polpc), data = crimes.data)
cv(mD1);cv(mD2); 
AIC(mD1,mD2)

mD3 <- glm.nb(crimes~(1+density+area), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3)
AIC(mD1,mD2,mD3)

mD4 <- glm.nb(crimes~(1+density+taxpc), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3)
AIC(mD1,mD2,mD3)

mD5 <- glm.nb(crimes~(1+density+region), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3);cv(mD4)
AIC(mD1,mD2,mD3,mD4)

mD6 <- glm.nb(crimes~(1+density+pctymale), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3);cv(mD4);cv(mD5);cv(mD6)
AIC(mD1,mD2,mD3,mD4,mD5,mD6)

mD7 <- glm.nb(crimes~(1+density+wcon), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3);cv(mD4);cv(mD5);cv(mD6);cv(mD7)
AIC(mD1,mD2,mD3,mD4,mD5,mD6,mD7)

mD8 <- glm.nb(crimes~(1+density+wsta), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3);cv(mD4);cv(mD5);cv(mD6);cv(mD7);cv(mD8)
AIC(mD1,mD2,mD3,mD4,mD5,mD6,mD7,mD8)

mD9 <- glm.nb(crimes~(1+density+wser), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3);cv(mD4);cv(mD5);cv(mD6);cv(mD7);cv(mD8);cv(mD9)
AIC(mD1,mD2,mD3,mD4,mD5,mD6,mD7,mD8,mD9)

mD10 <- glm.nb(crimes~(1+density+wtrd), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3);cv(mD4);cv(mD5);cv(mD6);cv(mD7);cv(mD8);cv(mD9);cv(mD10)
AIC(mD1,mD2,mD3,mD4,mD5,mD6,mD7,mD8,mD9,mD10)

mD11 <- glm.nb(crimes~(1+density+wtrd), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3);cv(mD4);cv(mD5);cv(mD6);cv(mD7);cv(mD8);cv(mD9);cv(mD10);cv(mD11)
AIC(mD1,mD2,mD3,mD4,mD5,mD6,mD7,mD8,mD9,mD10,mD11)

mD12 <- glm.nb(crimes~(1+density+wfir), data = crimes.data)
dim1 <- cbind(cv(mD1),cv(mD2),cv(mD3),cv(mD4),cv(mD5),cv(mD6),cv(mD7),cv(mD8),cv(mD9),cv(mD10),cv(mD11),cv(mD12))[1,]
A1 <- AIC(mD1,mD2,mD3,mD4,mD5,mD6,mD7,mD8,mD9,mD10,mD11,mD12)$AIC
dim1 <- dim1/max(dim1)
plot(dim1)
A1 <- A1/max(A1)
plot(A1)

# => gewinner: mD3.
mD3
#--------------------------------------------------------------------------------------------------#
m <- glm.nb(formula = crimes ~ (1 + density + area), data = crimes.data)
cv(m)
AIC(m)

m1 <- glm.nb(formula = crimes ~ (1 + density + area+prbarr), data = crimes.data)
cv(m);cv(m1)
AIC(m,m1)

m2 <- glm.nb(formula = crimes ~ (1 + density + area+prbpris), data = crimes.data)
cv(m);cv(m1);cv(m2)
AIC(m,m1,m2)

m3 <- glm.nb(formula = crimes ~ (1 + density + area+polpc), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3)
AIC(m,m1,m2,m3)

m4 <- glm.nb(formula = crimes ~ (1 + density + area+taxpc), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4)
AIC(m,m1,m2,m3,m4)

m5 <- glm.nb(formula = crimes ~ (1 + density + area+region), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4);cv(m5)
AIC(m,m1,m2,m3,m4,m5)

m6 <- glm.nb(formula = crimes ~ (1 + density + area+pctmin), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4);cv(m5);cv(m6)
AIC(m,m1,m2,m3,m4,m5,m6)

m7 <- glm.nb(formula = crimes ~ (1 + density + area+pctymale), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4);cv(m5);cv(m6);cv(m7)
AIC(m,m1,m2,m3,m4,m5,m6,m7)

m8 <- glm.nb(formula = crimes ~ (1 + density + area+wcon), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4);cv(m5);cv(m6);cv(m7);cv(m8)
AIC(m,m1,m2,m3,m4,m5,m6,m7,m8)

m9 <- glm.nb(formula = crimes ~ (1 + density + area+wsta), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4);cv(m5);cv(m6);cv(m7);cv(m8);cv(m9)
AIC(m,m1,m2,m3,m4,m5,m6,m7,m8,m9)

m10 <- glm.nb(formula = crimes ~ (1 + density + area+wser), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4);cv(m5);cv(m6);cv(m7);cv(m8);cv(m9);cv(m10)
AIC(m,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)

m11 <- glm.nb(formula = crimes ~ (1 + density + area+wtrd), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4);cv(m5);cv(m6);cv(m7);cv(m8);cv(m9);cv(m10);cv(m11)
AIC(m,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11)

m12 <- glm.nb(formula = crimes ~ (1 + density + area+wfir), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4);cv(m5);cv(m6);cv(m7);cv(m8);cv(m9);cv(m10);cv(m11);cv(m12)
a <- AIC(m,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)$AIC

dim2 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6),cv(m7),cv(m8),cv(m9),cv(m10),cv(m11),cv(m12))[1,]
dim2 <- dim2/max(dim2)
plot(dim2)
dim2
min(dim2)

plot(a)
min(a)
a

m11

#------------------------------------------------------------------------------------------------#
m <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd), data = crimes.data)
m1 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+prbarr), data = crimes.data)
m2 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+prbpris), data = crimes.data) 
m3 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+polpc), data = crimes.data)
m4 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+taxpc), data = crimes.data)
m5 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+region), data = crimes.data)
m6 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+pctmin), data = crimes.data)
m7 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+pctymale), data = crimes.data)
m8 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+wcon), data = crimes.data)
m9 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+wsta), data = crimes.data)
m10 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+wser), data = crimes.data)
m11 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+wfir), data = crimes.data)
a <- AIC(m,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11)$AIC
plot(a)
a
min(a)
#m1

dim2 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6),cv(m7),cv(m8),cv(m9),cv(m10),cv(m11))[1,]
dim2 <- dim2/max(dim2)
plot(dim2)
dim2
min(dim2)

m6
#------------------------------------------------------------------------------------------------#
m <- glm.nb(formula = crimes ~ (1 + density + area + wtrd+ pctmin), data = crimes.data)
m1 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr), data = crimes.data)
m2 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbpris), data = crimes.data)
m3 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ polpc), data = crimes.data)
m4 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ taxpc), data = crimes.data)
m5 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ region), data = crimes.data)
m6 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ pctymale), data = crimes.data)
m7 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ wcon), data = crimes.data)
m8 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ wsta), data = crimes.data)
m9 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ wser), data = crimes.data)
m10 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ wfir), data = crimes.data)

a <- AIC(m,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)$AIC
plot(a)
a
min(a)
m2

dim3 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6),cv(m7),cv(m8),cv(m9),cv(m10))[1,]
dim3 <- dim3/max(dim3)
plot(dim3)
dim3
min(dim3)
#m8
## hier schon keine verbesserung mehr durch hinzugabe von eingabeparametern bei cv.
## wähle das beste ergebnis aus der aic-methode

#------------------------------------------------------------------------------------------------#
m <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr), 
                  data = crimes.data)
m1 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ prbpris), 
            data = crimes.data)
m2 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ polpc), 
             data = crimes.data)
m3 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ taxpc), 
             data = crimes.data)
m4 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ region), 
             data = crimes.data)
m5 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ pctymale), 
             data = crimes.data)
m6 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ wcon), 
             data = crimes.data)
m7 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ wsta), 
             data = crimes.data)
m8 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ wser), 
             data = crimes.data)
m9 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ wfir), 
             data = crimes.data)

a <- AIC(m,m1,m2,m3,m4,m5,m6,m7,m8,m9)$AIC
plot(a)
a
min(a)
#m9

dim4 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6),cv(m7),cv(m8),cv(m9))[1,]
dim4 <- dim4/max(dim4)
plot(dim4)
dim4
min(dim4)
m6

#------------------------------------------------------------------------------------------------#
m <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ wcon), 
             data = crimes.data)
m1 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ wcon + prbpris), 
            data = crimes.data)
m2 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ wcon + polpc), 
             data = crimes.data)
m3 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ wcon + taxpc), 
             data = crimes.data)
m4 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ wcon + region), 
             data = crimes.data)
m5 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ wcon + pctymale), 
             data = crimes.data)
m6 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ wcon + wsta), 
             data = crimes.data)
m7 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ wcon + wser), 
             data = crimes.data)
m8 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr+ wcon + wfir), 
             data = crimes.data)

a <- AIC(m,m1,m2,m3,m4,m5,m6,m7,m8)$AIC
plot(a)
a
min(a)
#m8

dim5 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6),cv(m7),cv(m8))[1,]
dim5 <- dim5/max(dim5)
plot(dim5)
dim5
min(dim5)
m5

#------------------------------------------------------------------------------------------------#
m <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale), 
            data = crimes.data, 
            init.theta = 4.06573164, 
            link = log)
m1 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + prbpris), 
            data = crimes.data, 
            init.theta = 4.06573164, 
            link = log)
m2 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + polpc), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m3 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + taxpc), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m4 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + region), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m5 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wsta), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m6 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wser), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m7 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)


a <- AIC(m,m1,m2,m3,m4,m5,m6,m7,m8)$AIC
plot(a)
a
min(a)
# m7

dim6 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6),cv(m7),cv(m8))[1,]
dim6 <- dim6/max(dim6)
plot(dim6)
dim6
min(dim6)
# auch hier wieder keine verbesserung ggü dem ausgangsmodell
# m5
# wähle aic-vorschlag

#------------------------------------------------------------------------------------------------#
m <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir), 
            data = crimes.data, 
            init.theta = 4.06573164, 
            link = log)
m1 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                        prbpris), 
            data = crimes.data, 
            init.theta = 4.06573164, 
            link = log)
m2 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m3 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         taxpc), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m4 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         region), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m5 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         wsta), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m6 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         wser), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m7 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         wser), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)


a <- AIC(m,m1,m2,m3,m4,m5,m6,m7)$AIC
plot(a)
a
min(a)
# m2

dim7 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6),cv(m7))[1,]
dim7 <- dim7/max(dim7)
plot(dim7)
dim7
min(dim7)
# auch hier wieder kein cv-nachfolgemodell, das besser ist als der vorgänger
# entscheide aic:
m2

#------------------------------------------------------------------------------------------------#
m <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                        polpc), 
            data = crimes.data, 
            init.theta = 4.06573164, 
            link = log)
m1 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                        polpc + prbpris), 
            data = crimes.data, 
            init.theta = 4.06573164, 
            link = log)
m2 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + taxpc), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m3 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + region), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m4 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + wsta), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m5 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + wser), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m6 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + wser), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)

a <- AIC(m,m1,m2,m3,m4,m5,m6)$AIC
plot(a)
a
min(a)
# m1

dim8 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6))[1,]
dim8 <- dim8/max(dim8)
plot(dim8)
dim8
min(dim8)
# so wie eben
m1

#------------------------------------------------------------------------------------------------#
m <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                        polpc + prbpris), 
            data = crimes.data, 
            init.theta = 4.06573164, 
            link = log)
m1 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                        polpc + prbpris + taxpc), 
            data = crimes.data, 
            init.theta = 4.06573164, 
            link = log)
m2 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + prbpris + region), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m3 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + prbpris + wsta), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m4 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + prbpris + wser), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m5 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + prbpris + wser), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)


a <- AIC(m,m1,m2,m3,m4,m5)$AIC
plot(a)
a
min(a)
# aic-werte alle noch näher beieinander! 1428.5-130.5! 
# => alle modelle hier sehr ähnlich
# m2

dim9 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5))[1,]
dim9 <- dim9/max(dim9)
plot(dim9)
dim9
min(dim9)
m2

#------------------------------------------------------------------------------------------------#
m <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                        polpc + prbpris + region), 
            data = crimes.data, 
            init.theta = 4.06573164, 
            link = log)
m1 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                        polpc + prbpris + region + taxpc), 
            data = crimes.data, 
            init.theta = 4.06573164, 
            link = log)
m2 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + prbpris + region + wsta), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m3 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + prbpris + region + wser), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m4 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + prbpris + region + wser), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)

a <- AIC(m,m1,m2,m3,m4)$AIC
plot(a)
a
min(a)
# diesmal hier keine verbesserung
# aber aic-werte im intervall [1428.5, 1429.5]

dim10 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4))[1,]
dim10 <- dim10/max(dim10)
plot(dim10)
dim10
min(dim10)
# aic werte werden bei den modellen höher, wo der fehler geringer wird
# => die verbliebenen drei eingabevektoren haben 'keinen guten einfluss' auf die modellauswahl
# wfir hat scheinbar gar keinen einfluss auf die modellgüte:
# aic und cv-wert sind beide gleich. beide sind in beiden anschauungen das minimum
## soll heißen:
## das ausgangsmodell wird in keinem fall verbessert
## breche daher hier ab

# breche hier ab.
# die letzten vier werte im Gauß-Modell waren:
# wsta taxpc prbarr
# hier (nb) ist es ähnlich:
# wsta taxpc wser

#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
# ich entscheide mich dazu, das modell zu nehmen, welches noch 'gesunde' werte hatte.
# das war relativ früh geschehen.
mSW <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr), 
              data = crimes.data)
# begründung: 
m <- glm.nb(formula = crimes ~ (1 + density + area + wtrd+ pctmin), data = crimes.data)
m1 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbarr), data = crimes.data)
m2 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ prbpris), data = crimes.data)
m3 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ polpc), data = crimes.data)
m4 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ taxpc), data = crimes.data)
m5 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ region), data = crimes.data)
m6 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ pctymale), data = crimes.data)
m7 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ wcon), data = crimes.data)
m8 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ wsta), data = crimes.data)
m9 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ wser), data = crimes.data)
m10 <- glm.nb(formula = crimes ~ (1 + density + area+ wtrd+ pctmin+ wfir), data = crimes.data)

a <- AIC(m,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)$AIC
plot(a)
a
min(a)

# mit plot(a) kann man sehr gut sehen, dass die verbesserung um wfir hier noch relativ groß ist
# es gibt in dem plot drei größere sprünge:
# von m1 zu m9
# von m7 zu m3
# von m3 zu m5
# die fehler-abstände werden immer geringer!
# so ist zu wählen, ab welchem punkt man sagt, dass der hinzugewinn nichtig genug ist.

# nach dim3 gibt es zwar noch bessere aic-werte. dies ist natürlich nicht ausschlaggebend,
# zumal da der cv-wert hier noch einen eindeutigen nächsten kandidaten bestimmen konnte
# nach dim3 sind nennenswerten verbesserungen mehr entstanden:

m <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                        polpc + prbpris + region), 
            data = crimes.data, 
            init.theta = 4.06573164, 
            link = log)
m1 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + prbpris + region + taxpc), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m2 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + prbpris + region + wsta), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m3 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + prbpris + region + wser), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)
m4 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                         polpc + prbpris + region + wser), 
             data = crimes.data, 
             init.theta = 4.06573164, 
             link = log)

a <- AIC(m,m1,m2,m3,m4)$AIC
plot(a)
a
min(a)
# mit plot(a) sieht man, dass das ausgangsmodell noch einen besseren aic-wert hat
# auch wenn dieser nur um 1 besser ist, als die nachfolgenden modelle

dim10 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4))[1,]
dim10 <- dim10/max(dim10)
plot(dim10)
dim10
min(dim10)


# 
# sogar schon bei dim3 zu dim4 gibt es einen großen sprung in der fehlerabweichung
# so könnte auch schon 
mSW2 <- glm.nb(crimes ~ (1 + density + area + wtrd + pctmin + prbarr + wcon + pctymale + wfir + 
                           polpc + prbpris + region), 
               data = crimes.data, 
               init.theta = 4.06573164, 
               link = log)
# als gutes modell genommen werden
cv(mSW2); cv(mSW)
cv(m3O2)

# optimiere:
step(mSW2, ~(density+area+wtrd+pctmin+prbarr+wcon+pctymale+wfir+polpc+prbpris+region)^2)

mSW2O <- glm.nb(formula = crimes ~ density + area + wtrd + pctmin + prbarr + 
                  wcon + pctymale + wfir + polpc + prbpris + pctmin:wcon + 
                  density:prbarr + wcon:wfir + pctymale:polpc + wtrd:prbpris + 
                  area:pctmin + wcon:polpc + wfir:polpc + density:area + pctmin:prbarr + 
                  density:pctymale + area:wcon + area:prbarr + density:pctmin + 
                  pctmin:wfir + density:wfir + area:wtrd + density:prbpris + 
                  prbarr:prbpris + area:pctymale + wtrd:polpc + polpc:prbpris + 
                  pctymale:prbpris + pctmin:pctymale + pctymale:wfir, data = crimes.data, 
                init.theta = 30.31328806, link = log)
cv(mSW2); cv(mSW); cv(mSW2O); cv(m3O2)
# => keine verbesserung durch step()-optimierung

##### gewinner in dieser modellsuche:
mSW
