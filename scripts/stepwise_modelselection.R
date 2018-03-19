mD1 <- glm(crimes~(1+density), data = crimes.data)
cv(mD1); 
AIC(mD1)

mD2 <- glm(crimes~(1+density+polpc), data = crimes.data)
cv(mD1);cv(mD2); 
AIC(mD1,mD2)

mD3 <- glm(crimes~(1+density+area), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3)
AIC(mD1,mD2,mD3)

mD4 <- glm(crimes~(1+density+taxpc), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3)
AIC(mD1,mD2,mD3)

mD5 <- glm(crimes~(1+density+region), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3);cv(mD4)
AIC(mD1,mD2,mD3,mD4)

mD6 <- glm(crimes~(1+density+pctymale), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3);cv(mD4);cv(mD5);cv(mD6)
AIC(mD1,mD2,mD3,mD4,mD5,mD6)

mD7 <- glm(crimes~(1+density+wcon), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3);cv(mD4);cv(mD5);cv(mD6);cv(mD7)
AIC(mD1,mD2,mD3,mD4,mD5,mD6,mD7)

mD8 <- glm(crimes~(1+density+wsta), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3);cv(mD4);cv(mD5);cv(mD6);cv(mD7);cv(mD8)
AIC(mD1,mD2,mD3,mD4,mD5,mD6,mD7,mD8)

mD9 <- glm(crimes~(1+density+wser), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3);cv(mD4);cv(mD5);cv(mD6);cv(mD7);cv(mD8);cv(mD9)
AIC(mD1,mD2,mD3,mD4,mD5,mD6,mD7,mD8,mD9)

mD10 <- glm(crimes~(1+density+wtrd), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3);cv(mD4);cv(mD5);cv(mD6);cv(mD7);cv(mD8);cv(mD9);cv(mD10)
AIC(mD1,mD2,mD3,mD4,mD5,mD6,mD7,mD8,mD9,mD10)

mD11 <- glm(crimes~(1+density+wtrd), data = crimes.data)
cv(mD1);cv(mD2);cv(mD3);cv(mD4);cv(mD5);cv(mD6);cv(mD7);cv(mD8);cv(mD9);cv(mD10);cv(mD11)
AIC(mD1,mD2,mD3,mD4,mD5,mD6,mD7,mD8,mD9,mD10,mD11)

mD12 <- glm(crimes~(1+density+wfir), data = crimes.data)
dim1 <- cbind(cv(mD1),cv(mD2),cv(mD3),cv(mD4),cv(mD5),cv(mD6),cv(mD7),cv(mD8),cv(mD9),cv(mD10),cv(mD11),cv(mD12))[1,]
A1 <- AIC(mD1,mD2,mD3,mD4,mD5,mD6,mD7,mD8,mD9,mD10,mD11,mD12)$AIC
dim1 <- dim1/max(dim1)
plot(dim1)
A1 <- A1/max(A1)
plot(A1)

# => gewinner: mD3.
mD3
#--------------------------------------------------------------------------------------------------#
m <- glm(formula = crimes ~ (1 + density + area), data = crimes.data)
cv(m)
AIC(m)

m1 <- glm(formula = crimes ~ (1 + density + area+prbarr), data = crimes.data)
cv(m);cv(m1)
AIC(m,m1)

m2 <- glm(formula = crimes ~ (1 + density + area+prbpris), data = crimes.data)
cv(m);cv(m1);cv(m2)
AIC(m,m1,m2)

m3 <- glm(formula = crimes ~ (1 + density + area+polpc), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3)
AIC(m,m1,m2,m3)

m4 <- glm(formula = crimes ~ (1 + density + area+taxpc), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4)
AIC(m,m1,m2,m3,m4)

m5 <- glm(formula = crimes ~ (1 + density + area+region), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4);cv(m5)
AIC(m,m1,m2,m3,m4,m5)

m6 <- glm(formula = crimes ~ (1 + density + area+pctmin), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4);cv(m5);cv(m6)
AIC(m,m1,m2,m3,m4,m5,m6)

m7 <- glm(formula = crimes ~ (1 + density + area+pctymale), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4);cv(m5);cv(m6);cv(m7)
AIC(m,m1,m2,m3,m4,m5,m6,m7)

m8 <- glm(formula = crimes ~ (1 + density + area+wcon), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4);cv(m5);cv(m6);cv(m7);cv(m8)
AIC(m,m1,m2,m3,m4,m5,m6,m7,m8)

m9 <- glm(formula = crimes ~ (1 + density + area+wsta), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4);cv(m5);cv(m6);cv(m7);cv(m8);cv(m9)
AIC(m,m1,m2,m3,m4,m5,m6,m7,m8,m9)

m10 <- glm(formula = crimes ~ (1 + density + area+wser), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4);cv(m5);cv(m6);cv(m7);cv(m8);cv(m9);cv(m10)
AIC(m,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)

m11 <- glm(formula = crimes ~ (1 + density + area+wtrd), data = crimes.data)
cv(m);cv(m1);cv(m2);cv(m3);cv(m4);cv(m5);cv(m6);cv(m7);cv(m8);cv(m9);cv(m10);cv(m11)
AIC(m,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11)

m12 <- glm(formula = crimes ~ (1 + density + area+wfir), data = crimes.data)
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

m3

#------------------------------------------------------------------------------------------------#
m <- glm(formula = crimes ~ (1 + density + area + polpc), data = crimes.data)
m1 <- glm(formula = crimes ~ (1 + density + area + polpc+prbarr), data = crimes.data)
m2 <- glm(formula = crimes ~ (1 + density + area + polpc+prbpris), data = crimes.data)
m3 <- glm(formula = crimes ~ (1 + density + area + polpc+taxpc), data = crimes.data)
m4 <- glm(formula = crimes ~ (1 + density + area + polpc+region), data = crimes.data)
m5 <- glm(formula = crimes ~ (1 + density + area + polpc+pctmin), data = crimes.data)
m6 <- glm(formula = crimes ~ (1 + density + area + polpc+pctymale), data = crimes.data)
m7 <- glm(formula = crimes ~ (1 + density + area + polpc+wcon), data = crimes.data)
m8 <- glm(formula = crimes ~ (1 + density + area + polpc+wsta), data = crimes.data)
m9 <- glm(formula = crimes ~ (1 + density + area + polpc+wser), data = crimes.data)
m10 <- glm(formula = crimes ~ (1 + density + area + polpc+wtrd), data = crimes.data)
m11 <- glm(formula = crimes ~ (1 + density + area + polpc+wfir), data = crimes.data)


a <- AIC(m,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11)$AIC
plot(a)
a
min(a)
#m10

dim2 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6),cv(m7),cv(m8),cv(m9),cv(m10),cv(m11))[1,]
dim2 <- dim2/max(dim2)
plot(dim2)
dim2
min(dim2)

m10
#------------------------------------------------------------------------------------------------#
m <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd), data = crimes.data)
m1 <-glm(formula = crimes ~ (1 + density + area + polpc + wtrd+prbarr), data = crimes.data)
m2 <-glm(formula = crimes ~ (1 + density + area + polpc + wtrd+prbpris), data = crimes.data)
m3 <-glm(formula = crimes ~ (1 + density + area + polpc + wtrd+taxpc), data = crimes.data)
m4 <-glm(formula = crimes ~ (1 + density + area + polpc + wtrd+region), data = crimes.data)
m5 <-glm(formula = crimes ~ (1 + density + area + polpc + wtrd+pctmin), data = crimes.data)
m6 <-glm(formula = crimes ~ (1 + density + area + polpc + wtrd+pctymale), data = crimes.data)
m7 <-glm(formula = crimes ~ (1 + density + area + polpc + wtrd+wcon), data = crimes.data)
m8 <-glm(formula = crimes ~ (1 + density + area + polpc + wtrd+wsta), data = crimes.data)
m9 <-glm(formula = crimes ~ (1 + density + area + polpc + wtrd+wfir), data = crimes.data)
m10 <-glm(formula = crimes ~ (1 + density + area + polpc + wtrd+wser), data = crimes.data)

a <- AIC(m,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)$AIC
plot(a)
a
min(a)
#m2

dim3 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6),cv(m7),cv(m8),cv(m9),cv(m10))[1,]
dim3 <- dim3/max(dim3)
plot(dim3)
dim3
min(dim3)
m2

#------------------------------------------------------------------------------------------------#
m <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris), data = crimes.data)
m1 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris+prbarr), 
          data = crimes.data)
m2 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris+taxpc), 
          data = crimes.data)
m3 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris+region), 
          data = crimes.data)
m4 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris+pctmin), 
          data = crimes.data)
m5 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris+pctymale), 
          data = crimes.data)
m6 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris+wcon), 
          data = crimes.data)
m7 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris+wsta), 
          data = crimes.data)
m8 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris+wser), 
          data = crimes.data)
m9 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris+wfir), 
          data = crimes.data)

a <- AIC(m,m1,m2,m3,m4,m5,m6,m7,m8,m9)$AIC
plot(a)
a
min(a)
#m4

dim4 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6),cv(m7),cv(m8),cv(m9))[1,]
dim4 <- dim4/max(dim4)
plot(dim4)
dim4
min(dim4)
m4

#------------------------------------------------------------------------------------------------#
m <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                               pctmin), data = crimes.data)
m1 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                               pctmin+prbarr), data = crimes.data)
m2 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin+taxpc), data = crimes.data)
m3 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin+taxpc), data = crimes.data)
m4 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin+region), data = crimes.data)
m5 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin+pctymale), data = crimes.data)
m6 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin+wcon), data = crimes.data)
m7 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin+wser), data = crimes.data)
m8 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin+wfir), data = crimes.data)

a <- AIC(m,m1,m2,m3,m4,m5,m6,m7,m8)$AIC
plot(a)
a
min(a)
#m4

dim5 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6),cv(m7),cv(m8))[1,]
dim5 <- dim5/max(dim5)
plot(dim5)
dim5
min(dim5)
m4
# kaum verbesserung bei cv.

#------------------------------------------------------------------------------------------------#
m <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                             pctmin + region), data = crimes.data)
m1 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                               pctmin + region+prbarr), data = crimes.data)
m2 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region+taxpc), data = crimes.data)
m3 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region+pctmin), data = crimes.data)
m4 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region+pctymale), data = crimes.data)
m5 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region+wcon), data = crimes.data)
m6 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region+wsta), data = crimes.data)
m7 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region+wser), data = crimes.data)
m8 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region+wfir), data = crimes.data)

a <- AIC(m,m1,m2,m3,m4,m5,m6,m7,m8)$AIC
plot(a)
a
min(a)
# auch hier keine weitere verbesserung durch nachfolgemodelle.
# evtl m3

dim6 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6),cv(m7),cv(m8))[1,]
dim6 <- dim6/max(dim6)
plot(dim6)
dim6
min(dim6)
m8

#------------------------------------------------------------------------------------------------#
m <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                               pctmin + region + wfir), data = crimes.data)
m1 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir+prbarr), data = crimes.data)
m2 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir+taxpc), data = crimes.data)
m3 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir+pctmin), data = crimes.data)
m4 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir+pctymale), data = crimes.data)
m5 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir+wcon), data = crimes.data)
m6 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir+wsta), data = crimes.data)
m7 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir+wser), data = crimes.data)

a <- AIC(m,m1,m2,m3,m4,m5,m6,m7)$AIC
plot(a)
a
min(a)
# y-werte in dem plot konvergieren immer mehr an 1.0
# evtl m6

dim7 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6),cv(m7))[1,]
dim7 <- dim7/max(dim7)
plot(dim7)
dim7
min(dim7)
m4

#------------------------------------------------------------------------------------------------#
m <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                               pctmin + region + wfir + pctymale), data = crimes.data)
m1 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir + pctymale+prbarr), data = crimes.data)
m2 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir + pctymale+taxpc), data = crimes.data)
m3 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir + pctymale+wcon), data = crimes.data)
m4 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir + pctymale+wsta), data = crimes.data)
m5 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir + pctymale+wser), data = crimes.data)
m6 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir + pctymale+wtrd), data = crimes.data)

a <- AIC(m,m1,m2,m3,m4,m5,m6)$AIC
plot(a)
a
min(a)
# aic-werte alle sehr nahe beieinander! 1609-1611! 
# => alle modelle hier sehr ähnlich
# m5

dim8 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5),cv(m6))[1,]
dim8 <- dim8/max(dim8)
plot(dim8)
dim8
min(dim8)
m5

#------------------------------------------------------------------------------------------------#
m <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                               pctmin + region + wfir + pctymale + wser), 
         data = crimes.data)
m1 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                               pctmin + region + wfir + pctymale + wser+prbarr), 
         data = crimes.data)
m2 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                               pctmin + region + wfir + pctymale + wser+taxpc), 
         data = crimes.data)
m3 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir + pctymale + wser+wcon), 
          data = crimes.data)
m4 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir + pctymale + wser+wsta), 
          data = crimes.data)
m5 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir + pctymale + wser+wtrd), 
          data = crimes.data)

a <- AIC(m,m1,m2,m3,m4,m5)$AIC
plot(a)
a
min(a)
# aic-werte alle noch näher beieinander! 1610-1611! 
# => alle modelle hier sehr ähnlich
# m5

dim9 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4),cv(m5))[1,]
dim9 <- dim9/max(dim9)
plot(dim9)
dim9
min(dim9)
m3

#------------------------------------------------------------------------------------------------#
m <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                               pctmin + region + wfir + pctymale + wser + wcon), 
         data = crimes.data)
m1 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir + pctymale + wser + wcon+prbarr), 
          data = crimes.data)
m2 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir + pctymale + wser + wcon+taxpc), 
          data = crimes.data)
m3 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir + pctymale + wser + wcon+wsta), 
          data = crimes.data)
m4 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                                pctmin + region + wfir + pctymale + wser + wcon+wfir), 
          data = crimes.data)

a <- AIC(m,m1,m2,m3,m4)$AIC
plot(a)
a
min(a)

dim10 <- cbind(cv(m),cv(m1),cv(m2),cv(m3),cv(m4))[1,]
dim10 <- dim10/max(dim10)
plot(dim10)
dim10
min(dim10)
# aic werte werden bei den modellen höher, wo der fehler geringer wird
# => die verbliebenen drei eingabevektoren haben 'keinen guten einfluss' auf die modellauswahl
# wfir hat scheinbar gar keinen einfluss auf die modellgüte:
# aic wir cv-wert sind beide gleich. beide sind in beiden anschauungen das minimum
m4


#------------------------------------------------------------------------------------------------#
m <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                             pctmin + region + wfir + pctymale + wser + wcon + wfir), 
         data = crimes.data)
m1 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                              pctmin + region + wfir + pctymale + wser + wcon + wfir+prbarr), 
         data = crimes.data)
m2 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                              pctmin + region + wfir + pctymale + wser + wcon + wfir+taxpc), 
          data = crimes.data)
m3 <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                              pctmin + region + wfir + pctymale + wser + wcon + wfir+wsta), 
          data = crimes.data)

a <- AIC(m,m1,m2,m3)$AIC
plot(a)
a
min(a)

dim11 <- cbind(cv(m),cv(m1),cv(m2),cv(m3))[1,]
dim11 <- dim11/max(dim11)
plot(dim11)
dim11
min(dim10)

# breche hier ab.
# die letzten vier werte sind:
# wsta taxpc prbarr

#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
# ich entscheide mich dazu, das modell zu nehmen, welches noch 'gesunde' werte hatte.
# das war relativ früh geschehen.
mSW <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris + 
                               pctmin + region + wfir + pctymale + wser), 
           data = crimes.data)
# begründung: 
# mit plot(dim8) kann man sehr gut sehen, dass die verbesserung um wser die letzte 'Richtige'
# verbesserung ist.
# die fehler-abstände werden immer geringer!
# so ist zu wählen, ab welchem punkt man sagt, dass der hinzugewinn nichtig genug ist.

# bereits bei dim6 gibt es keine bessere aic-werte. dies ist natürlich nicht ausschlaggebend,
# zumal da der cv-wert hier noch einen eindeutigen nächsten kandidaten bestimmen konnte
# bereits nach dim3 sind keine nennenswerten verbesserungen mehr entstanden

# sogar schon bei dim3 zu dim4 gibt es einen großen sprung in der fehlerabweichung
# so könnte auch schon 
mSW2 <- m <- glm(formula = crimes ~ (1 + density + area + polpc + wtrd + prbpris), data = crimes.data)
# als gutes modell genommen werden
cv(mSW2)
cv(m3O2)
