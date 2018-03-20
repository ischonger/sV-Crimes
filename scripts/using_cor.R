cor(crimes.data$crimes, crimes.data$prbarr)
plot(crimes.data$crimes, crimes.data$prbarr)
# -0.2994541
cor(crimes.data$crimes, crimes.data$prbpris)
plot(crimes.data$crimes, crimes.data$prbpris)
# -0.008989058
cor(crimes.data$crimes, crimes.data$polpc)
plot(crimes.data$crimes, crimes.data$polpc)
# -0.008989058
cor(crimes.data$crimes, crimes.data$density)
plot(crimes.data$crimes, crimes.data$density)
# 0.9091849
cor(crimes.data$crimes, crimes.data$area)
plot(crimes.data$crimes, crimes.data$area)
# 0.1170337
cor(crimes.data$crimes, crimes.data$taxpc)
plot(crimes.data$crimes, crimes.data$taxpc)
# 0.2002482
cor(crimes.data$crimes, as.factor(crimes.data$region))
plot(crimes.data$crimes, crimes.data$region)
# ?
cor(crimes.data$crimes, crimes.data$pctmin)
plot(crimes.data$crimes, crimes.data$pctmin)
# 0.03044025
cor(crimes.data$crimes, crimes.data$pctymale)
plot(crimes.data$crimes, crimes.data$pctymale)
# 0.1690798
cor(crimes.data$crimes, crimes.data$wcon)
plot(crimes.data$crimes, crimes.data$wcon)
# 0.4683103
cor(crimes.data$crimes, crimes.data$wsta)
plot(crimes.data$crimes, crimes.data$wsta)
# 0.3094517
cor(crimes.data$crimes, crimes.data$wser)
plot(crimes.data$crimes, crimes.data$wser)
# 0.4995101
cor(crimes.data$crimes, crimes.data$wtrd)
plot(crimes.data$crimes, crimes.data$wtrd)
# 0.623725
cor(crimes.data$crimes, crimes.data$wfir)
plot(crimes.data$crimes, crimes.data$wfir)
# 0.5460246

corV <- c(-0.2994541, -0.008989058, -0.008989058, 0.9091849, 0.1170337, 0.2002482, 0.03044025, 
          0.03044025, 0.1690798, 0.4683103, 0.3094517, 0.4995101, 0.623725, 0.5460246)
plot(corV)
tail(sort(corV), 5)
mCor <- glm.nb(formula = crimes~(density+wser+wfir+wtrd+pctymale), data = crimes.data)
cv(mCor); AIC(mCor)
step(mCor, ~(density+wser+wfir+wtrd+pctymale)^2)
mCorO <- glm.nb(formula = crimes ~ density + wser + wfir + wtrd + pctymale + 
                  wfir:wtrd + density:wser, data = crimes.data, init.theta = 3.468214965, 
                link = log)
cv(mCor); cv(mCorO); cv(mR)
AIC(mCor, mCorO, m3Opt)
# ergebnis ähnlich gut wie das aus versuch 1.

######### bestes ergebnis: mCor
mCor


# for covariance matrix:
# https://stat.ethz.ch/pipermail/r-help/2007-June/135102.html
# more stuff:
# https://www.youtube.com/watch?v=bYSWm0lddMI