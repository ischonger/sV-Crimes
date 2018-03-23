mDensity <- glm.nb(crimes~density, data = crimes.data)

#acm <- vcov(mDensity)
mv <- c(1:14) # vector of the mean values of the columns of sC
sC.matrix <- matrix(unlist(sC), ncol = 16, byrow = FALSE) # sC as a matrix
sC.matrix <- sC.matrix[,-16] <- sC.matrix[,-8]
#for(col in 1:14) {
#  mv[col] <- mean(sC.matrix[,col])
#}
#mv
# create means for each column
#sC_mean <- matrix(data=1, nrow=nrow(sC.matrix)) %*% t(matrix(mv)) 
# creates a difference matrix sCd: sC_difference
#sC.matrix <- sC.matrix[,-1]
#sCd <- sC.matrix - sC_mean
#creates the covariance matrix
#sC.cov <- (nrow(sC.matrix)-1)^-1 %*% t(sCd) %*% sCd
