### cross validation ###

## compare singles ##
cross_validation_singles <- function(repeats = 10) {
  index <- rep(1:7, each = 6)
  index <- index[-(dim(crimes.data)[1]+1)]
  index <- sample(index)
  
  SPSE1 <- SPSE2 <- SPSE3 <- SPSE4 <- SPSE5 <- SPSE6 <- SPSE7 <- SPSE8 <- SPSE9 <- SPSE10 <- SPSE11 <- SPSE12 <- SPSE13 <- SPSE14 <- 0
  
  for(i in 1:repeats) {
    crimes.test <- crimes.data[index==i,]
    crimes.train <- crimes.data[-index!=i,]
    
    # schätzung der parameter
    mPrbarr <- glm(normY~(1+prbarr), data = crimes.data)
    mPrbpris <- glm(normY~(1+prbpris), data = crimes.data)
    mPolpc <- glm(normY~(1+prbpris), data = crimes.data)
    mDensity <- glm(normY~(1+density), data = crimes.data)
    mArea <- glm(normY~(1+area), data = crimes.data)
    mTaxpc <- glm(normY~(1+taxpc), data = crimes.data)
    mRegion <- glm(normY~(1+region), data = crimes.data)
    mPctmin <- glm(normY~(1+pctmin), data = crimes.data)
    mPctymale <- glm(normY~(1+pctymale), data = crimes.data)
    mWcon <- glm(normY~(1+wcon), data = crimes.data)
    mWsta <- glm(normY~(1+wsta), data = crimes.data)
    mWser <- glm(normY~(1+wser), data = crimes.data)
    mWtrd <- glm(normY~(1+wtrd), data = crimes.data)
    mWfir <- glm(normY~(1+wfir), data = crimes.data)
    
    
    # schätzung des prognosefehlers
    SPSE1 <- SPSE1 + sum((crimes.test$crimes - predict(lm1, newdata=crimes.test))^2)
    SPSE2 <- SPSE2 + sum((crimes.test$crimes - predict(lm2, newdata=crimes.test))^2)
    SPSE3 <- SPSE3 + sum((crimes.test$crimes - predict(lm3, newdata=crimes.test))^2)
    SPSE4 <- SPSE4 + sum((crimes.test$crimes - predict(lm4, newdata=crimes.test))^2)
    SPSE5 <- SPSE5 + sum((crimes.test$crimes - predict(lm5, newdata=crimes.test))^2)
    SPSE6 <- SPSE6 + sum((crimes.test$crimes - predict(lm5, newdata=crimes.test))^2)
    SPSE7 <- SPSE7 + sum((crimes.test$crimes - predict(lm5, newdata=crimes.test))^2)
    SPSE8 <- SPSE8 + sum((crimes.test$crimes - predict(lm5, newdata=crimes.test))^2)
    SPSE9 <- SPSE9 + sum((crimes.test$crimes - predict(lm5, newdata=crimes.test))^2)
    SPSE10 <- SPSE10 + sum((crimes.test$crimes - predict(lm5, newdata=crimes.test))^2)
    SPSE11 <- SPSE11 + sum((crimes.test$crimes - predict(lm5, newdata=crimes.test))^2)
    SPSE12 <- SPSE12 + sum((crimes.test$crimes - predict(lm5, newdata=crimes.test))^2)
    SPSE13 <- SPSE13 + sum((crimes.test$crimes - predict(lm5, newdata=crimes.test))^2)
    SPSE14 <- SPSE14 + sum((crimes.test$crimes - predict(lm5, newdata=crimes.test))^2)
  }
  
  cbind(SPSE1, SPSE2, SPSE3, SPSE4, SPSE5, SPSE6, SPSE7, SPSE8, SPSE9, SPSE10, SPSE11, SPSE12, SPSE13, SPSE14)
}

cross_validation_singles()
cvs <- cross_validation_singles(1)
plot(cross_validation_singles())

## mDensity ##
cv <- function(model, repeats = 10) {
  index <- rep(1:7, each = 6)
  index <- index[-(dim(crimes.data)[1]+1)]
  index <- sample(index)
  SPSE1 <- 0
  
  for(i in 1:repeats) {
    crimes.test <- crimes.data[index==i,]
    crimes.train <- crimes.data[-index!=i,]
    
    # schätzung der parameter
    #model
    
    # schätzung des prognosefehlers
    SPSE1 <- SPSE1 + sum((crimes.test$crimes - predict(model, newdata=crimes.test))^2)
  }
  
  SPSE1
}
