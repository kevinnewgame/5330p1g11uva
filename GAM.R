source("datCln.R")      # training data
source("logRMSE.R")     # cost function



#### First step fit ####
bestSub <- RESULT[[bestIndex]]
firstPred <- bestSub$predictor[!bestSub$predictor %in% c("EngDispl", "NumCyl")]
lm.fit <- lm(FE ~ ., dat$train[, c("FE", firstPred)])

y <- lm.fit$residuals
plot(dat$train$NumCyl, y)   # Don't need to fit GAM
