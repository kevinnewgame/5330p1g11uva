#### Description: ####
# Best subset in first order

library(boot)
source("datCln.R")      # training data


# Chars -> List(Chars)
# Make every possible predictor combination
makeAllPredComb <- function(allPred) {
    numPred <- length(allPred)
    
    allPredComb <- list()   # container
    for (np in seq(numPred)) {
        predComb <- combn(allPred, np, simplify = FALSE)
        allPredComb <- c(allPredComb, predComb)
    }
    return(allPredComb)
}


## Class: lmCV ####
# Numeric, Chars, Numeric -> lmCV
# Make class: constructor-lmCV
lmCV <- function(logRMSE, df, predictors, nPred) {
    list(logRMSE = logRMSE, 
         df.res = df,
         predictor = predictors,
         numPred = nPred) -> info
    class(info) <- "lmCV"
    return(info)
}


# Chars(predictor) -> lmCV
# Return class lmCV: logRMSE, predictor, df
cvGlm <- function(preds) {
    # data in use
    dat <- dat$train[, which(names(dat$train) %in% c("logFE", preds))]
    # main
    glm.fit <- glm(logFE ~ ., data = dat)
    cv.err <- cv.glm(dat, glm.fit, K = 10)
    # information for model selection by CV
    lmCV(logRMSE = sqrt(cv.err$delta[1]),
         df = glm.fit$df.residual, 
         predictors =  names(dat)[names(dat) != "logFE"],
         nPred = sum(names(dat) != "logFE")
    ) -> out
    return(out)
}
# # test:
# lmCV.test <- cvGlm(c("EngDispl", "NumCyl"))



# Safe code
safeCode <- function() {
    answer <- sample(1000:9999, 1)
    n <- readline(prompt = paste("Enter", answer, "to continue: "))
    if (as.numeric(n) != answer) {
        stop("Wrong answer! The program stop.")
    }
}


# prinf function
printf <- function(...) cat(sprintf(...))


# List of Chars -> List of lmCV
# Get CV information of linear models
getCVlmResult <- function(set) {
    # Safety question to avoid rewrite the previous result
    safeCode()
    
    return(lapply(set, cvGlm))
}


# lmCV -> lmCV
# Remove "predictor" from lmCV
rmPred <- function(lmcv) {
    lmcv[["predictor"]] <- NULL
    return(lmcv)
}


# List of lmCV -> data.frame
# Make a dataframe without information of predictor
CVresult2df <- function(locv) {
    locv <- lapply(locv, rmPred)
    locv <- lapply(locv, unclass)
    locv <- lapply(locv, data.frame)
    out <- do.call(rbind, locv)
    return(out)
}



#### Data ####
# Using data: dat$train
# predictor set
allPred <- names(dat$train)[!names(dat$train) %in% "logFE"]
PRED_SET <- allPred[!allPred %in% c("weightLevel")]    # Drop



#### Main ####

# ### Generate best subset
# set.seed(15)
# ## Calculating logRMSE and save data
# # Make all possible combination in predictor set
# allPredComb <- makeAllPredComb(PRED_SET)
# ## CV Results
# system.time(f10cvlm.2 <- getCVlmResult(allPredComb))    # 5.4mins
# save(f10cvlm.2, file = "f10cvlm_2.RData")

### The best subset
RESULT <- CVresults.1
# Transform to data.frame for plots
lmCV.df <- CVresult2df(RESULT)
# The best subset predictor:
bestIndex <- which.min(sapply(RESULT, function(lmCV) lmCV[["logRMSE"]]))
RESULT[[bestIndex]]

PRED_SET

### Plot: model complexity vs. logRMSE
plot(logRMSE ~ df.res, data = lmCV.df, 
     main = "10-fold CV",
     ylab = "logRMSE",
     xlab = "Residual Degrees of Freedom")




# Estimating test error by 100 times 10-fold cv
SELECT_PRED <- RESULT[[bestIndex]]$predictor

tmp <- rep(list(SELECT_PRED), 100)
TEST_ERROR <- lapply(tmp, cvGlm)
tmp <- sapply(TEST_ERROR, function(l) l[["logRMSE"]])
boxplot(tmp)
mean(tmp)

# Future prediction
tmp <- dat$train[which(names(dat$train) %in% c("logFE", SELECT_PRED))]
SELECT_LM <- lm(logFE ~ ., tmp)

tmp <- dat$test[which(names(dat$test) %in% SELECT_PRED)]
PREDICTION <- exp(predict(SELECT_LM, tmp))
plot(sort(PREDICTION))


