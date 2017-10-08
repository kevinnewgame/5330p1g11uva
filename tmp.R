
#### Best Subset ####

## total set
for (numPred in seq(NUM_PRED)) {
    comb <- combn(names(df), numPred, simplify = FALSE)
    
    for (pred in comb) {
        selectPred <- c(pred, "FE")
        df.in <- df[, which((names(df) %in% selectPred))]
        
        ## Leave-one-out CV
        
    }
}


## Leave-one-out CV
# y.hat <- vector("numeric", NUM_ROW)
# for (i in seq(NUM_ROW)) {
#     y.hat[i] <- predict(lm(FE ~ ., data = df[-i,]), df[i,])
# }
