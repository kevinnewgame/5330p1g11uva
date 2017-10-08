dat <- data.6
windows()
pairs(dat)

## Relationship between CarClass <> EngineDisplacement,
# CarClass <> FE
par(mar = c(11, 4, 1, 2) + 0.1)

v1 <- with(data.0, reorder(CarlineClassDesc, EngDispl, median))
boxplot(data.0$EngDispl ~ v1, las = 2, xaxt = "n")
l <- levels(v1)
text(x = seq_along(l), y = par("usr")[3] - .6, srt = 45, adj = 1,
     labels = l, xpd = TRUE)

v2 <- with(data.0, reorder(CarlineClassDesc, FE, median))
boxplot(data.0$FE ~ v2, las = 2, xaxt = "n")
l <- levels(v2)
text(x = seq_along(l), y = par("usr")[3] - .6, srt = 45, adj = 1,
     labels = l, xpd = TRUE)

# histogram of CarClass
plot(v2, xaxt = "n", las = 2)
text(x = seq_along(l) * 1.15, y = par("usr")[3] - .6, srt = 45, adj = 1,
     labels = l, xpd = TRUE)


# Boxplot of FE
boxplot(dat$train$FE, main = "Boxplot: FE", ylab = "mpg")
