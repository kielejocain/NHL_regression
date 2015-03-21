source("~/workspace/NHL_regression/R3/setup.R")
skaterstats <- nhlClean()
## you'll need gpModel4 from pred1_GP.R, as well as preds2013/4 to flesh out test models
GP2013 <- preds2013[, c(1, 8)]
names(GP2013)[2] <- "games_played.p"
GP2013 <- merge(GP2013, skaterstats[skaterstats$season == 2013, c(1, 3)])
GP2013[, 3] <- GP2013[, 3] * 48
GP2014 <- preds2014[, c(1, 8)]
names(GP2014)[2] <- "games_played.p"
GP2014 <- merge(GP2014, skaterstats[skaterstats$season == 2014, c(1, 3)])
GP2014[, 3] <- GP2014[, 3] * 82
# fitData <- nhlShape(2010, 2010, outcome = 42)
# esgFactors <- nhlAnalyze(fitData, fitData, seed = 492472)
# esgFactors
cols = c(1:2, 4, 15, 29:30, 32:34, 42, 46)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
controls <- list()
controls[[1]] <- fitControl
controls[[2]] <- fitControl
controls[[3]] <- fitControl
controls[[4]] <- fitControl
esgModel <- nhlModel(2013, 2013, outcome = 42, cols = cols, methods = c("rf", "gbm", "knn", "svmLinear"),
                     controls = controls, seed = 292255)
esgCorrs <- nhlCorr(2010, 2013, 42, esgModel)
esgModel2 <- nhlModel(2013, 2013, outcome = 42, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                     controls = controls, seed = 38563)
esgCorrs2 <- nhlCorr(2010, 2013, 42, esgModel2)
preds2013 <- nhlPredict(2012, 2012, esgModel2, outcome = 42)
preds2013 <- merge(GP2013[, c(1, 3)], preds2013)
preds2014 <- nhlPredict(2013, 2013, esgModel2, outcome = 42)
preds2014 <- merge(GP2014[, c(1, 3)], preds2014)
preds2013$rf[preds2013$rf > 1] <- 1
preds2013$gbm[preds2013$gbm > 1] <- 1
preds2013$svmLinear[preds2013$svmLinear > 1] <- 1
preds2013$cumulative[preds2013$cumulative > 1] <- 1
preds2013$outcome <- preds2013$outcome * preds2013$games_played
preds2013[, c(3:6, 8)] <- preds2013[, c(3:6, 8)] * 48
preds2013$mean <- (preds2013$rf + preds2013$gbm + preds2013$svmLinear) / 3
preds2014$rf[preds2014$rf > 1] <- 1
preds2014$gbm[preds2014$gbm > 1] <- 1
preds2014$svmLinear[preds2014$svmLinear > 1] <- 1
preds2014$cumulative[preds2014$cumulative > 1] <- 1
preds2014$outcome <- preds2014$outcome * preds2014$games_played
preds2014[, c(3:6, 8)] <- preds2014[, c(3:6, 8)] * 82
preds2014$mean <- (preds2014$rf + preds2014$gbm + preds2014$svmLinear) / 3
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: ES Goals, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("ES Goals in 2013") + ylab("ES Goals in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 ES Goals, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 ES Goals, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 ES Goals, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 ES Goals, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 ES Goals, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20141_naive.png")
print(plot14naive)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20142_rf.png")
print(plot14rf)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20143_gbm.png")
print(plot14gbm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20144_svm.png")
print(plot14svm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20145_cum.png")
print(plot14cum)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20146_mean.png")
print(plot14mean)
dev.off()
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/ESG/ESG2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: ES Goals, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("ES Goals in 2012") + ylab("ES Goals in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 ES Goals, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 ES Goals, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 ES Goals, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 ES Goals, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 ES Goals, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20131_naive.png")
print(plot13naive)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20132_rf.png")
print(plot13rf)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20133_gbm.png")
print(plot13gbm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20134_svm.png")
print(plot13svm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20135_cum.png")
print(plot13cum)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20136_mean.png")
print(plot13mean)
dev.off()
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/ESG/ESG2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()
preds2013 <- nhlPredict(2012, 2012, esgModel2, outcome = 42)
preds2014 <- nhlPredict(2013, 2013, esgModel2, outcome = 42)
preds2013 <- merge(GP2013, preds2013)
preds2014 <- merge(GP2014, preds2014)
for (i in c(4:7, 9)) {
      preds2013[, i] <- preds2013[, i] * preds2013$games_played.p
      preds2014[, i] <- preds2014[, i] * preds2014$games_played.p
}
preds2013$outcome <- preds2013$outcome * preds2013$games_played
preds2014$outcome <- preds2014$outcome * preds2014$games_played
preds2013$mean <- (preds2013$rf + preds2013$gbm + preds2013$svmLinear) / 3
preds2014$mean <- (preds2014$rf + preds2014$gbm + preds2014$svmLinear) / 3
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive2 <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: ES Goals, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("ES Goals in 2012") + ylab("ES Goals in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf2 <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 ES Goals, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm2 <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 ES Goals, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm2 <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 ES Goals, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum2 <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 ES Goals, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean2 <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 ES Goals, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20131_naive2.png")
print(plot13naive2)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20132_rf2.png")
print(plot13rf2)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20133_gbm2.png")
print(plot13gbm2)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20134_svm2.png")
print(plot13svm2)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20135_cum2.png")
print(plot13cum2)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20136_mean2.png")
print(plot13mean2)
dev.off()
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/ESG/ESG2013_full2.png")
grid.arrange(plot13naive2, plot13rf2, plot13gbm2, plot13svm2, plot13cum2, plot13mean2, ncol = 2)
dev.off()