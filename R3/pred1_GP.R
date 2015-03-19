source("~/workspace/NHL_regression/R3/setup.R")
skaterstats <- nhlClean()
fitData <- nhlShape(2010, 2010, outcome = 3)
gpFactors <- nhlAnalyze(fitData, fitData, seed = 404508)
gpFactors
cols <- c(1:4, 30, 32, 38, 41, 46)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
controls <- list()
controls[[1]] <- fitControl
controls[[2]] <- fitControl
controls[[3]] <- fitControl
controls[[4]] <- fitControl
gpModel <- nhlModel(2010, 2010, outcome = 3, cols = cols, methods = c("rf", "gbm", "knn", "svmLinear"),
                    controls = controls, seed = 714537)
gpCorrs <- nhlCorr(2010, 2013, 3, gpModel)
gpModel2 <- nhlModel(2010, 2010, outcome = 3, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                     controls = controls, seed = 845856)
gpCorrs2 <- nhlCorr(2010, 2013, 3, gpModel2)
gpModel3 <- nhlModel(2013, 2013, outcome = 3, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                     controls = controls, seed = 244416)
gpCorrs3 <- nhlCorr(2010, 2013, 3, gpModel3)
gpModel4 <- nhlModel(2013, 2013, outcome = 3, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                     controls = controls, seed = 174896)
gpCorrs4 <- nhlCorr(2010, 2013, 3, gpModel4)
preds2013 <- nhlPredict(2012, 2012, gpModel4, outcome = 3)
preds2014 <- nhlPredict(2013, 2013, gpModel4, outcome = 3)
preds2013$rf <- preds2013$rf * 48
preds2013$gbm <- preds2013$gbm * 48
preds2013$svmLinear <- preds2013$svmLinear * 48
preds2013$cumulative <- preds2013$cumulative * 48
preds2013$outcome <- preds2013$outcome * 48
preds2013$naive <- preds2013$naive * 48
preds2013$mean <- (preds2013$rf + preds2013$gbm + preds2013$svmLinear) / 3
preds2014$rf <- preds2014$rf * 82
preds2014$gbm <- preds2014$gbm * 82
preds2014$svmLinear <- preds2014$svmLinear * 82
preds2014$cumulative <- preds2014$cumulative * 82
preds2014$outcome <- preds2014$outcome * 82
preds2014$naive <- preds2014$naive * 82
preds2014$mean <- (preds2014$rf + preds2014$gbm + preds2014$svmLinear) / 3
corr <- round(gpCorrs4["2014", "naive"], digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Games Played, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("Games Played in 2013") + ylab("Games Played in 2014")
corr <- round(gpCorrs4["2014", "rf"], digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Games Played, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Games Played") + ylab("Actual Games Played")
corr <- round(gpCorrs4["2014", "gbm"], digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Games Played, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Games Played") + ylab("Actual Games Played")
corr <- round(gpCorrs4["2014", "svmLinear"], digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Games Played, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Games Played") + ylab("Actual Games Played")
corr <- round(gpCorrs4["2014", "cumulative"], digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Games Played, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Games Played") + ylab("Actual Games Played")
corr <- round(gpCorrs4["2014", "mean"], digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Games Played, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Games Played") + ylab("Actual Games Played")
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20141_naive.png")
print(plot14naive)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20142_rf.png")
print(plot14rf)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20143_gbm.png")
print(plot14gbm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20144_svm.png")
print(plot14svm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20145_cum.png")
print(plot14cum)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20146_mean.png")
print(plot14mean)
dev.off()
library(gridExtra)
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/GP/GP2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
results2011 <- nhlPredict(2010, 2010, 3, gpModel)
corData <- nhlShape(2010, 2010, cols = cols, outcome = 3, rm.nhlnum = FALSE)
corData <- merge(corData, nhlPredict(2009, 2010, 3, gpTest))
corData$rf[corData$rf > 1] <- 1
corData$gbm[corData$gbm > 1] <- 1
corData$games_played[corData$games_played > 1] <- 1
corData$outcome <- corData$outcome * 82
corData$rf <- corData$rf * 82
corData$gbm <- corData$gbm * 82
corData$games_played <- corData$games_played * 82
cor(corData$outcome, corData$games_played.1)
cor(corData$outcome, corData$rf)
cor(corData$outcome, corData$gbm)
cor(corData$outcome, corData$games_played)
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20111_naive.png")
qplot(outcome, games_played.1, data = corData, geom = c("smooth", "point"),
      main = "Naive Model: Games Played, 2010 vs. 2011",
      xlab = "Games Played in 2010", ylab = "Games Played in 2011")
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20111_rf.png")
qplot(outcome, rf, data = corData, geom = c("smooth", "point"),
      main = "Games Played in 2011, Random Forest Model",
      xlab = "Predicted Games Played", ylab = "Actual Games Played")
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20113_gbm.png")
qplot(outcome, gbm, data = corData, geom = c("smooth", "point"),
      main = "Naive Model: Games Played in 2011, Boosting Model",
      xlab = "Predicted Games Played", ylab = "Actual Games Played")
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20114_joint.png")
qplot(outcome, games_played, data = corData, geom = c("smooth", "point"),
      main = "Naive Model: Games Played in 2011, Joint Model",
      xlab = "Predicted Games Played", ylab = "Actual Games Played")
dev.off()
output <- nhlPredict(2013, 2014, 3, gpModel)
dbWriteTable(conn, "skatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)