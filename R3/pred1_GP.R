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
# gpModel <- nhlModel(2010, 2010, outcome = 3, cols = cols, methods = c("rf", "gbm", "knn", "svmLinear"),
#                     controls = controls, seed = 714537)
# gpCorrs <- nhlCorr(2010, 2013, 3, gpModel)
# gpModel2 <- nhlModel(2010, 2010, outcome = 3, cols = cols, methods = c("rf", "gbm", "svmLinear"),
#                      controls = controls, seed = 845856)
# gpCorrs2 <- nhlCorr(2010, 2013, 3, gpModel2)
# gpModel3 <- nhlModel(2013, 2013, outcome = 3, cols = cols, methods = c("rf", "gbm", "svmLinear"),
#                      controls = controls, seed = 244416)
# gpCorrs3 <- nhlCorr(2010, 2013, 3, gpModel3)
gpModel4 <- nhlModel(2013, 2013, outcome = 3, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                     controls = controls, seed = 174896)
gpCorrs4 <- nhlCorr(2010, 2013, 3, gpModel4)
preds2013 <- nhlPredict(2012, 2012, gpModel4, outcome = 3)
preds2014 <- nhlPredict(2013, 2013, gpModel4, outcome = 3)
preds2013$rf[preds2013$rf > 1] <- 1
preds2013$gbm[preds2013$gbm > 1] <- 1
preds2013$svmLinear[preds2013$svmLinear > 1] <- 1
preds2013$cumulative[preds2013$cumulative > 1] <- 1
preds2013[, -1] <- preds2013[, -1] * 48
preds2013$mean <- (preds2013$rf + preds2013$gbm + preds2013$svmLinear) / 3
preds2014$rf[preds2014$rf > 1] <- 1
preds2014$gbm[preds2014$gbm > 1] <- 1
preds2014$svmLinear[preds2014$svmLinear > 1] <- 1
preds2014$cumulative[preds2014$cumulative > 1] <- 1
preds2014[, -1] <- preds2014[, -1] * 82
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
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/GP/GP2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(gpCorrs4["2013", "naive"], digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Games Played, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("Games Played in 2012") + ylab("Games Played in 2013")
corr <- round(gpCorrs4["2013", "rf"], digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Games Played, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Games Played") + ylab("Actual Games Played")
corr <- round(gpCorrs4["2013", "gbm"], digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Games Played, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Games Played") + ylab("Actual Games Played")
corr <- round(gpCorrs4["2013", "svmLinear"], digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Games Played, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Games Played") + ylab("Actual Games Played")
corr <- round(gpCorrs4["2013", "cumulative"], digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Games Played, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Games Played") + ylab("Actual Games Played")
corr <- round(gpCorrs4["2013", "mean"], digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Games Played, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Games Played") + ylab("Actual Games Played")
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20131_naive.png")
print(plot13naive)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20132_rf.png")
print(plot13rf)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20133_gbm.png")
print(plot13gbm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20134_svm.png")
print(plot13svm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20135_cum.png")
print(plot13cum)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20136_mean.png")
print(plot13mean)
dev.off()
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/GP/GP2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()
preds2015 <- nhlPredict(2014, 2014, gpModel4, outcome = 3)
preds2015$rf <- preds2015$rf * 82
preds2015$gbm <- preds2015$gbm * 82
preds2015$svmLinear <- preds2015$svmLinear * 82
preds2015$games_played <- (preds2015$rf + preds2015$gbm + preds2015$svmLinear) / 3
preds2015$games_played[preds2015$games_played > 82] <- 82
output <- pred2015[, c("nhl_num", "games_played")]
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
dbWriteTable(conn, "newskatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)