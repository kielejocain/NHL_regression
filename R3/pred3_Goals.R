## define custom functions, call data from the database
source("~/workspace/NHL_regression/R3/setup.R")

## clean the data, add position data from bio table
skaterstats <- nhlClean()
skaterstats <- merge(skaterstats, skaters[, 3:4])
skaterstats$player_position <- as.factor(skaterstats$player_position)

## fetch previous predictions
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
rs <- dbSendQuery(conn, statement = "SELECT * FROM newskatpred15
                  ORDER BY nhl_num;")
output <- fetch(rs, n = -1)
dbClearResult(rs)
dbDisconnect(conn)

## bring in data for games played in 2013-2014 to scale up the data to totals
## rather than per game numbers
source("~/workspace/NHL_regression/R3/GPsetup.R")

## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 42)
# factControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
# esgFactors.rf <- nhlAnalyze2(fitData, seed = 966489, importance = TRUE, trControl = factControl)
# esgFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 11052, trControl = factControl)
# esgFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 133840, trControl = factControl)
# esgFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 901012, trControl = factControl)
# esgFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 236226, trControl = factControl)

## use the importance output to select factors
cols <- list()
cols[["rf"]] <- c(1:2, 4:6, 9:10, 13, 15:16, 24:25, 29:34, 38:42, 45:47)
cols[["gbm"]] <- c(1:2, 4, 13, 15, 29:30, 32, 42, 46)
cols[["pls"]] <- c(1:2, 16, 33:34, 38:41)
cols[["knn"]] <- c(1:2, 4:6, 9:10, 13, 15:16, 25, 29:30, 32:34, 40, 42, 45:46)
cols[["svmLinear"]] <- c(1:2, 4:6, 9:10, 13, 15:16, 25, 29:30, 32:34, 40, 42, 45:46)

## build single models, ensemble the models, and look at correlations
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
controls <- list()
controls[[1]] <- fitControl
controls[[2]] <- fitControl
controls[[3]] <- fitControl
controls[[4]] <- fitControl
controls[[5]] <- fitControl
# esgModel <- nhlModel(2010, 2010, outcome = 42, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
#                      controls = controls, seed = 656903)
# esgCorrs <- nhlCorr(2010, 2013, 42, esgModel)
esgModel2 <- nhlModel(2010, 2010, outcome = 42, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                      controls = controls, seed = 377740)
esgCorrs2 <- nhlCorr(2010, 2013, 42, esgModel2)
# esgModel3 <- nhlModel(2013, 2013, outcome = 42, cols = cols, methods = c("rf", "gbm", "svmLinear"),
#                       controls = controls, seed = 106865)
# esgCorrs3 <- nhlCorr(2010, 2013, 42, esgModel3)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, esgModel2, outcome = 42)
preds2014 <- nhlPredict(2013, 2013, esgModel2, outcome = 42)
preds2013 <- merge(GP2013, preds2013)
preds2014 <- merge(GP2014, preds2014)
preds2013$rf[preds2013$rf < 0] <- 0
preds2013$gbm[preds2013$gbm < 0] <- 0
preds2013$svmLinear[preds2013$svmLinear < 0] <- 0
preds2013$cumulative[preds2013$cumulative < 0] <- 0
preds2014$rf[preds2014$rf < 0] <- 0
preds2014$gbm[preds2014$gbm < 0] <- 0
preds2014$svmLinear[preds2014$svmLinear < 0] <- 0
preds2014$cumulative[preds2014$cumulative < 0] <- 0
for (i in c(5:8)) {
      preds2013[, i] <- preds2013[, i] * preds2013$games_played.pred
      preds2014[, i] <- preds2014[, i] * preds2014$games_played.pred
}
preds2013$outcome <- preds2013$outcome * preds2013$games_played
preds2014$outcome <- preds2014$outcome * preds2014$games_played
preds2013$naive <- preds2013$naive * preds2013$games_played.prev / 82 * 48
preds2014$naive <- preds2014$naive * preds2014$games_played.prev / 48 * 82
preds2013$mean <- (preds2013$rf + preds2013$gbm + preds2013$svmLinear) / 3
preds2014$mean <- (preds2014$rf + preds2014$gbm + preds2014$svmLinear) / 3

## graphing
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: ES Goals, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("ES Goals in 2013 (Scaled)") + ylab("ES Goals in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 ES Goals, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 ES Goals, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 ES Goals, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 ES Goals, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 ES Goals, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/Goals/ESG2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: ES Goals, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("ES Goals in 2012 (Scaled)") + ylab("ES Goals in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 ES Goals, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 ES Goals, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 ES Goals, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 ES Goals, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 ES Goals, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/Goals/ESG2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting ESG for 2015
preds2015 <- nhlPredict(2014, 2014, esgModel2, outcome = 42)
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
output$es_goals <- preds2015$cumulative * preds2015$games_played

## Short-handed Goals
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 11)
# shgFactors.rf <- nhlAnalyze2(fitData, seed = 574248, importance = TRUE, trControl = factControl)
# shgFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 162414, trControl = factControl)
# shgFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 925306, trControl = factControl)
# shgFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 703743, trControl = factControl)
# shgFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 585725, trControl = factControl)

## use the importance output to select factors
cols[["rf"]] <- c(1:6, 9:11, 13, 15, 24:27, 29, 31:34, 38:42, 45:47)
cols[["gbm"]] <- c(1:2, 11, 15, 29, 32:33)
cols[["pls"]] <- c(1:2, 15, 16, 33:34, 38:41)
cols[["knn"]] <- c(1:2, 4, 6, 11, 13, 15:16, 27, 29:30, 32:34, 39, 42, 46)
cols[["svmLinear"]] <- c(1:2, 4, 11, 13, 15, 29, 32:34, 42)

## build single models, ensemble the models, and look at correlations
# shgModel <- nhlModel(2010, 2010, outcome = 11, cols = cols, methods = c("rf", "gbm", "knn", "pls", "svmLinear"),
#                      controls = controls, seed = 113970)
# shgCorrs <- nhlCorr(2010, 2013, 11, shgModel)
# shgModel2 <- nhlModel(2013, 2013, outcome = 11, cols = cols, methods = c("rf", "gbm", "knn", "pls"),
#                      controls = controls, seed = 824216)
# shgCorrs2 <- nhlCorr(2010, 2013, 11, shgModel2)
shgModel3 <- nhlModel(2013, 2013, outcome = 11, cols = cols, methods = c("rf", "gbm", "knn"),
                      controls = controls, seed = 447632)
shgCorrs3 <- nhlCorr(2010, 2013, 11, shgModel3)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, shgModel3, outcome = 11)
preds2014 <- nhlPredict(2013, 2013, shgModel3, outcome = 11)
preds2013 <- merge(GP2013, preds2013)
preds2014 <- merge(GP2014, preds2014)
preds2013$rf[preds2013$rf < 0] <- 0
preds2013$gbm[preds2013$gbm < 0] <- 0
preds2013$knn[preds2013$knn < 0] <- 0
preds2013$cumulative[preds2013$cumulative < 0] <- 0
preds2014$rf[preds2014$rf < 0] <- 0
preds2014$gbm[preds2014$gbm < 0] <- 0
preds2014$knn[preds2014$knn < 0] <- 0
preds2014$cumulative[preds2014$cumulative < 0] <- 0
for (i in c(5:8)) {
      preds2013[, i] <- preds2013[, i] * preds2013$games_played.pred
      preds2014[, i] <- preds2014[, i] * preds2014$games_played.pred
}
preds2013$outcome <- preds2013$outcome * preds2013$games_played
preds2014$outcome <- preds2014$outcome * preds2014$games_played
preds2013$naive <- preds2013$naive * preds2013$games_played.prev / 82 * 48
preds2014$naive <- preds2014$naive * preds2014$games_played.prev / 48 * 82
preds2013$mean <- (preds2013$rf + preds2013$gbm + preds2013$knn) / 3
preds2014$mean <- (preds2014$rf + preds2014$gbm + preds2014$knn) / 3

## graphing
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: SH Goals, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("SH Goals in 2013 (Scaled)") + ylab("SH Goals in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 SH Goals, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Goals") + ylab("Actual SH Goals")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 SH Goals, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Goals") + ylab("Actual SH Goals")
corr <- round(cor(preds2014$outcome, preds2014$knn), digits = 4)
plot14knn <- ggplot(preds2014, aes(x=knn, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 SH Goals, k-Nearest Neighbors Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Goals") + ylab("Actual SH Goals")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 SH Goals, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Goals") + ylab("Actual SH Goals")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 SH Goals, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Goals") + ylab("Actual SH Goals")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/Goals/SHG2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14knn, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: SH Goals, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("SH Goals in 2012 (Scaled)") + ylab("SH Goals in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 SH Goals, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Goals") + ylab("Actual SH Goals")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 SH Goals, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Goals") + ylab("Actual SH Goals")
corr <- round(cor(preds2013$outcome, preds2013$knn), digits = 4)
plot13knn <- ggplot(preds2013, aes(x=knn, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 SH Goals, k-Nearest Neighbors Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Goals") + ylab("Actual SH Goals")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 SH Goals, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Goals") + ylab("Actual SH Goals")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 SH Goals, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Goals") + ylab("Actual SH Goals")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/Goals/SHG2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13knn, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting SHG for 2015
preds2015 <- nhlPredict(2014, 2014, shgModel3, outcome = 11)
# preds2015$rf[preds2015$rf < 0] <- 0
# preds2015$gbm[preds2015$gbm < 0] <- 0
# preds2015$knn[preds2015$knn < 0] <- 0
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
# preds2015[, 3:5] <- preds2015[, 3:5] * preds2015$games_played
# preds2015$sh_goals <- (preds2015$rf + preds2015$gbm + preds2015$knn) / 3
preds2015$sh_goals <- preds2015$cumulative * preds2015$games_played
output <- merge(output, preds2015[, c(1, 9)])

## Power Play Goals
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 9)
# ppgFactors.rf <- nhlAnalyze2(fitData, seed = 782497, importance = TRUE, trControl = factControl)
# ppgFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 203047, trControl = factControl)
# ppgFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 256806, trControl = factControl)
# ppgFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 785346, trControl = factControl)
# ppgFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 809034, trControl = factControl)

## use the importance output to select factors
cols[["rf"]] <- c(1:2, 4:6, 9:10, 15:16, 24:25, 29:30, 32, 38:43, 45:47)
cols[["gbm"]] <- c(1:2, 4, 6, 9, 15, 40)
cols[["pls"]] <- c(1:2, 9, 16, 38:41)
cols[["knn"]] <- c(1:2, 4:6, 9:10, 15, 24:25, 30, 40, 42:43, 46)
cols[["svmLinear"]] <- c(1:2, 4:6, 9:10, 13, 15, 24:25, 30, 40, 42:43, 45:46)

## build single models, ensemble the models, and look at correlations
# ppgModel <- nhlModel(2010, 2010, outcome = 9, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
#                       controls = controls, seed = 101142)
# ppgCorrs <- nhlCorr(2010, 2013, 9, ppgModel)
# ppgModel2 <- nhlModel(2013, 2013, outcome = 9, cols = cols, methods = c("rf", "gbm", "pls", "svmLinear"),
#                      controls = controls, seed = 429596)
# ppgCorrs2 <- nhlCorr(2010, 2013, 9, ppgModel2)
ppgModel3 <- nhlModel(2013, 2013, outcome = 9, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                      controls = controls, seed = 206107)
ppgCorrs3 <- nhlCorr(2010, 2013, 9, ppgModel3)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, ppgModel3, outcome = 9)
preds2014 <- nhlPredict(2013, 2013, ppgModel3, outcome = 9)
preds2013 <- merge(GP2013, preds2013)
preds2014 <- merge(GP2014, preds2014)
preds2013$rf[preds2013$rf < 0] <- 0
preds2013$gbm[preds2013$gbm < 0] <- 0
preds2013$svmLinear[preds2013$svmLinear < 0] <- 0
preds2013$cumulative[preds2013$cumulative < 0] <- 0
preds2014$rf[preds2014$rf < 0] <- 0
preds2014$gbm[preds2014$gbm < 0] <- 0
preds2014$svmLinear[preds2014$svmLinear < 0] <- 0
preds2014$cumulative[preds2014$cumulative < 0] <- 0
for (i in c(5:8)) {
      preds2013[, i] <- preds2013[, i] * preds2013$games_played.pred
      preds2014[, i] <- preds2014[, i] * preds2014$games_played.pred
}
preds2013$outcome <- preds2013$outcome * preds2013$games_played
preds2014$outcome <- preds2014$outcome * preds2014$games_played
preds2013$naive <- preds2013$naive * preds2013$games_played.prev / 82 * 48
preds2014$naive <- preds2014$naive * preds2014$games_played.prev / 48 * 82
preds2013$mean <- (preds2013$rf + preds2013$gbm + preds2013$svmLinear) / 3
preds2014$mean <- (preds2014$rf + preds2014$gbm + preds2014$svmLinear) / 3

## graphing
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: PP Goals, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("PP Goals in 2013 (Scaled)") + ylab("PP Goals in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 PP Goals, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 PP Goals, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 PP Goals, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 PP Goals, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 PP Goals, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/Goals/PPG2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: PP Goals, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("PP Goals in 2012 (Scaled)") + ylab("PP Goals in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 PP Goals, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 PP Goals, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 PP Goals, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 PP Goals, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 PP Goals, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/Goals/PPG2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting PPG for 2015
preds2015 <- nhlPredict(2014, 2014, ppgModel3, outcome = 9)
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
# preds2015[, 3:5] <- preds2015[, 3:5] * preds2015$games_played
# preds2015$pp_goals <- (preds2015$rf + preds2015$gbm + preds2015$svmLinear) / 3
preds2015$pp_goals <- preds2015$cumulative * preds2015$games_played
output <- merge(output, preds2015[, c(1, 9)])
output$goals <- rowSums(output[, 7:9])

## printing output to database
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
dbWriteTable(conn, "newskatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)