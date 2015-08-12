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

## Even Strength Assists
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 45)
# factControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
# esaFactors.rf <- nhlAnalyze2(fitData, seed = 208335, importance = TRUE, trControl = factControl)
# esaFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 273079, trControl = factControl)
# esaFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 582900, trControl = factControl)
# esaFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 865713, trControl = factControl)
# esaFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 125759, trControl = factControl)

## use the importance output to select factors
cols <- list()
cols[["rf"]] <- c(1:2, 4:6, 15, 24:25, 30, 32, 38:42, 45:46)
cols[["gbm"]] <- c(1:2, 5:6, 15, 24, 30, 46)
cols[["pls"]] <- c(1:2, 16, 33:34, 38:41)
cols[["knn"]] <- c(1:2, 4:6, 9:10, 13, 15, 24:25, 30, 32, 38, 40:43, 45:46)
cols[["svmLinear"]] <- c(1:2, 4:6, 9:10, 13, 15, 24:25, 30, 32, 38, 40:43, 45:46)

## build single models, ensemble the models, and look at correlations
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
controls <- list()
controls[[1]] <- fitControl
controls[[2]] <- fitControl
controls[[3]] <- fitControl
controls[[4]] <- fitControl
controls[[5]] <- fitControl
# esaModel <- nhlModel(2010, 2010, outcome = 45, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
#                      controls = controls, seed = 195784)
# esaCorrs <- nhlCorr(2010, 2013, 45, esaModel)
# esaModel2 <- nhlModel(2010, 2010, outcome = 45, cols = cols, methods = c("rf", "gbm", "pls", "svmLinear"),
#                      controls = controls, seed = 897306)
# esaCorrs2 <- nhlCorr(2010, 2013, 45, esaModel2)
esaModel3 <- nhlModel(2010, 2010, outcome = 45, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                      controls = controls, seed = 252443)
esaCorrs3 <- nhlCorr(2010, 2013, 45, esaModel3)
# esaModel4 <- nhlModel(2013, 2013, outcome = 45, cols = cols, methods = c("rf", "gbm", "svmLinear"),
#                       controls = controls, seed = 113097)
# esaCorrs4 <- nhlCorr(2010, 2013, 45, esaModel4)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, esaModel3, outcome = 45)
preds2014 <- nhlPredict(2013, 2013, esaModel3, outcome = 45)
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
preds2013$mean <- rowMeans(preds2013[,  5:7])
preds2014$mean <- rowMeans(preds2014[,  5:7])

## graphing
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: ES Assists, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("ES Assists in 2013 (Scaled)") + ylab("ES Assists in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 ES Assists, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Assists") + ylab("Actual ES Assists")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 ES Assists, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Assists") + ylab("Actual ES Assists")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 ES Assists, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Assists") + ylab("Actual ES Assists")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 ES Assists, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Assists") + ylab("Actual ES Assists")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 ES Assists, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Assists") + ylab("Actual ES Assists")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/Assists/ESA2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: ES Assists, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("ES Assists in 2012 (Scaled)") + ylab("ES Assists in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 ES Assists, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Assists") + ylab("Actual ES Assists")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 ES Assists, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Assists") + ylab("Actual ES Assists")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 ES Assists, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Assists") + ylab("Actual ES Assists")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 ES Assists, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Assists") + ylab("Actual ES Assists")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 ES Assists, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Assists") + ylab("Actual ES Assists")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/Assists/ESA2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting ESA for 2015
preds2015 <- nhlPredict(2014, 2014, esaModel3, outcome = 45)
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
output$es_assists <- preds2015$cumulative * preds2015$games_played

## Short-handed Assists
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 44)
# shaFactors.rf <- nhlAnalyze2(fitData, seed = 985813, importance = TRUE, trControl = factControl)
# shaFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 570838, trControl = factControl)
# shaFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 234518, trControl = factControl)
# shaFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 309764, trControl = factControl)
# shaFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 854736, trControl = factControl)

## use the importance output to select factors
cols[["rf"]] <- c(1:6, 8, 10, 24, 26:30, 32, 39:46)
cols[["gbm"]] <- c(1:3, 19, 24, 26:28, 35, 39)
cols[["pls"]] <- c(1:2, 38:41)
cols[["knn"]] <- c(1:2, 5, 7:8, 27:28, 32, 34:35, 39)
cols[["svmLinear"]] <- c(1:3, 5, 7, 25, 27:28, 35, 39)

## build single models, ensemble the models, and look at correlations
# shaModel <- nhlModel(2010, 2010, outcome = 44, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
#                      controls = controls, seed = 598534)
# shaCorrs <- nhlCorr(2010, 2013, 44, shaModel)
# shaModel2 <- nhlModel(2011, 2011, outcome = 44, cols = cols, methods = c("rf", "gbm", "pls", "svmLinear"),
#                      controls = controls, seed = 443613)
# shaCorrs2 <- nhlCorr(2010, 2013, 44, shaModel2)
# shaModel3 <- nhlModel(2013, 2013, outcome = 44, cols = cols, methods = c("rf", "gbm", "pls", "svmLinear"),
#                       controls = controls, seed = 252443)
# shaCorrs3 <- nhlCorr(2010, 2013, 44, shaModel3)
shaModel4 <- nhlModel(2012, 2012, outcome = 44, cols = cols, methods = c("rf", "gbm", "pls"),
                      controls = controls, seed = 113097)
shaCorrs4 <- nhlCorr(2010, 2013, 44, shaModel4)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, shaModel4, outcome = 44)
preds2014 <- nhlPredict(2013, 2013, shaModel4, outcome = 44)
preds2013 <- merge(GP2013, preds2013)
preds2014 <- merge(GP2014, preds2014)
preds2013$rf[preds2013$rf < 0] <- 0
preds2013$gbm[preds2013$gbm < 0] <- 0
preds2013$pls[preds2013$pls < 0] <- 0
preds2013$cumulative[preds2013$cumulative < 0] <- 0
preds2014$rf[preds2014$rf < 0] <- 0
preds2014$gbm[preds2014$gbm < 0] <- 0
preds2014$pls[preds2014$pls < 0] <- 0
preds2014$cumulative[preds2014$cumulative < 0] <- 0
for (i in c(5:8)) {
      preds2013[, i] <- preds2013[, i] * preds2013$games_played.pred
      preds2014[, i] <- preds2014[, i] * preds2014$games_played.pred
}
preds2013$outcome <- preds2013$outcome * preds2013$games_played
preds2014$outcome <- preds2014$outcome * preds2014$games_played
preds2013$naive <- preds2013$naive * preds2013$games_played.prev / 82 * 48
preds2014$naive <- preds2014$naive * preds2014$games_played.prev / 48 * 82
preds2013$mean <- rowMeans(preds2013[,  5:7])
preds2014$mean <- rowMeans(preds2014[,  5:7])

## graphing
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: SH Assists, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("SH Assists in 2013 (Scaled)") + ylab("SH Assists in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 SH Assists, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Assists") + ylab("Actual SH Assists")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 SH Assists, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Assists") + ylab("Actual SH Assists")
corr <- round(cor(preds2014$outcome, preds2014$pls), digits = 4)
plot14pls <- ggplot(preds2014, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 SH Assists, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Assists") + ylab("Actual SH Assists")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 SH Assists, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Assists") + ylab("Actual SH Assists")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 SH Assists, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Assists") + ylab("Actual SH Assists")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/Assists/SHA2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14pls, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: SH Assists, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("SH Assists in 2012 (Scaled)") + ylab("SH Assists in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 SH Assists, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Assists") + ylab("Actual SH Assists")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 SH Assists, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Assists") + ylab("Actual SH Assists")
corr <- round(cor(preds2013$outcome, preds2013$pls), digits = 4)
plot13pls <- ggplot(preds2013, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 SH Assists, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Assists") + ylab("Actual SH Assists")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 SH Assists, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Assists") + ylab("Actual SH Assists")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 SH Assists, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted SH Assists") + ylab("Actual SH Assists")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/Assists/SHA2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13pls, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting SHA for 2015
preds2015 <- nhlPredict(2014, 2014, shaModel4, outcome = 44)
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$pls[preds2015$pls < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
preds2015[, 3:5] <- preds2015[, 3:5] * preds2015$games_played
output$sh_assists <- rowMeans(preds2015[, 3:5])

## Power Play Assists
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 43)
# ppaFactors.rf <- nhlAnalyze2(fitData, seed = 369289, importance = TRUE, trControl = factControl)
# ppaFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 242141, trControl = factControl)
# ppaFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 187146, trControl = factControl)
# ppaFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 90151, trControl = factControl)
# ppaFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 868706, trControl = factControl)

## use the importance output to select factors
cols[["rf"]] <- c(1:6, 9:10, 15, 24:25, 30, 32, 38:43)
cols[["gbm"]] <- c(1:2, 5, 10, 24:25, 40, 43)
cols[["pls"]] <- c(1:2, 16, 38:41)
cols[["knn"]] <- c(1:2, 4:6, 9:10, 13, 15, 24:25, 30:32, 38, 40:41, 43, 45:46)
cols[["svmLinear"]] <- c(1:2, 4:6, 9:10, 13, 15, 24:25, 30:32, 38, 40:41, 43, 45:46)

## build single models, ensemble the models, and look at correlations
# ppaModel <- nhlModel(2010, 2010, outcome = 43, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
#                      controls = controls, seed = 669753)
# ppaCorrs <- nhlCorr(2010, 2013, 43, ppaModel)
# ppaModel2 <- nhlModel(2013, 2013, outcome = 43, cols = cols, methods = c("rf", "gbm", "pls", "svmLinear"),
#                      controls = controls, seed = 85279)
# ppaCorrs2 <- nhlCorr(2010, 2013, 43, ppaModel2)
ppaModel3 <- nhlModel(2010, 2010, outcome = 43, cols = cols, methods = c("rf", "pls", "svmLinear"),
                     controls = controls, seed = 363317)
ppaCorrs3 <- nhlCorr(2010, 2013, 43, ppaModel3)
# ppaModel4 <- nhlModel(2013, 2013, outcome = 43, cols = cols, methods = c("rf", "pls", "svmLinear"),
#                       controls = controls, seed = 678321)
# ppaCorrs4 <- nhlCorr(2010, 2013, 43, ppaModel4)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, ppaModel3, outcome = 43)
preds2014 <- nhlPredict(2013, 2013, ppaModel3, outcome = 43)
preds2013 <- merge(GP2013, preds2013)
preds2014 <- merge(GP2014, preds2014)
preds2013$rf[preds2013$rf < 0] <- 0
preds2013$pls[preds2013$pls < 0] <- 0
preds2013$svmLinear[preds2013$svmLinear < 0] <- 0
preds2013$cumulative[preds2013$cumulative < 0] <- 0
preds2014$rf[preds2014$rf < 0] <- 0
preds2014$pls[preds2014$pls < 0] <- 0
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
preds2013$mean <- rowMeans(preds2013[,  5:7])
preds2014$mean <- rowMeans(preds2014[,  5:7])

## graphing
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: PP Assists, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("PP Assists in 2013 (Scaled)") + ylab("PP Assists in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 PP Assists, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Assists") + ylab("Actual PP Assists")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 PP Assists, Support Vector Machine Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Assists") + ylab("Actual PP Assists")
corr <- round(cor(preds2014$outcome, preds2014$pls), digits = 4)
plot14pls <- ggplot(preds2014, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 PP Assists, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Assists") + ylab("Actual PP Assists")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 PP Assists, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Assists") + ylab("Actual PP Assists")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 PP Assists, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Assists") + ylab("Actual PP Assists")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/Assists/PPA2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14pls, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: PP Assists, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("PP Assists in 2012 (Scaled)") + ylab("PP Assists in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 PP Assists, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Assists") + ylab("Actual PP Assists")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 PP Assists, Support Vector Machine Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Assists") + ylab("Actual PP Assists")
corr <- round(cor(preds2013$outcome, preds2013$pls), digits = 4)
plot13pls <- ggplot(preds2013, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 PP Assists, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Assists") + ylab("Actual PP Assists")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 PP Assists, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Assists") + ylab("Actual PP Assists")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 PP Assists, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Assists") + ylab("Actual PP Assists")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/Assists/PPA2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13pls, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting PPA for 2015
preds2015 <- nhlPredict(2014, 2014, ppaModel3, outcome = 43)
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$pls[preds2015$pls < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
preds2015[, 3:5] <- preds2015[, 3:5] * preds2015$games_played
output$pp_assists <- rowMeans(preds2015[, 3:5])

## extrapolating other data
output$assists <- rowSums(output[, 11:13])
output$es_points <- output$es_goals + output$es_assists
output$sh_points <- output$sh_goals + output$sh_assists
output$pp_points <- output$pp_goals + output$pp_assists
output$points <- rowSums(output[, 15:17])

## printing output to database
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
dbWriteTable(conn, "newskatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)
