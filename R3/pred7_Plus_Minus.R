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

## Team Goals For
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 24)
# factControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
# tgfFactors.rf <- nhlAnalyze2(fitData, seed = 608626, importance = TRUE, trControl = factControl)
# tgfFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 874259, trControl = factControl)
# tgfFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 37496, trControl = factControl)
# tgfFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 282028, trControl = factControl)
# tgfFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 471085, trControl = factControl)

## use the importance output to select factors
cols <- list()
cols[["rf"]] <- c(1:2, 5, 24:26, 30, 32, 38:41, 43, 46)
cols[["gbm"]] <- c(1:2, 24, 38, 40:41)
cols[["pls"]] <- c(1:2, 38:41)
cols[["knn"]] <- c(1:2, 4:6, 9:10, 15, 24:25, 30, 38, 40:43, 45)
cols[["svmLinear"]] <- c(1:2, 5:6, 24:25, 30, 38, 40:41, 43)

## build single models, ensemble the models, and look at correlations
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
controls <- list()
controls[[1]] <- fitControl
controls[[2]] <- fitControl
controls[[3]] <- fitControl
controls[[4]] <- fitControl
controls[[5]] <- fitControl
# tgfModel <- nhlModel(2010, 2010, outcome = 24, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
#                      controls = controls, seed = 928021)
# tgfCorrs <- nhlCorr(2010, 2013, 24, tgfModel)
# tgfModel2 <- nhlModel(2010, 2010, outcome = 24, cols = cols, methods = c("rf", "gbm", "pls", "svmLinear"),
#                      controls = controls, seed = 226886)
# tgfCorrs2 <- nhlCorr(2010, 2013, 24, tgfModel2)
tgfModel3 <- nhlModel(2013, 2013, outcome = 24, cols = cols, methods = c("rf", "gbm", "pls", "svmLinear"),
                      controls = controls, seed = 758243)
tgfCorrs3 <- nhlCorr(2010, 2013, 24, tgfModel3)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, tgfModel3, outcome = 24)
preds2014 <- nhlPredict(2013, 2013, tgfModel3, outcome = 24)
preds2013 <- merge(GP2013, preds2013)
preds2014 <- merge(GP2014, preds2014)
preds2013$rf[preds2013$rf < 0] <- 0
preds2013$gbm[preds2013$gbm < 0] <- 0
preds2013$pls[preds2013$pls < 0] <- 0
preds2013$svmLinear[preds2013$svmLinear < 0] <- 0
preds2013$cumulative[preds2013$cumulative < 0] <- 0
preds2014$rf[preds2014$rf < 0] <- 0
preds2014$gbm[preds2014$gbm < 0] <- 0
preds2014$pls[preds2014$pls < 0] <- 0
preds2014$svmLinear[preds2014$svmLinear < 0] <- 0
preds2014$cumulative[preds2014$cumulative < 0] <- 0
for (i in c(5:9)) {
      preds2013[, i] <- preds2013[, i] * preds2013$games_played.pred
      preds2014[, i] <- preds2014[, i] * preds2014$games_played.pred
}
preds2013$outcome <- preds2013$outcome * preds2013$games_played
preds2014$outcome <- preds2014$outcome * preds2014$games_played
preds2013$naive <- preds2013$naive * preds2013$games_played.prev / 82 * 48
preds2014$naive <- preds2014$naive * preds2014$games_played.prev / 48 * 82
preds2013$mean <- rowMeans(preds2013[,  5:8])
preds2014$mean <- rowMeans(preds2014[,  5:8])

## graphing
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: Team Goals For, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("Team Goals For in 2013 (Scaled)") + ylab("Team Goals For in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team Goals For, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals For") + ylab("Actual Team Goals For")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team Goals For, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals For") + ylab("Actual Team Goals For")
corr <- round(cor(preds2014$outcome, preds2014$pls), digits = 4)
plot14pls <- ggplot(preds2014, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team Goals For, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals For") + ylab("Actual Team Goals For")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team Goals For, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals For") + ylab("Actual Team Goals For")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team Goals For, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals For") + ylab("Actual Team Goals For")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team Goals For, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals For") + ylab("Actual Team Goals For")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/PM/TGF2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14pls, plot14svm, plot14cum, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: Team Goals For, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("Team Goals For in 2012 (Scaled)") + ylab("Team Goals For in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team Goals For, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals For") + ylab("Actual Team Goals For")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team Goals For, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals For") + ylab("Actual Team Goals For")
corr <- round(cor(preds2013$outcome, preds2013$pls), digits = 4)
plot13pls <- ggplot(preds2013, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team Goals For, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals For") + ylab("Actual Team Goals For")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team Goals For, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals For") + ylab("Actual Team Goals For")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team Goals For, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals For") + ylab("Actual Team Goals For")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team Goals For, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals For") + ylab("Actual Team Goals For")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/PM/TGF2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13pls, plot13svm, plot13cum, ncol = 2)
dev.off()

## predicting Team Goals For for 2015
preds2015 <- nhlPredict(2014, 2014, tgfModel3, outcome = 24)
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
output$team_goals_for <- preds2015$cumulative * preds2015$games_played

## Team PP Goals For
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 25)
# tpgfFactors.rf <- nhlAnalyze2(fitData, seed = 103235, importance = TRUE, trControl = factControl)
# tpgfFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 442982, trControl = factControl)
# tpgfFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 602398, trControl = factControl)
# tpgfFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 930714, trControl = factControl)
# tpgfFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 632229, trControl = factControl)

## use the importance output to select factors
cols[["rf"]] <- c(1:2, 6, 10, 24:25, 30, 40, 43, 46)
cols[["gbm"]] <- c(1:2, 6, 9:10, 15, 30, 24:25, 40, 43)
cols[["pls"]] <- c(1:2, 16, 38:41)
cols[["knn"]] <- c(1:2, 4:6, 9:10, 15, 24:25, 30, 40, 43, 45:46)
cols[["svmLinear"]] <- c(1:2, 4:6, 9:10, 15, 24:25, 30, 40, 43, 45:46)

## build single models, ensemble the models, and look at correlations
# tpgfModel <- nhlModel(2010, 2010, outcome = 25, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
#                      controls = controls, seed = 913690)
# tpgfCorrs <- nhlCorr(2010, 2013, 25, tpgfModel)
tpgfModel2 <- nhlModel(2010, 2010, outcome = 25, cols = cols, methods = c("rf", "gbm", "pls", "svmLinear"),
                      controls = controls, seed = 161461)
tpgfCorrs2 <- nhlCorr(2010, 2013, 25, tpgfModel2)
# tpgfModel3 <- nhlModel(2013, 2013, outcome = 25, cols = cols, methods = c("rf", "gbm", "pls", "svmLinear"),
#                       controls = controls, seed = 113770)
# tpgfCorrs3 <- nhlCorr(2010, 2013, 25, tpgfModel3)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, tpgfModel2, outcome = 25)
preds2014 <- nhlPredict(2013, 2013, tpgfModel2, outcome = 25)
preds2013 <- merge(GP2013, preds2013)
preds2014 <- merge(GP2014, preds2014)
preds2013$rf[preds2013$rf < 0] <- 0
preds2013$gbm[preds2013$gbm < 0] <- 0
preds2013$pls[preds2013$pls < 0] <- 0
preds2013$svmLinear[preds2013$svmLinear < 0] <- 0
preds2013$cumulative[preds2013$cumulative < 0] <- 0
preds2014$rf[preds2014$rf < 0] <- 0
preds2014$gbm[preds2014$gbm < 0] <- 0
preds2014$pls[preds2014$pls < 0] <- 0
preds2014$svmLinear[preds2014$svmLinear < 0] <- 0
preds2014$cumulative[preds2014$cumulative < 0] <- 0
for (i in c(5:9)) {
      preds2013[, i] <- preds2013[, i] * preds2013$games_played.pred
      preds2014[, i] <- preds2014[, i] * preds2014$games_played.pred
}
preds2013$outcome <- preds2013$outcome * preds2013$games_played
preds2014$outcome <- preds2014$outcome * preds2014$games_played
preds2013$naive <- preds2013$naive * preds2013$games_played.prev / 82 * 48
preds2014$naive <- preds2014$naive * preds2014$games_played.prev / 48 * 82
preds2013$mean <- rowMeans(preds2013[,  5:8])
preds2014$mean <- rowMeans(preds2014[,  5:8])

## graphing
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: Team PP Goals For, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("Team PP Goals For in 2013 (Scaled)") + ylab("Team PP Goals For in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team PP Goals For, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals For") + ylab("Actual Team PP Goals For")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team PP Goals For, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals For") + ylab("Actual Team PP Goals For")
corr <- round(cor(preds2014$outcome, preds2014$pls), digits = 4)
plot14pls <- ggplot(preds2014, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team PP Goals For, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals For") + ylab("Actual Team PP Goals For")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team PP Goals For, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals For") + ylab("Actual Team PP Goals For")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team PP Goals For, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals For") + ylab("Actual Team PP Goals For")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team PP Goals For, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals For") + ylab("Actual Team PP Goals For")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/PM/TPPGF2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14pls, plot14svm, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: Team PP Goals For, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("Team PP Goals For in 2012 (Scaled)") + ylab("Team PP Goals For in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team PP Goals For, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals For") + ylab("Actual Team PP Goals For")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team PP Goals For, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals For") + ylab("Actual Team PP Goals For")
corr <- round(cor(preds2013$outcome, preds2013$pls), digits = 4)
plot13pls <- ggplot(preds2013, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team PP Goals For, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals For") + ylab("Actual Team PP Goals For")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team PP Goals For, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals For") + ylab("Actual Team PP Goals For")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team PP Goals For, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals For") + ylab("Actual Team PP Goals For")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team PP Goals For, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals For") + ylab("Actual Team PP Goals For")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/PM/TPPGF2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13pls, plot13svm, plot13mean, ncol = 2)
dev.off()

## predicting Team PP Goals For for 2015
preds2015 <- nhlPredict(2014, 2014, tpgfModel2, outcome = 25)
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$pls[preds2015$pls < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
output$team_pp_goals_for <- rowMeans(preds2015[, 3:6]) * preds2015$games_played

## Team Goals Against
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 26)
# tgaFactors.rf <- nhlAnalyze2(fitData, seed = 640345, importance = TRUE, trControl = factControl)
# tgaFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 225956, trControl = factControl)
# tgaFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 956974, trControl = factControl)
# tgaFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 318899, trControl = factControl)
# tgaFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 526169, trControl = factControl)

## use the importance output to select factors
cols[["rf"]] <- c(1:4, 6, 24:27, 29, 31, 33:34, 38:41, 43, 47)
cols[["gbm"]] <- c(1:2, 26, 38:41, 47)
cols[["pls"]] <- c(1:2, 16, 38:41)
cols[["knn"]] <- c(1:2, 24, 26:27, 29, 31, 38:39, 41)
cols[["svmLinear"]] <- c(1:2, 24, 26:27, 29, 31, 38:39, 41)

## build single models, ensemble the models, and look at correlations
tgaModel <- nhlModel(2010, 2010, outcome = 26, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
                     controls = controls, seed = 37178)
tgaCorrs <- nhlCorr(2010, 2013, 26, tgaModel)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, tgaModel, outcome = 26)
preds2014 <- nhlPredict(2013, 2013, tgaModel, outcome = 26)
preds2013 <- merge(GP2013, preds2013)
preds2014 <- merge(GP2014, preds2014)
preds2013$rf[preds2013$rf < 0] <- 0
preds2013$gbm[preds2013$gbm < 0] <- 0
preds2013$pls[preds2013$pls < 0] <- 0
preds2013$knn[preds2013$knn < 0] <- 0
preds2013$svmLinear[preds2013$svmLinear < 0] <- 0
preds2013$cumulative[preds2013$cumulative < 0] <- 0
preds2014$rf[preds2014$rf < 0] <- 0
preds2014$gbm[preds2014$gbm < 0] <- 0
preds2014$pls[preds2014$pls < 0] <- 0
preds2014$knn[preds2014$knn < 0] <- 0
preds2014$svmLinear[preds2014$svmLinear < 0] <- 0
preds2014$cumulative[preds2014$cumulative < 0] <- 0
for (i in c(5:10)) {
      preds2013[, i] <- preds2013[, i] * preds2013$games_played.pred
      preds2014[, i] <- preds2014[, i] * preds2014$games_played.pred
}
preds2013$outcome <- preds2013$outcome * preds2013$games_played
preds2014$outcome <- preds2014$outcome * preds2014$games_played
preds2013$naive <- preds2013$naive * preds2013$games_played.prev / 82 * 48
preds2014$naive <- preds2014$naive * preds2014$games_played.prev / 48 * 82
preds2013$mean <- rowMeans(preds2013[,  5:9])
preds2014$mean <- rowMeans(preds2014[,  5:9])

## graphing
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: Team Goals Against, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("Team Goals Against in 2013 (Scaled)") + ylab("Team Goals Against in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team Goals Against, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals Against") + ylab("Actual Team Goals Against")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team Goals Against, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals Against") + ylab("Actual Team Goals Against")
corr <- round(cor(preds2014$outcome, preds2014$pls), digits = 4)
plot14pls <- ggplot(preds2014, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team Goals Against, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals Against") + ylab("Actual Team Goals Against")
corr <- round(cor(preds2014$outcome, preds2014$knn), digits = 4)
plot14knn <- ggplot(preds2014, aes(x=knn, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team Goals Against, k-Nearest Neighbors Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals Against") + ylab("Actual Team Goals Against")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team Goals Against, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals Against") + ylab("Actual Team Goals Against")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team Goals Against, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals Against") + ylab("Actual Team Goals Against")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team Goals Against, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals Against") + ylab("Actual Team Goals Against")
png(width = 960, height = 1220, 
    filename = "~/workspace/NHL_regression/graphics/PM/TGA2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14pls, plot14knn, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: Team Goals Against, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("Team Goals Against in 2012 (Scaled)") + ylab("Team Goals Against in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team Goals Against, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals Against") + ylab("Actual Team Goals Against")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team Goals Against, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals Against") + ylab("Actual Team Goals Against")
corr <- round(cor(preds2013$outcome, preds2013$pls), digits = 4)
plot13pls <- ggplot(preds2013, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team Goals Against, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals Against") + ylab("Actual Team Goals Against")
corr <- round(cor(preds2013$outcome, preds2013$knn), digits = 4)
plot13knn <- ggplot(preds2013, aes(x=knn, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team Goals Against, k-Nearest Neighbors Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals Against") + ylab("Actual Team Goals Against")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team Goals Against, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals Against") + ylab("Actual Team Goals Against")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team Goals Against, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals Against") + ylab("Actual Team Goals Against")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team Goals Against, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team Goals Against") + ylab("Actual Team Goals Against")
png(width = 960, height = 1220, 
    filename = "~/workspace/NHL_regression/graphics/PM/TGA2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13pls, plot13knn, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting Team Goals Against for 2015
preds2015 <- nhlPredict(2014, 2014, tgaModel, outcome = 26)
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$pls[preds2015$pls < 0] <- 0
preds2015$knn[preds2015$knn < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
output$team_goals_against <- preds2015$cumulative * preds2015$games_played

## Team PP Goals Against
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 27)
# tpgaFactors.rf <- nhlAnalyze2(fitData, seed = 872633, importance = TRUE, trControl = factControl)
# tpgaFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 445138, trControl = factControl)
# tpgaFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 162775, trControl = factControl)
# tpgaFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 889032, trControl = factControl)
# tpgaFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 427993, trControl = factControl)

## use the importance output to select factors
cols[["rf"]] <- c(1:4, 27, 29, 33, 38:39)
cols[["gbm"]] <- c(1:2, 27, 29, 39)
cols[["pls"]] <- c(1:2, 16, 38:41)
cols[["knn"]] <- c(1:2, 25, 27, 29, 38:39, 41)
cols[["svmLinear"]] <- c(1:2, 25, 27, 29, 38:39, 41)

## build single models, ensemble the models, and look at correlations
tpgaModel <- nhlModel(2010, 2010, outcome = 27, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
                     controls = controls, seed = 992426)
tpgaCorrs <- nhlCorr(2010, 2013, 27, tpgaModel)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, tpgaModel, outcome = 27)
preds2014 <- nhlPredict(2013, 2013, tpgaModel, outcome = 27)
preds2013 <- merge(GP2013, preds2013)
preds2014 <- merge(GP2014, preds2014)
preds2013$rf[preds2013$rf < 0] <- 0
preds2013$gbm[preds2013$gbm < 0] <- 0
preds2013$pls[preds2013$pls < 0] <- 0
preds2013$knn[preds2013$knn < 0] <- 0
preds2013$svmLinear[preds2013$svmLinear < 0] <- 0
preds2013$cumulative[preds2013$cumulative < 0] <- 0
preds2014$rf[preds2014$rf < 0] <- 0
preds2014$gbm[preds2014$gbm < 0] <- 0
preds2014$pls[preds2014$pls < 0] <- 0
preds2014$knn[preds2014$knn < 0] <- 0
preds2014$svmLinear[preds2014$svmLinear < 0] <- 0
preds2014$cumulative[preds2014$cumulative < 0] <- 0
for (i in c(5:10)) {
      preds2013[, i] <- preds2013[, i] * preds2013$games_played.pred
      preds2014[, i] <- preds2014[, i] * preds2014$games_played.pred
}
preds2013$outcome <- preds2013$outcome * preds2013$games_played
preds2014$outcome <- preds2014$outcome * preds2014$games_played
preds2013$naive <- preds2013$naive * preds2013$games_played.prev / 82 * 48
preds2014$naive <- preds2014$naive * preds2014$games_played.prev / 48 * 82
preds2013$mean <- rowMeans(preds2013[,  5:9])
preds2014$mean <- rowMeans(preds2014[,  5:9])

## graphing
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: Team PP Goals Against, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("Team PP Goals Against in 2013 (Scaled)") + ylab("Team PP Goals Against in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team PP Goals Against, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals Against") + ylab("Actual Team PP Goals Against")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team PP Goals Against, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals Against") + ylab("Actual Team PP Goals Against")
corr <- round(cor(preds2014$outcome, preds2014$pls), digits = 4)
plot14pls <- ggplot(preds2014, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team PP Goals Against, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals Against") + ylab("Actual Team PP Goals Against")
corr <- round(cor(preds2014$outcome, preds2014$knn), digits = 4)
plot14knn <- ggplot(preds2014, aes(x=knn, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team PP Goals Against, k-Nearest Neighbors Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals Against") + ylab("Actual Team PP Goals Against")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team PP Goals Against, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals Against") + ylab("Actual Team PP Goals Against")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team PP Goals Against, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals Against") + ylab("Actual Team PP Goals Against")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 Team PP Goals Against, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals Against") + ylab("Actual Team PP Goals Against")
png(width = 960, height = 1220, 
    filename = "~/workspace/NHL_regression/graphics/PM/TPPGA2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14pls, plot14knn, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: Team PP Goals Against, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("Team PP Goals Against in 2012 (Scaled)") + ylab("Team PP Goals Against in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team PP Goals Against, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals Against") + ylab("Actual Team PP Goals Against")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team PP Goals Against, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals Against") + ylab("Actual Team PP Goals Against")
corr <- round(cor(preds2013$outcome, preds2013$pls), digits = 4)
plot13pls <- ggplot(preds2013, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team PP Goals Against, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals Against") + ylab("Actual Team PP Goals Against")
corr <- round(cor(preds2013$outcome, preds2013$knn), digits = 4)
plot13knn <- ggplot(preds2013, aes(x=knn, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team PP Goals Against, k-Nearest Neighbors Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals Against") + ylab("Actual Team PP Goals Against")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team PP Goals Against, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals Against") + ylab("Actual Team PP Goals Against")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team PP Goals Against, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals Against") + ylab("Actual Team PP Goals Against")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 Team PP Goals Against, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Team PP Goals Against") + ylab("Actual Team PP Goals Against")
png(width = 960, height = 1220, 
    filename = "~/workspace/NHL_regression/graphics/PM/TPPGA2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13pls, plot13knn, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting Team PP Goals Against for 2015
preds2015 <- nhlPredict(2014, 2014, tpgaModel, outcome = 27)
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$pls[preds2015$pls < 0] <- 0
preds2015$knn[preds2015$knn < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
output$team_pp_goals_against <- preds2015$cumulative * preds2015$games_played

## extrapolating plus/minus
output$plus_minus <- output$team_goals_for - output$team_pp_goals_for - output$team_goals_against + output$team_pp_goals_against

## printing output to database
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
dbWriteTable(conn, "newskatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)
