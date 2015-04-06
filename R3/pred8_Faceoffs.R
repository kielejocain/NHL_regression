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

## Faceoff Wins
## subset the data and build test models, then look at importance output
fitData <- nhlShape(2010, 2010, outcome = 33)
fowFactors.rf <- nhlAnalyze2(fitData, seed = 717583)
fowFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 213728)
fowFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 312670)
fowFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 494267)
fowFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 960991)

## use the importance output to select factors
cols <- list()
cols[["rf"]] <- c(1:2, 26, 33:34, 39, 41, 45, 47)
cols[["gbm"]] <- c(1:2, 33:34, 47)
cols[["pls"]] <- c(1:2, 33:34, 47)
cols[["knn"]] <- c(1:2, 33:34, 47)
cols[["svmLinear"]] <- c(1:2, 33:34, 47)

## build single models, ensemble the models, and look at correlations
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
controls <- list()
controls[[1]] <- fitControl
controls[[2]] <- fitControl
controls[[3]] <- fitControl
controls[[4]] <- fitControl
controls[[5]] <- fitControl
fowModel <- nhlModel(2010, 2010, outcome = 33, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
                     controls = controls, seed = 449960)
fowCorrs <- nhlCorr(2010, 2013, 33, fowModel)
fowModel2 <- nhlModel(2010, 2010, outcome = 33, cols = cols, methods = c("rf", "gbm", "knn", "svmLinear"),
                     controls = controls, seed = 575575)
fowCorrs2 <- nhlCorr(2010, 2013, 33, fowModel2)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, fowModel, outcome = 33)
preds2014 <- nhlPredict(2013, 2013, fowModel, outcome = 33)
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
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Faceoff Wins, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("Faceoff Wins in 2013 (Scaled)") + ylab("Faceoff Wins in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Faceoff Wins, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Wins") + ylab("Actual Faceoff Wins")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Faceoff Wins, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Wins") + ylab("Actual Faceoff Wins")
corr <- round(cor(preds2014$outcome, preds2014$pls), digits = 4)
plot14pls <- ggplot(preds2014, aes(x=pls, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Faceoff Wins, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Wins") + ylab("Actual Faceoff Wins")
corr <- round(cor(preds2014$outcome, preds2014$knn), digits = 4)
plot14knn <- ggplot(preds2014, aes(x=knn, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Faceoff Wins, k-Nearest Neighbors Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Wins") + ylab("Actual Faceoff Wins")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Faceoff Wins, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Wins") + ylab("Actual Faceoff Wins")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Faceoff Wins, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Wins") + ylab("Actual Faceoff Wins")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Faceoff Wins, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Wins") + ylab("Actual Faceoff Wins")
png(width = 960, height = 1220, 
    filename = "~/workspace/NHL_regression/graphics/Faceoffs/FOW2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14pls, plot14knn, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Faceoff Wins, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("Faceoff Wins in 2012 (Scaled)") + ylab("Faceoff Wins in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Faceoff Wins, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Wins") + ylab("Actual Faceoff Wins")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Faceoff Wins, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Wins") + ylab("Actual Faceoff Wins")
corr <- round(cor(preds2013$outcome, preds2013$pls), digits = 4)
plot13pls <- ggplot(preds2013, aes(x=pls, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Faceoff Wins, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Wins") + ylab("Actual Faceoff Wins")
corr <- round(cor(preds2013$outcome, preds2013$knn), digits = 4)
plot13knn <- ggplot(preds2013, aes(x=knn, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Faceoff Wins, k-Nearest Neighbors Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Wins") + ylab("Actual Faceoff Wins")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Faceoff Wins, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Wins") + ylab("Actual Faceoff Wins")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Faceoff Wins, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Wins") + ylab("Actual Faceoff Wins")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Faceoff Wins, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Wins") + ylab("Actual Faceoff Wins")
png(width = 960, height = 1220, 
    filename = "~/workspace/NHL_regression/graphics/Faceoffs/FOW2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13pls, plot13knn, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting faceoff wins for 2015
preds2015 <- nhlPredict(2014, 2014, fowModel, outcome = 33)
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$pls[preds2015$pls < 0] <- 0
preds2015$knn[preds2015$knn < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
output$faceoff_wins <- preds2015$cumulative * preds2015$games_played

## Faceoff Losses
## subset the data and build test models, then look at importance output
fitData <- nhlShape(2010, 2010, outcome = 34)
folFactors.rf <- nhlAnalyze2(fitData, seed = 540686)
folFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 749022)
folFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 377100)
folFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 164778)
folFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 921364)

## use the importance output to select factors
cols[["rf"]] <- c(1:2, 33:34, 39, 41, 45, 47)
cols[["gbm"]] <- c(1:2, 33:34)
cols[["pls"]] <- c(1:2, 33:34, 47)
cols[["knn"]] <- c(1:2, 33:34, 47)
cols[["svmLinear"]] <- c(1:2, 33:34, 47)

## build single models, ensemble the models, and look at correlations
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
controls <- list()
controls[[1]] <- fitControl
controls[[2]] <- fitControl
controls[[3]] <- fitControl
controls[[4]] <- fitControl
controls[[5]] <- fitControl
folModel <- nhlModel(2010, 2010, outcome = 34, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
                     controls = controls, seed = 449960)
folCorrs <- nhlCorr(2010, 2013, 34, folModel)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, folModel, outcome = 34)
preds2014 <- nhlPredict(2013, 2013, folModel, outcome = 34)
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
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Faceoff Losses, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("Faceoff Losses in 2013 (Scaled)") + ylab("Faceoff Losses in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Faceoff Losses, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Losses") + ylab("Actual Faceoff Losses")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Faceoff Losses, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Losses") + ylab("Actual Faceoff Losses")
corr <- round(cor(preds2014$outcome, preds2014$pls), digits = 4)
plot14pls <- ggplot(preds2014, aes(x=pls, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Faceoff Losses, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Losses") + ylab("Actual Faceoff Losses")
corr <- round(cor(preds2014$outcome, preds2014$knn), digits = 4)
plot14knn <- ggplot(preds2014, aes(x=knn, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Faceoff Losses, k-Nearest Neighbors Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Losses") + ylab("Actual Faceoff Losses")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Faceoff Losses, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Losses") + ylab("Actual Faceoff Losses")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Faceoff Losses, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Losses") + ylab("Actual Faceoff Losses")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Faceoff Losses, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Losses") + ylab("Actual Faceoff Losses")
png(width = 960, height = 1220, 
    filename = "~/workspace/NHL_regression/graphics/Faceoffs/FOL2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14pls, plot14knn, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Faceoff Losses, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("Faceoff Losses in 2012 (Scaled)") + ylab("Faceoff Losses in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Faceoff Losses, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Losses") + ylab("Actual Faceoff Losses")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Faceoff Losses, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Losses") + ylab("Actual Faceoff Losses")
corr <- round(cor(preds2013$outcome, preds2013$pls), digits = 4)
plot13pls <- ggplot(preds2013, aes(x=pls, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Faceoff Losses, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Losses") + ylab("Actual Faceoff Losses")
corr <- round(cor(preds2013$outcome, preds2013$knn), digits = 4)
plot13knn <- ggplot(preds2013, aes(x=knn, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Faceoff Losses, k-Nearest Neighbors Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Losses") + ylab("Actual Faceoff Losses")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Faceoff Losses, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Losses") + ylab("Actual Faceoff Losses")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Faceoff Losses, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Losses") + ylab("Actual Faceoff Losses")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Faceoff Losses, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Faceoff Losses") + ylab("Actual Faceoff Losses")
png(width = 960, height = 1220, 
    filename = "~/workspace/NHL_regression/graphics/Faceoffs/FOL2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13pls, plot13knn, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting faceoff losses for 2015
preds2015 <- nhlPredict(2014, 2014, folModel, outcome = 34)
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$pls[preds2015$pls < 0] <- 0
preds2015$knn[preds2015$knn < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
output$faceoff_losses <- preds2015$cumulative * preds2015$games_played

## printing output to database
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
dbWriteTable(conn, "newskatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)
