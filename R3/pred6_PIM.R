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

## Minor penalties
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 19)
# factControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
# minorFactors.rf <- nhlAnalyze2(fitData, seed = 555634, importance = TRUE, trControl = factControl)
# minorFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 623121, trControl = factControl)
# minorFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 723044, trControl = factControl)
# minorFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 503921, trControl = factControl)
# minorFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 443584, trControl = factControl)

## use the importance output to select factors
cols <- list()
cols[["rf"]] <- c(1:3, 5, 8, 19:20, 24, 26, 28, 30, 38, 40:42, 45)
cols[["gbm"]] <- c(1:2, 8, 19:20, 28, 40:41)
cols[["pls"]] <- c(1:2, 8, 28, 38:41)
cols[["knn"]] <- c(1:2, 8, 19:20, 28)
cols[["svmLinear"]] <- c(1:2, 8, 19:20, 28)

## build single models, ensemble the models, and look at correlations
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
controls <- list()
controls[[1]] <- fitControl
controls[[2]] <- fitControl
controls[[3]] <- fitControl
controls[[4]] <- fitControl
controls[[5]] <- fitControl
# minorModel <- nhlModel(2010, 2010, outcome = 19, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
#                      controls = controls, seed = 928021)
# minorCorrs <- nhlCorr(2010, 2013, 19, minorModel)
# minorModel2 <- nhlModel(2010, 2010, outcome = 19, cols = cols, methods = c("rf", "gbm", "knn", "svmLinear"),
#                        controls = controls, seed = 956946)
# minorCorrs2 <- nhlCorr(2010, 2013, 19, minorModel2)
minorModel3 <- nhlModel(2013, 2013, outcome = 19, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                        controls = controls, seed = 653195)
minorCorrs3 <- nhlCorr(2010, 2013, 19, minorModel3)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, minorModel3, outcome = 19)
preds2014 <- nhlPredict(2013, 2013, minorModel3, outcome = 19)
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
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Minors, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("Minors in 2013 (Scaled)") + ylab("Minors in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Minors, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Minors") + ylab("Actual Minors")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Minors, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Minors") + ylab("Actual Minors")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Minors, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Minors") + ylab("Actual Minors")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Minors, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Minors") + ylab("Actual Minors")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Minors, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Minors") + ylab("Actual Minors")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/PIM/Minors2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Minors, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("Minors in 2012 (Scaled)") + ylab("Minors in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Minors, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Minors") + ylab("Actual Minors")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Minors, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Minors") + ylab("Actual Minors")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Minors, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Minors") + ylab("Actual Minors")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Minors, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Minors") + ylab("Actual Minors")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Minors, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Minors") + ylab("Actual Minors")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/PIM/Minors2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting Minors for 2015
preds2015 <- nhlPredict(2014, 2014, minorModel3, outcome = 19)
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
output$minors <- rowMeans(preds2015[, 3:5]) * preds2015$games_played

## Major penalties
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 20)
# majorFactors.rf <- nhlAnalyze2(fitData, seed = 249013, importance = TRUE, trControl = factControl)
# majorFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 242772, trControl = factControl)
# majorFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 74786, trControl = factControl)
# majorFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 238164, trControl = factControl)
# majorFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 196996, trControl = factControl)

## use the importance output to select factors
cols[["rf"]] <- c(1:3, 8, 19:20, 25, 28, 38, 40:41, 47)
cols[["gbm"]] <- c(1:2, 8, 20, 41)
cols[["pls"]] <- c(1:2, 38:41)
cols[["knn"]] <- c(1:2, 8, 20:21, 38, 41)
cols[["svmLinear"]] <- c(1:2, 8, 20:21, 38, 41)

## build single models, ensemble the models, and look at correlations
# majorModel <- nhlModel(2010, 2010, outcome = 20, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
#                      controls = controls, seed = 7568)
# majorCorrs <- nhlCorr(2010, 2013, 20, majorModel)
majorModel2 <- nhlModel(2012, 2012, outcome = 20, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                       controls = controls, seed = 642206)
majorCorrs2 <- nhlCorr(2010, 2013, 20, majorModel2)
# majorModel3 <- nhlModel(2013, 2013, outcome = 20, cols = cols, methods = c("rf", "gbm", "svmLinear"),
#                         controls = controls, seed = 75638)
# majorCorrs3 <- nhlCorr(2010, 2013, 20, majorModel3)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, majorModel2, outcome = 20)
preds2014 <- nhlPredict(2013, 2013, majorModel2, outcome = 20)
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
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Majors, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("Majors in 2013 (Scaled)") + ylab("Majors in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Majors, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Majors") + ylab("Actual Majors")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Majors, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Majors") + ylab("Actual Majors")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Majors, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Majors") + ylab("Actual Majors")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Majors, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Majors") + ylab("Actual Majors")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Majors, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Majors") + ylab("Actual Majors")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/PIM/Majors2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth(method = "gam") + geom_point() + 
      ggtitle(paste("Naive Model: Majors, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("Majors in 2012 (Scaled)") + ylab("Majors in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Majors, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Majors") + ylab("Actual Majors")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Majors, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Majors") + ylab("Actual Majors")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Majors, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Majors") + ylab("Actual Majors")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Majors, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Majors") + ylab("Actual Majors")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Majors, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Majors") + ylab("Actual Majors")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/PIM/Majors2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting Majors for 2015
preds2015 <- nhlPredict(2014, 2014, majorModel2, outcome = 20)
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
output$majors <- preds2015$cumulative * preds2015$games_played

## Misconduct penalties
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 21)
# miscFactors.rf <- nhlAnalyze2(fitData, seed = 5997, importance = TRUE, trControl = factControl)
# miscFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 218471, trControl = factControl)
# miscFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 934130, trControl = factControl)
# miscFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 888571, trControl = factControl)
# miscFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 314769, trControl = factControl)

## use the importance output to select factors
cols[["rf"]] <- c(1:2, 8, 19:21)
cols[["gbm"]] <- c(1:2, 8, 19:21)
cols[["pls"]] <- c(1:2, 38:41)
cols[["knn"]] <- c(1:2, 8, 19:21, 38, 41)
cols[["svmLinear"]] <- c(1:2, 8, 19:21, 38, 41)

## build single models, ensemble the models, and look at correlations
# miscModel <- nhlModel(2010, 2010, outcome = 21, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
#                      controls = controls, seed = 164663)
# miscCorrs <- nhlCorr(2010, 2013, 21, miscModel)
# miscModel2 <- nhlModel(2010, 2010, outcome = 21, cols = cols, methods = c("rf", "gbm", "svmLinear"),
#                         controls = controls, seed = 71245)
# miscCorrs2 <- nhlCorr(2010, 2013, 21, miscModel2)
miscModel3 <- nhlModel(2013, 2013, outcome = 21, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                        controls = controls, seed = 64452)
miscCorrs3 <- nhlCorr(2010, 2013, 21, miscModel3)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, miscModel3, outcome = 21)
preds2014 <- nhlPredict(2013, 2013, miscModel3, outcome = 21)
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
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Misconducts, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("Misconducts in 2013 (Scaled)") + ylab("Misconducts in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Misconducts, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Misconducts") + ylab("Actual Misconducts")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Misconducts, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Misconducts") + ylab("Actual Misconducts")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Misconducts, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Misconducts") + ylab("Actual Misconducts")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Misconducts, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Misconducts") + ylab("Actual Misconducts")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Misconducts, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Misconducts") + ylab("Actual Misconducts")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/PIM/Misconducts2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Misconducts, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("Misconducts in 2012 (Scaled)") + ylab("Misconducts in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Misconducts, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Misconducts") + ylab("Actual Misconducts")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Misconducts, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Misconducts") + ylab("Actual Misconducts")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Misconducts, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Misconducts") + ylab("Actual Misconducts")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Misconducts, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Misconducts") + ylab("Actual Misconducts")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Misconducts, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Misconducts") + ylab("Actual Misconducts")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/PIM/Misconducts2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting Misconducts for 2015
preds2015 <- nhlPredict(2014, 2014, miscModel3, outcome = 21)
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
output$misconducts <- rowMeans(preds2015[, 3:5]) * preds2015$games_played

## Game Misconduct penalties
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 22)
# gmiscFactors.rf <- nhlAnalyze2(fitData, seed = 866733, importance = TRUE, trControl = factControl)
# gmiscFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 392015, trControl = factControl)
# gmiscFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 469089, trControl = factControl)
# gmiscFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 370527, trControl = factControl)
# gmiscFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 779892, trControl = factControl)

## use the importance output to select factors
cols[["rf"]] <- c(1:3, 4:6, 9:10, 15, 19:22, 24:25, 31, 33:34, 38, 40, 43, 45:46)
cols[["gbm"]] <- c(1:2, 15, 20, 38, 40:41)
cols[["pls"]] <- c(1:2, 38:41)
cols[["knn"]] <- c(1:2, 8, 20:21, 38, 41)
cols[["svmLinear"]] <- c(1:2, 8, 20, 38, 41)

## build single models, ensemble the models, and look at correlations
# gmiscModel <- nhlModel(2010, 2010, outcome = 22, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
#                      controls = controls, seed = 578544)
# gmiscCorrs <- nhlCorr(2010, 2013, 22, gmiscModel)
# gmiscModel2 <- nhlModel(2010, 2010, outcome = 22, cols = cols, methods = c("rf", "gbm", "svmLinear"),
#                         controls = controls, seed = 798192)
# gmiscCorrs2 <- nhlCorr(2010, 2013, 22, gmiscModel2)
gmiscModel3 <- nhlModel(2013, 2013, outcome = 22, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                       controls = controls, seed = 57589)
gmiscCorrs3 <- nhlCorr(2010, 2013, 22, gmiscModel3)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, gmiscModel3, outcome = 22)
preds2014 <- nhlPredict(2013, 2013, gmiscModel3, outcome = 22)
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
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Misconducts, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("Misconducts in 2013 (Scaled)") + ylab("Game Misconducts in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Game Misconducts, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Game Misconducts") + ylab("Actual Game Misconducts")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Game Misconducts, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Game Misconducts") + ylab("Actual Game Misconducts")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Game Misconducts, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Game Misconducts") + ylab("Actual Game Misconducts")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Game Misconducts, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Game Misconducts") + ylab("Actual Game Misconducts")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Game Misconducts, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Game Misconducts") + ylab("Actual Game Misconducts")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/PIM/Game_Misconducts2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Game Misconducts, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("Game Misconducts in 2012 (Scaled)") + ylab("Game Misconducts in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Game Misconducts, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Game Misconducts") + ylab("Actual Game Misconducts")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Game Misconducts, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Game Misconducts") + ylab("Actual Game Misconducts")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Game Misconducts, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Game Misconducts") + ylab("Actual Game Misconducts")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Game Misconducts, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Game Misconducts") + ylab("Actual Game Misconducts")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Game Misconducts, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Game Misconducts") + ylab("Actual Game Misconducts")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/PIM/Game_Misconducts2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting Game Misconducts for 2015
preds2015 <- nhlPredict(2014, 2014, gmiscModel3, outcome = 22)
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
output$game_misconducts <- preds2015$cumulative * preds2015$games_played

## computing penalty minutes
output$penalty_minutes <- 2 * output$minors + 5 * output$majors + 10 * (output$misconducts + output$game_misconducts)

## printing output to database
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
dbWriteTable(conn, "newskatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)
