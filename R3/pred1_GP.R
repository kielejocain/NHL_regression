source("~/workspace/NHL_regression/R3/setup.R")
skaterstats <- nhlClean()
gpModel <- nhlModel(2013, 2013, 3, seed = 404508)
names(skaterstats)[gpModel[["cols"]]]
cols <- c(1:3, 6, 15, 24, 30, 38:41)
plotData <- skaterstats[skaterstats$season %in% c(2013, 2014), c(1, 2, 3)]
plotData <- reshape(plotData, timevar = "season", idvar = "nhl_num", direction = "wide")
plotData <- plotData[complete.cases(plotData), ]
plotData$games_played.2013 <- plotData$games_played.2013 * 48
plotData$games_played.2014 <- plotData$games_played.2014 * 82
naiveCor <- cor(plotData$games_played.2013, plotData$games_played.2014)
png(filename = "~/workspace/NHL_regression/graphics/GP/GP20141_naive.png")
qplot(games_played.2014, games_played.2013, data = plotData, geom = c("smooth", "point"),
      main = "Naive Model: Games Played, 2013 vs. 2014",
      xlab = "Games Played in 2013", ylab = "Games Played in 2014")
dev.off()
gpTest <- nhlModel(2010, 2010, 3, cols = cols, seed = 794825)
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
testData <- nhlShape(2011, 2011, cols = cols, outcome = 3, rm.nhlnum = FALSE)
testData <- merge(testData, nhlPredict(2011, 2011, 3, gpTest))
testData$rf[testData$rf > 1] <- 1
testData$gbm[testData$gbm > 1] <- 1
testData$games_played[testData$games_played > 1] <- 1
testData$outcome <- testData$outcome * 82
testData$rf <- testData$rf * 82
testData$gbm <- testData$gbm * 82
testData$games_played <- testData$games_played * 82
cor(testData$outcome, testData$games_played.1)
cor(testData$outcome, testData$rf)
cor(testData$outcome, testData$gbm)
cor(testData$outcome, testData$games_played)
test2Data <- nhlShape(2012, 2012, cols = cols, outcome = 3, rm.nhlnum = FALSE)
test2Data <- merge(test2Data, nhlPredict(2012, 2012, 3, gpTest))
test2Data$rf[test2Data$rf > 1] <- 1
test2Data$gbm[test2Data$gbm > 1] <- 1
test2Data$games_played[test2Data$games_played > 1] <- 1
test2Data$outcome <- test2Data$outcome * 48
test2Data$rf <- test2Data$rf * 48
test2Data$gbm <- test2Data$gbm * 48
test2Data$games_played <- test2Data$games_played * 48
cor(test2Data$outcome, test2Data$games_played.1)
cor(test2Data$outcome, test2Data$rf)
cor(test2Data$outcome, test2Data$gbm)
cor(test2Data$outcome, test2Data$games_played)
png(filename = "~/workspace/NHL_regression/graphics/GP2012_new_test.png")
qplot(games_played, outcome, data = testData, geom = c("smooth", "point"),
      main = "Using All Factors: 2011-2012 NHL Season (Predicting)",
      xlab = "Predicted Games Played", ylab = "Actual Games Played")
dev.off()
output <- nhlPredict(2013, 2014, 3, gpModel)
dbWriteTable(conn, "skatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)