source("~/workspace/NHL_regression/R3/setup.R")
skaterstats <- nhlClean()
gpModel <- nhlModel(2009, 2010, 3, seed = 798619)
names(skaterstats)[gpModel[["cols"]]]
cols <- c(1:3, 6, 15, 24, 30, 41)
corData <- nhlShape(2009, 2010, outcome = 3, rm.nhlnum = FALSE)
corData <- merge(corData, nhlPredict(2009, 2010, 3, gpModel))
corData$games_played[corData$games_played > 1] <- 1
cor(corData$outcome, corData$games_played)
png(filename = "~/workspace/NHL_regression/graphics/GP2011_new_train.png")
qplot(games_played, outcome, data = corData, geom = c("smooth", "point"),
      main = "Using All Factors: 2010-2011 NHL Season (Training)",
      xlab = "Predicted Games Played", ylab = "Actual Games Played")
dev.off()
testData <- nhlShape(2010, 2011, outcome = 3, rm.nhlnum = FALSE)
testData <- merge(testData, nhlPredict(2010, 2011, 3, gpModel))
testData$games_played[testData$games_played > 1] <- 1
cor(testData$outcome, testData$games_played)
png(filename = "~/workspace/NHL_regression/graphics/GP2012_new_test.png")
qplot(games_played, outcome, data = testData, geom = c("smooth", "point"),
      main = "Using All Factors: 2011-2012 NHL Season (Predicting)",
      xlab = "Predicted Games Played", ylab = "Actual Games Played")
dev.off()
output <- nhlPredict(2013, 2014, 3, gpModel)
dbWriteTable(conn, "skatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)