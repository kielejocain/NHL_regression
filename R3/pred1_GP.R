source("~/workspace/NHL_regression/R2/setup.R")
skaterstats <- nhlClean()
gpModel <- nhlModel(2012, 2013, 3, seed = 798619, rf.cutoff = 10, distribution = "poisson")
names(skaterstats)[gpModel[["cols1"]]]
names(skaterstats)[gpModel[["cols2"]]]
cols1 <- c(1:8, 15, 24:25, 30:32, 38:41)
cols2 <- c(10, 16, 19, 35, 42, 45:46)
gpModel <- nhlModel(2009, 2010, 3, cols1 = cols1, cols2 = cols2, 
                    seed = 655714, distribution = "poisson")
corData <- nhlShape(2009, 2010, outcome = 3, rm.nhlnum = FALSE)
corData <- merge(corData, nhlPredict(2009, 2010, 3, gpModel))
corData$games_played[corData$games_played > 82] <- 82
cor(corData$outcome, corData$games_played)
png(filename = "~/workspace/NHL_regression/graphics/GP2011_full_train.png")
qplot(games_played, outcome, data = corData, geom = c("smooth", "point"),
      main = "Using All Factors: 2010-2011 NHL Season (Training)",
      xlab = "Predicted Games Played", ylab = "Actual Games Played")
dev.off()
testData <- nhlShape(2010, 2011, outcome = 3, rm.nhlnum = FALSE)
testData <- merge(testData, nhlPredict(2010, 2011, 3, gpModel))
testData$games_played[testData$games_played > 82] <- 82
cor(testData$outcome, testData$games_played)
png(filename = "~/workspace/NHL_regression/graphics/GP2012_full_test.png")
qplot(games_played, outcome, data = testData, geom = c("smooth", "point"),
      main = "Using All Factors: 2011-2012 NHL Season (Predicting)",
      xlab = "Predicted Games Played", ylab = "Actual Games Played")
dev.off()
cols12 <- c(1:3, 8, 38)
cols22 <- c(15, 24, 32)
gpModel2 <- nhlModel(2009, 2010, 3, cols1 = cols12, cols2 = cols22, 
                     seed = 257842, distribution = "poisson")
corData2 <- nhlShape(2009, 2010, outcome = 3, rm.nhlnum = FALSE)
corData2 <- merge(corData2, nhlPredict(2009, 2010, 3, gpModel2))
corData2$games_played[corData2$games_played > 82] <- 82
cor(corData2$outcome, corData2$games_played)
png(filename = "~/workspace/NHL_regression/graphics/GP2011_small_train.png")
qplot(games_played, outcome, data = corData2, geom = c("smooth", "point"),
      main = "Fewer Factors: 2010-2011 NHL Season (Training)",
      xlab = "Predicted Games Played", ylab = "Actual Games Played")
dev.off()
testData2 <- nhlShape(2010, 2011, outcome = 3, rm.nhlnum = FALSE)
testData2 <- merge(testData2, nhlPredict(2010, 2011, 3, gpModel2))
testData2$games_played[testData2$games_played > 82] <- 82
cor(testData2$outcome, testData2$games_played)
png(filename = "~/workspace/NHL_regression/graphics/GP2012_small_test.png")
qplot(games_played, outcome, data = testData2, geom = c("smooth", "point"),
      main = "Fewer Factors: 2011-2012 NHL Season (Predicting)",
      xlab = "Predicted Games Played", ylab = "Actual Games Played")
dev.off()
output <- nhlPredict(2013, 2014, 3, gpModel)
dbWriteTable(conn, "skatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)