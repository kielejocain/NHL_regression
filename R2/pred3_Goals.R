source("~/workspace/NHL_regression/R2/setup.R")
#skaterstats <- skaterstats[, -c(3:5, 38:41)]
skaterstats <- nhlClean()
goalsModel <- nhlModel(2012, 2013, 4, seed = 480782)
names(skaterstats)[goalsModel[["cols1"]]]
names(skaterstats)[goalsModel[["cols2"]]]
cols1 <- c(1:2, 4, 6, 15, 30, 32, 40)
cols2 <- c(24:25, 33)
goalsModel <- nhlModel(2009, 2010, 4, cols1 = cols1, cols2 = cols2, 
                    seed = 575)
corData <- nhlShape(2009, 2010, outcome = 4, rm.nhlnum = FALSE)
corData <- merge(corData, nhlPredictA(2009, 2010, 4, goalsModel))
cor(corData$outcome, corData$goals)
png(filename = "~/workspace/NHL_regression/graphics/Goals2011_full_train.png")
qplot(goals, outcome, data = corData, geom = c("smooth", "point"),
      main = "Using All Factors: 2010-2011 NHL Season (Training)",
      xlab = "Predicted Goals", ylab = "Actual Goals")
dev.off()
testData <- nhlShape(2010, 2011, outcome = 4, rm.nhlnum = FALSE)
testData <- merge(testData, nhlPredictA(2010, 2011, 4, goalsModel))
cor(testData$outcome, testData$goals)
png(filename = "~/workspace/NHL_regression/graphics/Goals2012_full_test.png")
qplot(goals, outcome, data = testData, geom = c("smooth", "point"),
      main = "Using All Factors: 2011-2012 NHL Season (Predicting)",
      xlab = "Predicted Goals", ylab = "Actual Goals")
dev.off()