source("~/workspace/NHL_regression/R3/setup.R")
skaterstats <- nhlClean()
# fitData <- nhlShape(2010, 2010, outcome = 42)
# esgFactors <- nhlAnalyze(fitData, fitData, seed = 492472)
# esgFactors
cols = c(1:2, 4, 15, 29:30, 32:34, 42, 46)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
controls <- list()
controls[[1]] <- fitControl
controls[[2]] <- fitControl
controls[[3]] <- fitControl
controls[[4]] <- fitControl
esgModel <- nhlModel(2013, 2013, outcome = 42, cols = cols, methods = c("rf", "gbm", "knn", "svmLinear"),
                     controls = controls, seed = 292255)
esgCorrs <- nhlCorr(2010, 2013, 42, esgModel)
preds2013 <- nhlPredict(2012, 2012, esgModel, outcome = 42)
preds2014 <- nhlPredict(2013, 2013, esgModel, outcome = 42)