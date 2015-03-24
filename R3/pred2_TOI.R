source("~/workspace/NHL_regression/R3/setup.R")
skaterstats <- nhlClean()
source("~/workspace/NHL_regression/R3/GPsetup.R")

## fetch previous predictions
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
rs <- dbSendQuery(conn, statement = "SELECT * FROM newskatpred15
                  ORDER BY nhl_num;")
output <- fetch(rs, n = -1)
dbClearResult(rs)
dbDisconnect(conn)

## factor analysis
# fitData <- nhlShape(2010, 2010, outcome = 38)
# estFactors <- nhlAnalyze(fitData, fitData, seed = 480536)
# estFactors

## model building
cols = c(1:2, 38, 41)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
controls <- list()
controls[[1]] <- fitControl
controls[[2]] <- fitControl
controls[[3]] <- fitControl
controls[[4]] <- fitControl
estModel <- nhlModel(2013, 2013, outcome = 38, cols = cols, methods = c("rf", "gbm", "knn", "svmLinear"),
                     controls = controls, seed = 490250)
estCorrs <- nhlCorr(2010, 2013, 38, estModel)
estModel2 <- nhlModel(2013, 2013, outcome = 38, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                      controls = controls, seed = 331791)
estCorrs2 <- nhlCorr(2010, 2013, 38, estModel2)