source("~/workspace/NHL_regression/R/setup.R")
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
rs <- dbSendQuery(conn, statement = "SELECT * FROM skatpred15
                  ORDER BY nhl_num;")
output <- fetch(rs, n = -1)
dbClearResult(rs)
skaterstats <- skaterstats[, -c(3:5, 38:41)]
faceoffWModel <- nhlModel(2012, 2013, 33, seed = 972432)
output <- merge(output, nhlPredict(2013, 2014, 33, faceoffWModel))
faceoffLModel <- nhlModel(2012, 2013, 34, seed = 997742, rf.cutoff = 25, gbm.cutoff = 8)
output <- merge(output, nhlPredict(2013, 2014, 34, faceoffLModel))
dbWriteTable(conn, "skatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)