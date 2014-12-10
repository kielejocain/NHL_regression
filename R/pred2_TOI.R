source("~/workspace/NHL_regression/R/setup.R")
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
rs <- dbSendQuery(conn, statement = "SELECT * FROM skatpred15
                  ORDER BY nhl_num;")
output <- fetch(rs, n = -1)
dbClearResult(rs)
load("~/workspace/NHL_regression/R/TOI.RData")
skaterstats <- skaterstats[, -c(3:5, 38:41)]
cols <- c(1:3, 15, 24, 28:32, 38:41)
rfData <- nhlShape(2014, 2014, cols = cols, rm.nhlnum = FALSE)
gbmData <- nhlShape(2013, 2014, cols = cols, rm.nhlnum = FALSE)
rfData <- rfData[rfData$nhl_num %in% gbmData$nhl_num, ]
esData <- as.data.frame(gbmData$nhl_num)
names(esData)[1] <- "nhl_num"
esData$esrf1 <- predict(esrfmod1, rfData)
esData$esrf2 <- predict(esrfmod2, rfData)
esData$esrf3 <- predict(esrfmod3, rfData)
esData$esrf4 <- predict(esrfmod4, rfData)
esData$esgbm1 <- predict(esgbmmod1, gbmData)
esData$esgbm2 <- predict(esgbmmod2, gbmData)
esData$esgbm3 <- predict(esgbmmod3, gbmData)
esData$esgbm4 <- predict(esgbmmod4, gbmData)
output$es_toi <- predict(esModel, esData)
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
dbWriteTable(conn, "skatpred15", output)
dbDisconnect(conn)