source("~/workspace/NHL_regression/R/setup.R")
load("~/workspace/NHL_regression/R/gp.RData")
skaterstats <- skaterstats[, -c(3:5, 38:41)]
rfData <- nhlShape(2014, 2014, cols = gpModel[["columns"]], rm.nhlnum = FALSE)
gbmData <- nhlShape(2013, 2014, cols = gpModel[["columns"]], rm.nhlnum = FALSE)
rfData <- rfData[rfData$nhl_num %in% gbmData$nhl_num, ]
gpData <- as.data.frame(gbmData$nhl_num)
names(gpData)[1] <- "nhl_num"
gpData$gprf1 <- predict(gpModel[[1]], rfData)
gpData$gprf2 <- predict(gpModel[[2]], rfData)
gpData$gprf3 <- predict(gpModel[[3]], rfData)
gpData$gprf4 <- predict(gpModel[[4]], rfData)
gpData$gprf5 <- predict(gpModel[[5]], rfData)
gpData$gprf6 <- predict(gpModel[[6]], rfData)
gpData$gpgbm1 <- predict(gpModel[[7]], gbmData)
gpData$gpgbm2 <- predict(gpModel[[8]], gbmData)
gpData$gpgbm3 <- predict(gpModel[[9]], gbmData)
gpData$gpgbm4 <- predict(gpModel[[10]], gbmData)
gpData$gpgbm5 <- predict(gpModel[[11]], gbmData)
gpData$gpgbm6 <- predict(gpModel[[12]], gbmData)
output <- as.data.frame(gpData$nhl_num)
names(output)[1] <- "nhl_num"
output$games_played <- predict(gpModel[["model"]], gpData)
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
dbWriteTable(conn, "skatpred15", output, row.names=FALSE)
dbDisconnect(conn)