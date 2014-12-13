source("~/workspace/NHL_regression/R/setup.R")
load("~/workspace/NHL_regression/R/gp.RData")
skaterstats <- skaterstats[, -c(3:5, 38:41)]
cols <- c(1:3, 5:6, 8, 15:16, 19:20, 24, 26, 28:32, 35, 38:41)
rfData <- nhlShape(2014, 2014, cols = cols, rm.nhlnum = FALSE)
gbmData <- nhlShape(2013, 2014, cols = cols, rm.nhlnum = FALSE)
rfData <- rfData[rfData$nhl_num %in% gbmData$nhl_num, ]
gpData <- as.data.frame(gbmData$nhl_num)
names(gpData)[1] <- "nhl_num"
gpData$gprf1 <- predict(gprfmod1, rfData)
gpData$gprf2 <- predict(gprfmod2, rfData)
gpData$gprf3 <- predict(gprfmod3, rfData)
gpData$gprf4 <- predict(gprfmod4, rfData)
gpData$gprf5 <- predict(gprfmod5, rfData)
gpData$gprf6 <- predict(gprfmod6, rfData)
gpData$gpgbm1 <- predict(gpgbmmod1, gbmData)
gpData$gpgbm2 <- predict(gpgbmmod2, gbmData)
gpData$gpgbm3 <- predict(gpgbmmod3, gbmData)
gpData$gpgbm4 <- predict(gpgbmmod4, gbmData)
gpData$gpgbm5 <- predict(gpgbmmod5, gbmData)
gpData$gpgbm6 <- predict(gpgbmmod6, gbmData)
output <- as.data.frame(gpData$nhl_num)
names(output)[1] <- "nhl_num"
output$games_played <- predict(gpModel, gpData)
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
dbWriteTable(conn, "skatpred15", output, row.names=FALSE)
dbDisconnect(conn)