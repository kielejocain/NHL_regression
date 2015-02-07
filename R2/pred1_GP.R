source("~/workspace/NHL_regression/R2/setup.R")
skaterstats <- nhlClean()
gpModel <- nhlModel(2012, 2013, 3, seed = 798619, rf.cutoff = 10, distribution = "poisson")
names(skaterstats)[gpModel[["cols1"]]]
names(skaterstats)[gpModel[["cols2"]]]
cols1 <- c(1:8, 15, 24:25, 30:32, 38:41)
cols2 <- c(10, 16, 19, 35, 42, 45:46)
gpModel <- nhlModel(2012, 2013, 3, cols1 = cols1, cols2 = cols2, 
                    seed = 655714, rf.cutoff = 10, distribution = "poisson")
corData <- nhlShape(2012, 2013, outcome = 3, rm.nhlnum = FALSE)
corData <- merge(corData, nhlPredict(2012, 2013, 3, gpModel))
corData$games_played[corData$games_played > 82] <- 82
png(filename = "~/workspace/NHL_regression/graphics/GP2014.png")
qplot(games_played, outcome, data = corData, geom = c("smooth", "point"),
      main = "Predicted Games Played in 2013-2014 NHL Season",
      xlab = "Predicted Games Played", ylab = "Actual Games Played")
dev.off()
cor(corData$outcome, corData$games_played)
output <- nhlPredict(2013, 2014, 3, gpModel)
dbWriteTable(conn, "skatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)