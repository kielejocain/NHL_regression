source("~/workspace/NHL_regression/R3/setup.R")

## clean the data, add position data from bio table
skaterstats <- nhlClean()
skaterstats <- merge(skaterstats, skaters[, 3:4])
skaterstats$player_position <- as.factor(skaterstats$player_position)

## fetch previous predictions
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
rs <- dbSendQuery(conn, statement = "SELECT * FROM newskatpred15
                  ORDER BY nhl_num;")
output <- fetch(rs, n = -1)
dbClearResult(rs)
dbDisconnect(conn)

## bring in data for games played in 2013-2014 to scale up the data to totals
## rather than per game numbers
source("~/workspace/NHL_regression/R3/GPsetup.R")

## Shots
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 15)
# shotFactors.rf <- nhlAnalyze2(fitData, seed = 564722)
# shotFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 714647)
# shotFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 137693)
# shotFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 13280)
# shotFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 540772)

## use the importance output to select factors
cols <- list()
cols[["rf"]] <- c(1:2, 4, 6, 15, 30, 32, 38:41, 46)
cols[["gbm"]] <- c(1:2, 4, 6, 15, 24, 29:30, 32, 40)
cols[["pls"]] <- c(1:2, 15:16, 38:41)
cols[["knn"]] <- c(1:2, 4:6, 9:10, 13, 15, 24:25, 30, 32, 40, 42:43, 45:46)
cols[["svmLinear"]] <- c(1:2, 4:6, 9:10, 13, 15, 24:25, 30, 32, 40, 42:43, 45:46)

## build single models, ensemble the models, and look at correlations
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
controls <- list()
controls[[1]] <- fitControl
controls[[2]] <- fitControl
controls[[3]] <- fitControl
controls[[4]] <- fitControl
controls[[5]] <- fitControl
# shotModel <- nhlModel(2010, 2010, outcome = 15, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
#                      controls = controls, seed = 135928)
# shotCorrs <- nhlCorr(2010, 2013, 15, shotModel)
shotModel2 <- nhlModel(2013, 2013, outcome = 15, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                      controls = controls, seed = 938701)
shotCorrs2 <- nhlCorr(2010, 2013, 15, shotModel2)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, shotModel2, outcome = 15)
preds2014 <- nhlPredict(2013, 2013, shotModel2, outcome = 15)
preds2013 <- merge(GP2013, preds2013)
preds2014 <- merge(GP2014, preds2014)
preds2013$rf[preds2013$rf < 0] <- 0
preds2013$gbm[preds2013$gbm < 0] <- 0
preds2013$svmLinear[preds2013$svmLinear < 0] <- 0
preds2013$cumulative[preds2013$cumulative < 0] <- 0
preds2014$rf[preds2014$rf < 0] <- 0
preds2014$gbm[preds2014$gbm < 0] <- 0
preds2014$svmLinear[preds2014$svmLinear < 0] <- 0
preds2014$cumulative[preds2014$cumulative < 0] <- 0
for (i in c(5:8)) {
      preds2013[, i] <- preds2013[, i] * preds2013$games_played.pred
      preds2014[, i] <- preds2014[, i] * preds2014$games_played.pred
}
preds2013$outcome <- preds2013$outcome * preds2013$games_played
preds2014$outcome <- preds2014$outcome * preds2014$games_played
preds2013$naive <- preds2013$naive * preds2013$games_played.prev / 82 * 48
preds2014$naive <- preds2014$naive * preds2014$games_played.prev / 48 * 82
preds2013$mean <- rowMeans(preds2013[,  5:7])
preds2014$mean <- rowMeans(preds2014[,  5:7])

## graphing
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Shots, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("Shots in 2013 (Scaled)") + ylab("Shots in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Shots, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Shots") + ylab("Actual Shots")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Shots, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Shots") + ylab("Actual Shots")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Shots, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Shots") + ylab("Actual Shots")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Shots, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Shots") + ylab("Actual Shots")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 Shots, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Shots") + ylab("Actual Shots")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/Shots/Shots2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Shots, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("Shots in 2012 (Scaled)") + ylab("Shots in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Shots, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Shots") + ylab("Actual Shots")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Shots, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Shots") + ylab("Actual Shots")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Shots, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted Shots") + ylab("Actual Shots")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Shots, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Shots") + ylab("Actual Shots")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 Shots, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted Shots") + ylab("Actual Shots")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/Shots/Shots2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting Shots for 2015 and extrapolating shot percentage
preds2015 <- nhlPredict(2014, 2014, shotModel2, outcome = 15)
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
output$shots <- preds2015$cumulative * preds2015$games_played
output$shot_pct <- output$goals / output$shots

## printing output to database
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
dbWriteTable(conn, "newskatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)
