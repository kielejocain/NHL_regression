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
# fitData <- nhlShape(2010, 2010, outcome = 42)
# esgFactors <- nhlAnalyze(fitData, fitData, seed = 492472)
# esgFactors

## model building
cols = c(1:2, 4, 15, 29:30, 32:34, 42, 46)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
controls <- list()
controls[[1]] <- fitControl
controls[[2]] <- fitControl
controls[[3]] <- fitControl
controls[[4]] <- fitControl
# esgModel <- nhlModel(2013, 2013, outcome = 42, cols = cols, methods = c("rf", "gbm", "knn", "svmLinear"),
#                      controls = controls, seed = 292255)
# esgCorrs <- nhlCorr(2010, 2013, 42, esgModel)
esgModel2 <- nhlModel(2013, 2013, outcome = 42, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                     controls = controls, seed = 38563)
esgCorrs2 <- nhlCorr(2010, 2013, 42, esgModel2)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, esgModel2, outcome = 42)
preds2014 <- nhlPredict(2013, 2013, esgModel2, outcome = 42)
preds2013 <- merge(GP2013, preds2013)
preds2014 <- merge(GP2014, preds2014)
preds2013$rf[preds2013$rf > 1] <- 1
preds2013$rf[preds2013$rf < 0] <- 0
preds2013$gbm[preds2013$gbm > 1] <- 1
preds2013$gbm[preds2013$gbm < 0] <- 0
preds2013$svmLinear[preds2013$svmLinear > 1] <- 1
preds2013$svmLinear[preds2013$svmLinear < 0] <- 0
preds2013$cumulative[preds2013$cumulative > 1] <- 1
preds2013$cumulative[preds2013$cumulative < 0] <- 0
preds2014$rf[preds2014$rf > 1] <- 1
preds2014$rf[preds2014$rf < 0] <- 0
preds2014$gbm[preds2014$gbm > 1] <- 1
preds2014$gbm[preds2014$gbm < 0] <- 0
preds2014$svmLinear[preds2014$svmLinear > 1] <- 1
preds2014$svmLinear[preds2014$svmLinear < 0] <- 0
preds2014$cumulative[preds2014$cumulative > 1] <- 1
preds2014$cumulative[preds2014$cumulative < 0] <- 0
for (i in c(5:8)) {
      preds2013[, i] <- preds2013[, i] * preds2013$games_played.pred
      preds2014[, i] <- preds2014[, i] * preds2014$games_played.pred
}
preds2013$outcome <- preds2013$outcome * preds2013$games_played
preds2014$outcome <- preds2014$outcome * preds2014$games_played
preds2013$naive <- preds2013$naive * preds2013$games_played.prev / 82 * 48
preds2014$naive <- preds2014$naive * preds2014$games_played.prev / 48 * 82
preds2013$mean <- (preds2013$rf + preds2013$gbm + preds2013$svmLinear) / 3
preds2014$mean <- (preds2014$rf + preds2014$gbm + preds2014$svmLinear) / 3

## graphing
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: ES Goals, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("ES Goals in 2013 (Scaled)") + ylab("ES Goals in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 ES Goals, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 ES Goals, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 ES Goals, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 ES Goals, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 ES Goals, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20141_naive.png")
print(plot14naive)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20142_rf.png")
print(plot14rf)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20143_gbm.png")
print(plot14gbm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20144_svm.png")
print(plot14svm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20145_cum.png")
print(plot14cum)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20146_mean.png")
print(plot14mean)
dev.off()
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/ESG/ESG2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: ES Goals, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("ES Goals in 2012 (Scaled)") + ylab("ES Goals in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 ES Goals, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 ES Goals, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 ES Goals, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 ES Goals, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 ES Goals, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted ES Goals") + ylab("Actual ES Goals")
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20131_naive.png")
print(plot13naive)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20132_rf.png")
print(plot13rf)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20133_gbm.png")
print(plot13gbm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20134_svm.png")
print(plot13svm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20135_cum.png")
print(plot13cum)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/ESG/ESG20136_mean.png")
print(plot13mean)
dev.off()
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/ESG/ESG2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting ESG for 2015
preds2015 <- nhlPredict(2014, 2014, esgModel2, outcome = 42)
preds2015$rf[preds2015$rf > 1] <- 1
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm > 1] <- 1
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear > 1] <- 1
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015$cumulative[preds2015$cumulative > 1] <- 1
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
preds2015[, 3:5] <- preds2015[, 3:5] * preds2015$games_played
preds2015$es_goals <- (preds2015$rf + preds2015$gbm + preds2015$svmLinear) / 3
output <- merge(output, preds2015[, c(1, 9)])

## factor analysis
fitData <- nhlShape(2010, 2010, outcome = 9)
ppgFactors <- nhlAnalyze(fitData, fitData, seed = 207526)
ppgFactors

## model building
cols <- c(1:2, 4, 6, 9, 15, 40, 43)
# ppgModel <- nhlModel(2013, 2013, outcome = 9, cols = cols, methods = c("rf", "gbm", "knn", "svmLinear"),
#                       controls = controls, seed = 101142)
# esgCorrs <- nhlCorr(2010, 2013, 9, ppgModel)
ppgModel2 <- nhlModel(2013, 2013, outcome = 9, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                     controls = controls, seed = 429596)
esgCorrs2 <- nhlCorr(2010, 2013, 9, ppgModel2)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, ppgModel2, outcome = 9)
preds2014 <- nhlPredict(2013, 2013, ppgModel2, outcome = 9)
preds2013 <- merge(GP2013, preds2013)
preds2014 <- merge(GP2014, preds2014)
preds2013$rf[preds2013$rf > 1] <- 1
preds2013$rf[preds2013$rf < 0] <- 0
preds2013$gbm[preds2013$gbm > 1] <- 1
preds2013$gbm[preds2013$gbm < 0] <- 0
preds2013$svmLinear[preds2013$svmLinear > 1] <- 1
preds2013$svmLinear[preds2013$svmLinear < 0] <- 0
preds2013$cumulative[preds2013$cumulative > 1] <- 1
preds2013$cumulative[preds2013$cumulative < 0] <- 0
preds2014$rf[preds2014$rf > 1] <- 1
preds2014$rf[preds2014$rf < 0] <- 0
preds2014$gbm[preds2014$gbm > 1] <- 1
preds2014$gbm[preds2014$gbm < 0] <- 0
preds2014$svmLinear[preds2014$svmLinear > 1] <- 1
preds2014$svmLinear[preds2014$svmLinear < 0] <- 0
preds2014$cumulative[preds2014$cumulative > 1] <- 1
preds2014$cumulative[preds2014$cumulative < 0] <- 0
for (i in c(5:8)) {
      preds2013[, i] <- preds2013[, i] * preds2013$games_played.pred
      preds2014[, i] <- preds2014[, i] * preds2014$games_played.pred
}
preds2013$outcome <- preds2013$outcome * preds2013$games_played
preds2014$outcome <- preds2014$outcome * preds2014$games_played
preds2013$naive <- preds2013$naive * preds2013$games_played.prev / 82 * 48
preds2014$naive <- preds2014$naive * preds2014$games_played.prev / 48 * 82
preds2013$mean <- (preds2013$rf + preds2013$gbm + preds2013$svmLinear) / 3
preds2014$mean <- (preds2014$rf + preds2014$gbm + preds2014$svmLinear) / 3

## graphing
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: PP Goals, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      xlab("PP Goals in 2013 (Scaled)") + ylab("PP Goals in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 PP Goals, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 PP Goals, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 PP Goals, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 PP Goals, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2014$outcome, preds2014$mean), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2014 PP Goals, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
png(filename = "~/workspace/NHL_regression/graphics/PPG/PPG20141_naive.png")
print(plot14naive)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/PPG/PPG20142_rf.png")
print(plot14rf)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/PPG/PPG20143_gbm.png")
print(plot14gbm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/PPG/PPG20144_svm.png")
print(plot14svm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/PPG/PPG20145_cum.png")
print(plot14cum)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/PPG/PPG20146_mean.png")
print(plot14mean)
dev.off()
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/PPG/PPG2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14gbm, plot14svm, plot14cum, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: PP Goals, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      xlab("PP Goals in 2012 (Scaled)") + ylab("PP Goals in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 PP Goals, Random Forest Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 PP Goals, Random Boosting Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 PP Goals, SVM Model (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 PP Goals, Regression Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
corr <- round(cor(preds2013$outcome, preds2013$mean), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("2013 PP Goals, Simple Ensembling (r = ", corr, ")", sep = "")) +
      xlab("Predicted PP Goals") + ylab("Actual PP Goals")
png(filename = "~/workspace/NHL_regression/graphics/PPG/PPG20131_naive.png")
print(plot13naive)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/PPG/PPG20132_rf.png")
print(plot13rf)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/PPG/PPG20133_gbm.png")
print(plot13gbm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/PPG/PPG20134_svm.png")
print(plot13svm)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/PPG/PPG20135_cum.png")
print(plot13cum)
dev.off()
png(filename = "~/workspace/NHL_regression/graphics/PPG/PPG20136_mean.png")
print(plot13mean)
dev.off()
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/PPG/PPG2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13gbm, plot13svm, plot13cum, plot13mean, ncol = 2)
dev.off()

## predicting PPG for 2015
preds2015 <- nhlPredict(2014, 2014, ppgModel2, outcome = 9)
preds2015$rf[preds2015$rf > 1] <- 1
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm > 1] <- 1
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear > 1] <- 1
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015$cumulative[preds2015$cumulative > 1] <- 1
preds2015$cumulative[preds2015$cumulative < 0] <- 0
preds2015 <- merge(output[, 1:2], preds2015)
preds2015[, 3:5] <- preds2015[, 3:5] * preds2015$games_played
preds2015$pp_goals <- (preds2015$rf + preds2015$gbm + preds2015$svmLinear) / 3
output <- merge(output, preds2015[, c(1, 9)])

## factor analysis
fitData <- nhlShape(2010, 2010, outcome = 11)
shgFactors <- nhlAnalyze(fitData, fitData, seed = 266989)
shgFactors

## model building
cols <- c(1:2, 11, 15, 29, 32:34, 39, 42)
shgModel <- nhlModel(2013, 2013, outcome = 11, cols = cols, methods = c("rf", "gbm", "knn", "svmLinear"),
                     controls = controls, seed = 347717)
shgCorrs <- nhlCorr(2010, 2013, 42, shgModel)
shgModel2 <- nhlModel(2013, 2013, outcome = 11, cols = cols, methods = c("rf", "gbm", "svmLinear"), 
                      controls = controls, seed = 40310)
shgCorrs2 <- nhlCorr(2010, 2013, 42, shgModel2)