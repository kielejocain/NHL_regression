## define custom functions, call data from the database
source("~/workspace/NHL_regression/R3/setup.R")

## clean the data
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

## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 38)
# factControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
# estFactors.rf <- nhlAnalyze2(fitData, seed = 480536, importance = TRUE, trControl = factControl)
# estFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 779785, trControl = factControl)
# estFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 769266, trControl = factControl)
# estFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 726863, trControl = factControl)
# estFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 943811, trControl = factControl)

## use the importance output to select factors
cols <- list()
cols[["rf"]] <- c(1:2, 8, 20, 24, 29, 31, 34, 38, 41, 47)
cols[["gbm"]] <- c(1:2, 8, 20, 24, 30:31, 38, 41, 47)
cols[["pls"]] <- c(1:2, 38:41)
cols[["knn"]] <- c(1:2, 5, 24, 26, 29:31, 38:41)
cols[["svmLinear"]] <- c(1:2, 5, 24, 26, 29, 31, 38:41)

## build single models, ensemble the models, and look at correlations
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
controls <- list()
controls[[1]] <- fitControl
controls[[2]] <- fitControl
controls[[3]] <- fitControl
controls[[4]] <- fitControl
controls[[5]] <- fitControl
# estModel <- nhlModel(2010, 2010, outcome = 38, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
#                      controls = controls, seed = 453739)
# estCorrs <- nhlCorr(2010, 2013, 38, estModel)
# estModel2 <- nhlModel(2010, 2010, outcome = 38, cols = cols, methods = c("rf", "gbm", "pls", "svmLinear"),
#                       controls = controls, seed = 564505)
# estCorrs2 <- nhlCorr(2010, 2013, 38, estModel2)
estModel3 <- nhlModel(2013, 2013, outcome = 38, cols = cols, methods = c("rf", "gbm", "pls", "svmLinear"),
                      controls = controls, seed = 132137)
estCorrs3 <- nhlCorr(2010, 2013, 38, estModel3)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, estModel3, outcome = 38)
preds2014 <- nhlPredict(2013, 2013, estModel3, outcome = 38)
preds2013$rf[preds2013$rf < 0] <- 0
preds2013$gbm[preds2013$gbm < 0] <- 0
preds2013$pls[preds2013$pls < 0] <- 0
preds2013$svmLinear[preds2013$svmLinear < 0] <- 0
preds2013$cumulative[preds2013$cumulative < 0] <- 0
preds2014$rf[preds2014$rf < 0] <- 0
preds2014$gbm[preds2014$gbm < 0] <- 0
preds2014$pls[preds2014$pls < 0] <- 0
preds2014$svmLinear[preds2014$svmLinear < 0] <- 0
preds2014$cumulative[preds2014$cumulative < 0] <- 0
preds2013$mean <- rowMeans(preds2013[, 2:5])
preds2014$mean <- rowMeans(preds2014[, 2:5])

## graphing
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: ES TOI/G, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      scale_y_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      xlab("ES TOI/G in 2013") + ylab("ES TOI/G in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 ES TOI/G, Random Forest Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      scale_y_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      xlab("Predicted ES TOI/G") + ylab("Actual ES TOI/G")
corr <- round(cor(preds2014$outcome, preds2014$pls), digits = 4)
plot14pls <- ggplot(preds2014, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 ES TOI/G, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      scale_y_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      xlab("Predicted ES TOI/G") + ylab("Actual ES TOI/G")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 ES TOI/G, k-Nearest Neighbors Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      scale_y_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      xlab("Predicted ES TOI/G") + ylab("Actual ES TOI/G")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 ES TOI/G, SVM Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      scale_y_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      xlab("Predicted ES TOI/G") + ylab("Actual ES TOI/G")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 ES TOI/G, Regression Ensembling (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      scale_y_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      xlab("Predicted ES TOI/G") + ylab("Actual ES TOI/G")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/TOI/ESTOI2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14pls, plot14gbm, plot14svm, plot14cum, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point()  +
      ggtitle(paste("Naive Model: ES TOI/G, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      scale_y_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      xlab("ES TOI/G in 2012") + ylab("ES TOI/G in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 ES TOI/G, Random Forest Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      scale_y_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      xlab("Predicted ES TOI/G") + ylab("Actual ES TOI/G")
corr <- round(cor(preds2013$outcome, preds2013$pls), digits = 4)
plot13pls <- ggplot(preds2013, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 ES TOI/G, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      scale_y_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      xlab("Predicted ES TOI/G") + ylab("Actual ES TOI/G")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 ES TOI/G, k-Nearest Neighbors Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      scale_y_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      xlab("Predicted ES TOI/G") + ylab("Actual ES TOI/G")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 ES TOI/G, SVM Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      scale_y_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      xlab("Predicted ES TOI/G") + ylab("Actual ES TOI/G")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 ES TOI/G, Regression Ensembling (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      scale_y_continuous(breaks = c(300, 600, 900, 1200), labels = c("5:00", "10:00", "15:00", "20:00")) +
      xlab("Predicted ES TOI/G") + ylab("Actual ES TOI/G")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/TOI/ESTOI2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13pls, plot13gbm, plot13svm, plot13cum, ncol = 2)
dev.off()

## predicting ESG for 2015
preds2015 <- nhlPredict(2014, 2014, estModel3, outcome = 38)
preds2015$cumulative[preds2015$cumulative < 0] <- 0
output$es_toi <- preds2015$cumulative

## short-handed TOI
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 39)
# shtFactors.rf <- nhlAnalyze2(fitData, seed = 480353, importance = TRUE, trControl = factControl)
# shtFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 243781, trControl = factControl)
# shtFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 795779, trControl = factControl)
# shtFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 92018, trControl = factControl)
# shtFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 313740, trControl = factControl)

## use the importance output to select factors
cols[["rf"]] <- c(1:2, 27, 29, 33, 38:39)
cols[["gbm"]] <- c(1:2, 27, 29, 39)
cols[["pls"]] <- c(1:2, 38:41)
cols[["knn"]] <- c(1:2, 25, 27, 29, 38:39, 41)
cols[["svmLinear"]] <- c(1:2, 25, 27, 29, 38:39, 41)

## build single models, ensemble the models, and look at correlations
# shtModel <- nhlModel(2010, 2010, outcome = 39, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
#                      controls = controls, seed = 756846)
# shtCorrs <- nhlCorr(2010, 2013, 39, shtModel)
# shtModel2 <- nhlModel(2010, 2010, outcome = 39, cols = cols, methods = c("rf", "gbm", "pls", "svmLinear"),
#                      controls = controls, seed = 636404)
# shtCorrs2 <- nhlCorr(2010, 2013, 39, shtModel2)
shtModel3 <- nhlModel(2013, 2013, outcome = 39, cols = cols, methods = c("rf", "gbm", "pls", "svmLinear"),
                      controls = controls, seed = 423159)
shtCorrs3 <- nhlCorr(2010, 2013, 39, shtModel3)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, shtModel3, outcome = 39)
preds2014 <- nhlPredict(2013, 2013, shtModel3, outcome = 39)
preds2013$rf[preds2013$rf < 0] <- 0
preds2013$pls[preds2013$pls < 0] <- 0
preds2013$gbm[preds2013$gbm < 0] <- 0
preds2013$svmLinear[preds2013$svmLinear < 0] <- 0
preds2013$cumulative[preds2013$cumulative < 0] <- 0
preds2014$rf[preds2014$rf < 0] <- 0
preds2014$pls[preds2014$pls < 0] <- 0
preds2014$gbm[preds2014$gbm < 0] <- 0
preds2014$svmLinear[preds2014$svmLinear < 0] <- 0
preds2014$cumulative[preds2014$cumulative < 0] <- 0
preds2013$mean <- rowMeans(preds2013[, 2:5])
preds2014$mean <- rowMeans(preds2014[, 2:5])

## graphing
sh_breaks = c(0, 60, 120, 180, 240, 300)
sh_labels = c("", "1:00", "2:00", "3:00", "4:00", "5:00")
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: SH TOI/G, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = sh_breaks, labels = sh_labels) +
      scale_y_continuous(breaks = sh_breaks, labels = sh_labels) +
      xlab("SH TOI/G in 2013") + ylab("SH TOI/G in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 SH TOI/G, Random Forest Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = sh_breaks, labels = sh_labels) +
      scale_y_continuous(breaks = sh_breaks, labels = sh_labels) +
      xlab("Predicted SH TOI/G") + ylab("Actual SH TOI/G")
corr <- round(cor(preds2014$outcome, preds2014$pls), digits = 4)
plot14pls <- ggplot(preds2014, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 SH TOI/G, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = sh_breaks, labels = sh_labels) +
      scale_y_continuous(breaks = sh_breaks, labels = sh_labels) +
      xlab("predicted SH TOI/G") + ylab("Actual SH TOI/G")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 SH TOI/G, Boosting Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = sh_breaks, labels = sh_labels) +
      scale_y_continuous(breaks = sh_breaks, labels = sh_labels) +
      xlab("Predicted SH TOI/G") + ylab("Actual SH TOI/G")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 SH TOI/G, SVM Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = sh_breaks, labels = sh_labels) +
      scale_y_continuous(breaks = sh_breaks, labels = sh_labels) +
      xlab("Predicted SH TOI/G") + ylab("Actual SH TOI/G")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14cum <- ggplot(preds2014, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 SH TOI/G, Regression Ensembling (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = sh_breaks, labels = sh_labels) +
      scale_y_continuous(breaks = sh_breaks, labels = sh_labels) +
      xlab("Predicted SH TOI/G") + ylab("Actual SH TOI/G")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/TOI/SHTOI2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14pls, plot14gbm, plot14svm, plot14cum, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point()  +
      ggtitle(paste("Naive Model: SH TOI/G, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = sh_breaks, labels = sh_labels) +
      scale_y_continuous(breaks = sh_breaks, labels = sh_labels) +
      xlab("SH TOI/G in 2012") + ylab("SH TOI/G in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 SH TOI/G, Random Forest Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = sh_breaks, labels = sh_labels) +
      scale_y_continuous(breaks = sh_breaks, labels = sh_labels) +
      xlab("Predicted SH TOI/G") + ylab("Actual SH TOI/G")
corr <- round(cor(preds2013$outcome, preds2013$pls), digits = 4)
plot13pls <- ggplot(preds2013, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 SH TOI/G, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = sh_breaks, labels = sh_labels) +
      scale_y_continuous(breaks = sh_breaks, labels = sh_labels) +
      xlab("Predicted SH TOI/G") + ylab("Actual SH TOI/G")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 SH TOI/G, Boosting Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = sh_breaks, labels = sh_labels) +
      scale_y_continuous(breaks = sh_breaks, labels = sh_labels) +
      xlab("Predicted SH TOI/G") + ylab("Actual SH TOI/G")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 SH TOI/G, SVM Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = sh_breaks, labels = sh_labels) +
      scale_y_continuous(breaks = sh_breaks, labels = sh_labels) +
      xlab("Predicted SH TOI/G") + ylab("Actual SH TOI/G")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13cum <- ggplot(preds2013, aes(x=cumulative, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 SH TOI/G, Regression Ensembling (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = sh_breaks, labels = sh_labels) +
      scale_y_continuous(breaks = sh_breaks, labels = sh_labels) +
      xlab("Predicted SH TOI/G") + ylab("Actual SH TOI/G")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/TOI/SHTOI2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13pls, plot13gbm, plot13svm, plot13cum, ncol = 2)
dev.off()

## predicting SHTOI for 2015
preds2015 <- nhlPredict(2014, 2014, shtModel3, outcome = 39)
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$pls[preds2015$pls < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015$mean <- rowMeans(preds2015[, 2:5])
output$sh_toi <- preds2015$mean

## power play TOI
## subset the data and build test models, then look at importance output
# fitData <- nhlShape(2010, 2010, outcome = 40)
# pptFactors.rf <- nhlAnalyze2(fitData, seed = 19825, importance = TRUE, trControl = factControl)
# pptFactors.gbm <- nhlAnalyze2(fitData, method = "gbm", seed = 55496, trControl = factControl)
# pptFactors.pls <- nhlAnalyze2(fitData, method = "pls", seed = 629255, trControl = factControl)
# pptFactors.knn <- nhlAnalyze2(fitData, method = "knn", seed = 549234, trControl = factControl)
# pptFactors.svm <- nhlAnalyze2(fitData, method = "svmLinear", seed = 174663, trControl = factControl)

## use the importance output to select factors
cols[["rf"]] <- c(1:2, 6, 10, 24:25, 32, 40, 43, 46)
cols[["gbm"]] <- c(1:2, 6, 24:25, 32, 40, 43)
cols[["pls"]] <- c(1:2, 38:41)
cols[["knn"]] <- c(1:2, 4:6, 9:10, 15, 24:25, 30, 40:41, 43, 45:46)
cols[["svmLinear"]] <- c(1:2, 4:6, 9:10, 15, 24:25, 30, 40:41, 43, 45:46)

## build single models, ensemble the models, and look at correlations
# pptModel <- nhlModel(2013, 2013, outcome = 40, cols = cols, methods = c("rf", "gbm", "pls", "knn", "svmLinear"),
#                      controls = controls, seed = 424953)
# pptCorrs <- nhlCorr(2010, 2013, 40, pptModel)
pptModel2 <- nhlModel(2010, 2010, outcome = 40, cols = cols, methods = c("rf", "gbm", "pls", "svmLinear"),
                     controls = controls, seed = 546989)
pptCorrs2 <- nhlCorr(2010, 2013, 40, pptModel2)

## prediction shaping
preds2013 <- nhlPredict(2012, 2012, pptModel2, outcome = 40)
preds2014 <- nhlPredict(2013, 2013, pptModel2, outcome = 40)
preds2013$rf[preds2013$rf < 0] <- 0
preds2013$pls[preds2013$pls < 0] <- 0
preds2013$gbm[preds2013$gbm < 0] <- 0
preds2013$svmLinear[preds2013$svmLinear < 0] <- 0
preds2013$cumulative[preds2013$cumulative < 0] <- 0
preds2014$rf[preds2014$rf < 0] <- 0
preds2014$pls[preds2014$pls < 0] <- 0
preds2014$gbm[preds2014$gbm < 0] <- 0
preds2014$svmLinear[preds2014$svmLinear < 0] <- 0
preds2014$cumulative[preds2014$cumulative < 0] <- 0
preds2013$mean <- rowMeans(preds2013[, 2:5])
preds2014$mean <- rowMeans(preds2014[, 2:5])

## graphing
pp_breaks = c(0, 60, 120, 180, 240, 300)
pp_labels = c("", "1:00", "2:00", "3:00", "4:00", "5:00")
corr <- round(cor(preds2014$outcome, preds2014$naive), digits = 4)
plot14naive <- ggplot(preds2014, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("Naive Model: PP TOI/G, 2013 vs. 2014 (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = pp_breaks, labels = pp_labels) +
      scale_y_continuous(breaks = pp_breaks, labels = pp_labels) +
      xlab("PP TOI/G in 2013") + ylab("PP TOI/G in 2014")
corr <- round(cor(preds2014$outcome, preds2014$rf), digits = 4)
plot14rf <- ggplot(preds2014, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 PP TOI/G, Random Forest Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = pp_breaks, labels = pp_labels) +
      scale_y_continuous(breaks = pp_breaks, labels = pp_labels) +
      xlab("Predicted PP TOI/G") + ylab("Actual PP TOI/G")
corr <- round(cor(preds2014$outcome, preds2014$pls), digits = 4)
plot14pls <- ggplot(preds2014, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 PP TOI/G, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = pp_breaks, labels = pp_labels) +
      scale_y_continuous(breaks = pp_breaks, labels = pp_labels) +
      xlab("predicted PP TOI/G") + ylab("Actual PP TOI/G")
corr <- round(cor(preds2014$outcome, preds2014$gbm), digits = 4)
plot14gbm <- ggplot(preds2014, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 PP TOI/G, Boosting Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = pp_breaks, labels = pp_labels) +
      scale_y_continuous(breaks = pp_breaks, labels = pp_labels) +
      xlab("Predicted PP TOI/G") + ylab("Actual PP TOI/G")
corr <- round(cor(preds2014$outcome, preds2014$svmLinear), digits = 4)
plot14svm <- ggplot(preds2014, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 PP TOI/G, SVM Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = pp_breaks, labels = pp_labels) +
      scale_y_continuous(breaks = pp_breaks, labels = pp_labels) +
      xlab("Predicted PP TOI/G") + ylab("Actual PP TOI/G")
corr <- round(cor(preds2014$outcome, preds2014$cumulative), digits = 4)
plot14mean <- ggplot(preds2014, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2014 PP TOI/G, Simple Ensembling (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = pp_breaks, labels = pp_labels) +
      scale_y_continuous(breaks = pp_breaks, labels = pp_labels) +
      xlab("Predicted PP TOI/G") + ylab("Actual PP TOI/G")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/TOI/PPTOI2014_full.png")
grid.arrange(plot14naive, plot14rf, plot14pls, plot14gbm, plot14svm, plot14mean, ncol = 2)
dev.off()
corr <- round(cor(preds2013$outcome, preds2013$naive), digits = 4)
plot13naive <- ggplot(preds2013, aes(x=naive, y=outcome)) +
      geom_smooth(method="lm") + geom_point()  +
      ggtitle(paste("Naive Model: PP TOI/G, 2012 vs. 2013 (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = pp_breaks, labels = pp_labels) +
      scale_y_continuous(breaks = pp_breaks, labels = pp_labels) +
      xlab("PP TOI/G in 2012") + ylab("PP TOI/G in 2013")
corr <- round(cor(preds2013$outcome, preds2013$rf), digits = 4)
plot13rf <- ggplot(preds2013, aes(x=rf, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 PP TOI/G, Random Forest Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = pp_breaks, labels = pp_labels) +
      scale_y_continuous(breaks = pp_breaks, labels = pp_labels) +
      xlab("Predicted PP TOI/G") + ylab("Actual PP TOI/G")
corr <- round(cor(preds2013$outcome, preds2013$pls), digits = 4)
plot13pls <- ggplot(preds2013, aes(x=pls, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 PP TOI/G, Partial Least Squares Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = pp_breaks, labels = pp_labels) +
      scale_y_continuous(breaks = pp_breaks, labels = pp_labels) +
      xlab("Predicted PP TOI/G") + ylab("Actual PP TOI/G")
corr <- round(cor(preds2013$outcome, preds2013$gbm), digits = 4)
plot13gbm <- ggplot(preds2013, aes(x=gbm, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 PP TOI/G, Boosting Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = pp_breaks, labels = pp_labels) +
      scale_y_continuous(breaks = pp_breaks, labels = pp_labels) +
      xlab("Predicted PP TOI/G") + ylab("Actual PP TOI/G")
corr <- round(cor(preds2013$outcome, preds2013$svmLinear), digits = 4)
plot13svm <- ggplot(preds2013, aes(x=svmLinear, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 PP TOI/G, SVM Model (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = pp_breaks, labels = pp_labels) +
      scale_y_continuous(breaks = pp_breaks, labels = pp_labels) +
      xlab("Predicted PP TOI/G") + ylab("Actual PP TOI/G")
corr <- round(cor(preds2013$outcome, preds2013$cumulative), digits = 4)
plot13mean <- ggplot(preds2013, aes(x=mean, y=outcome)) +
      geom_smooth(method="lm") + geom_point() + 
      ggtitle(paste("2013 PP TOI/G, Simple Ensembling (r = ", corr, ")", sep = "")) +
      scale_x_continuous(breaks = pp_breaks, labels = pp_labels) +
      scale_y_continuous(breaks = pp_breaks, labels = pp_labels) +
      xlab("Predicted PP TOI/G") + ylab("Actual PP TOI/G")
png(width = 960, height = 960, 
    filename = "~/workspace/NHL_regression/graphics/TOI/PPTOI2013_full.png")
grid.arrange(plot13naive, plot13rf, plot13pls, plot13gbm, plot13svm, plot13mean, ncol = 2)
dev.off()

## predicting PPTOI for 2015
preds2015 <- nhlPredict(2014, 2014, pptModel2, outcome = 40)
preds2015$rf[preds2015$rf < 0] <- 0
preds2015$gbm[preds2015$gbm < 0] <- 0
preds2015$pls[preds2015$pls < 0] <- 0
preds2015$svmLinear[preds2015$svmLinear < 0] <- 0
preds2015$mean <- rowMeans(preds2015[, 2:5])
output$pp_toi <- preds2015$mean
output$toi <- rowSums(output[, 3:5])

## printing output to database
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
dbWriteTable(conn, "newskatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)