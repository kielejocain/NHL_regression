cols <- c(1:4, 30, 32, 38, 41, 46)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
controls <- list()
controls[[1]] <- fitControl
controls[[2]] <- fitControl
controls[[3]] <- fitControl
controls[[4]] <- fitControl
gpModel4 <- nhlModel(2013, 2013, outcome = 3, cols = cols, methods = c("rf", "gbm", "svmLinear"),
                     controls = controls, seed = 174896)
preds2013 <- nhlPredict(2012, 2012, gpModel4, outcome = 3)
preds2014 <- nhlPredict(2013, 2013, gpModel4, outcome = 3)
preds2013$rf[preds2013$rf > 1] <- 1
preds2013$gbm[preds2013$gbm > 1] <- 1
preds2013$svmLinear[preds2013$svmLinear > 1] <- 1
preds2013$cumulative[preds2013$cumulative > 1] <- 1
preds2013[, -1] <- preds2013[, -1] * 48
preds2013$mean <- (preds2013$rf + preds2013$gbm + preds2013$svmLinear) / 3
preds2014$rf[preds2014$rf > 1] <- 1
preds2014$gbm[preds2014$gbm > 1] <- 1
preds2014$svmLinear[preds2014$svmLinear > 1] <- 1
preds2014$cumulative[preds2014$cumulative > 1] <- 1
preds2014[, -1] <- preds2014[, -1] * 82
preds2014$mean <- (preds2014$rf + preds2014$gbm + preds2014$svmLinear) / 3
GP2013 <- preds2013[, c(1, 8)]
names(GP2013)[2] <- "games_played.pred"
GP2013 <- merge(GP2013, skaterstats[skaterstats$season == 2012, c(1, 3)])
GP2013[, 3] <- GP2013[, 3] * 82
names(GP2013)[3] <- "games_played.prev"
GP2013 <- merge(GP2013, skaterstats[skaterstats$season == 2013, c(1, 3)])
GP2013[, 4] <- GP2013[, 4] * 48
GP2014 <- preds2014[, c(1, 8)]
names(GP2014)[2] <- "games_played.pred"
GP2014 <- merge(GP2014, skaterstats[skaterstats$season == 2013, c(1, 3)])
GP2014[, 3] <- GP2014[, 3] * 48
names(GP2014)[3] <- "games_played.prev"
GP2014 <- merge(GP2014, skaterstats[skaterstats$season == 2014, c(1, 3)])
GP2014[, 4] <- GP2014[, 4] * 82
rm(cols, gpModel4, preds2013, preds2014, fitControl, controls)