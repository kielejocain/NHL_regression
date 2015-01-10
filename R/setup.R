## This script will load the NHL data and relevant functions that  are used in
## the manipulation and analysis of the data set.

library(doMC)
registerDoMC(4)
library(RPostgreSQL)
driv <- dbDriver("PostgreSQL")
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
rs <- dbSendQuery(conn, statement = "SELECT * FROM skaterstats
                  ORDER BY season, nhl_num;")
skaterstats <- fetch(rs, n = -1)
dbClearResult(rs)
rs <- dbSendQuery(conn, statement = "SELECT * FROM skaters
                  ORDER BY nhl_num;")
skaters <- fetch(rs, n = -1)
dbClearResult(rs)
dbDisconnect(conn)
library(caret)
library(randomForest)
library(gbm)

# The nhlShape function takes a subset of the data in the skaterstats format
#
# start = first season's stats used as factors (integer)
# end = final season's stats used as factors (integer)
# cols = columns that have been selected as factors (integer vector)
# data = full data set (data frame)
# rm.NA = whether to remove incomplete observations (boolean)
#
# returns wide data frame with one row for each player

nhlShape <- function(start, end, cols = NA, outcome = NA, data = skaterstats,
                     rm.nhlnum = TRUE, rm.NA = TRUE){
      output <- subset(data, season >= start & season <= end)
      output$season <- end + 1 - output$season
      if (!is.na(cols[1])) {
            output <- output[, cols]
      }
      output <- reshape(output, timevar = "season", idvar = "nhl_num",
                      direction = "wide")
      if (!is.na(outcome)) {
            values <- subset(data, season == end+1)
            values <- values[, c(1, outcome)]
            names(values)[2] <- "outcome"
            output <- merge(output, values)
      }
      if (rm.nhlnum) {
            output <- output[, -1]
      }
      if (rm.NA) {
            output <- output[complete.cases(output), ]
      }      
      return(output)
}

# The nhlBuild function builds a machine learning model that predicts the chosen
# outcome, using as features the selected columns over the desired previous
# seasons.  Designed to be used after nhlShape(); be sure that the column to be
# predicted is named `outcome`.
#
# data = full data set (data frame)
# type = model type ("rf": random forest, "gbm": boosting)
# perc = percent of the data set to commit to the training set (0 < numeric < 1)
# seed = desired seed, for random number generator, if applicable (integer)
#
# returns a random forest model object.

nhlBuild <- function(data, type = "rf", perc = 1, seed = NA, ...) {
      if (!is.na(seed)){
            set.seed(seed)
      }
      if (perc < 1) {
            inTrain <- createDataPartition(data$outcome, p = perc, list = FALSE)
            training <- data[inTrain, ]
            testing <- data[-inTrain, ]
      } else {
            training <- data
      }
      if (type == "rf") {
            model <- randomForest(outcome ~ ., data = training, importance = TRUE, ...)
      } else if (type == "gbm") {
            # n.trees = 10000, cv.folds = 5, n.cores = 4
            model <- gbm(outcome ~ ., data = training, ...)
      }
      if (perc < 1) {
            corr <- cor(testing$outcome, predict(model, testing, type = "response"))
            print(corr)
      }
      return(model)
      }


nhlAnalyze <- function(rfdata, gbmdata, gbm.cutoff = 4, rf.cutoff = 15, seed = NA) {
      if (!is.na(seed)) {
            set.seed(seed)
      }
      rfmod1 <- nhlBuild(data = rfdata, perc = 0.7)
      rfmod2 <- nhlBuild(data = rfdata, perc = 0.7)
      rfmod3 <- nhlBuild(data = rfdata, perc = 0.7)
      rfmod4 <- nhlBuild(data = rfdata, perc = 0.7)
      rfsum1 <- data.frame(var = row.names(importance(rfmod1)),
                           percIncMSE = importance(rfmod1)[, 1],
                           incNodePur = importance(rfmod1)[, 2],
                           row.names = NULL)
      rfsum2 <- data.frame(var = row.names(importance(rfmod2)),
                           percIncMSE = importance(rfmod2)[, 1],
                           incNodePur = importance(rfmod2)[, 2],
                           row.names = NULL)
      rfsum3 <- data.frame(var = row.names(importance(rfmod3)),
                           percIncMSE = importance(rfmod3)[, 1],
                           incNodePur = importance(rfmod3)[, 2],
                           row.names = NULL)
      rfsum4 <- data.frame(var = row.names(importance(rfmod4)),
                           percIncMSE = importance(rfmod4)[, 1],
                           incNodePur = importance(rfmod4)[, 2],
                           row.names = NULL)
      rfsum <- merge(rfsum1, rfsum2, by = 1, suffixes = c(".1", ".2"))
      rfsumm <- merge(rfsum3, rfsum4, by = 1, suffixes = c(".3", ".4"))
      rfsum <- merge(rfsum, rfsumm)
      rfsum$rf.tot <- rfsum$percIncMSE.1 + rfsum$percIncMSE.2 + rfsum$percIncMSE.3 + rfsum$percIncMSE.4
      rfsum <- rfsum[order(rfsum$rf.tot, decreasing = TRUE),]
      rfsum <- rfsum[rfsum$rf.tot > rf.cutoff, c(1, 10)]
      row.names(rfsum) <- NULL
      gbmmod1 <- nhlBuild(data = gbmdata, type = "gbm", perc = 0.7, n.trees = 10000, cv.folds = 5, n.cores = 4)
      gbmmod2 <- nhlBuild(data = gbmdata, type = "gbm", perc = 0.7, n.trees = 10000, cv.folds = 5, n.cores = 4)
      gbmmod3 <- nhlBuild(data = gbmdata, type = "gbm", perc = 0.7, n.trees = 10000, cv.folds = 5, n.cores = 4)
      gbmmod4 <- nhlBuild(data = gbmdata, type = "gbm", perc = 0.7, n.trees = 10000, cv.folds = 5, n.cores = 4)
      gbmsum1 <- as.data.frame(summary(gbmmod1, plotit=FALSE))
      gbmsum2 <- as.data.frame(summary(gbmmod2, plotit=FALSE))
      gbmsum3 <- as.data.frame(summary(gbmmod3, plotit=FALSE))
      gbmsum4 <- as.data.frame(summary(gbmmod4, plotit=FALSE))
      gbmsum <- merge(gbmsum1, gbmsum2, by = 1)
      names(gbmsum) <- c("var", "var.inf.1", "var.inf.2")
      gbmsum <- merge(gbmsum, gbmsum3, by = 1)
      names(gbmsum)[4] <- "var.inf.3"
      gbmsum <- merge(gbmsum, gbmsum4, by = 1)
      names(gbmsum)[5] <- "var.inf.4"
      gbmsum$gbm.tot <- gbmsum$var.inf.1 + gbmsum$var.inf.2 + gbmsum$var.inf.3 + gbmsum$var.inf.4
      gbmsum <- gbmsum[order(gbmsum$gbm.tot, decreasing = TRUE), ]
      gbmsum <- gbmsum[gbmsum$gbm.tot > gbm.cutoff, c(1, 6)]
      row.names(gbmsum) <- NULL
      output <- merge(rfsum, gbmsum, all = TRUE, sort = FALSE)
      return(output)
}

nhlModel <- function(start, end, outcome, data = skaterstats,
                     rf.cutoff = 30, gbm.cutoff = 10, seed = NA, distribution = "gaussian") {
      rfdata <- nhlShape(end, end, outcome = outcome, data = data)
      gbmdata <- nhlShape(start, end, outcome = outcome, data = data)
      print(paste("Preliminary analysis on", names(data)[outcome]))
      factorMatrix <- nhlAnalyze(rfdata, gbmdata, gbm.cutoff = 4,
                                 rf.cutoff = 15, seed = seed)
      factors1 <- as.vector(factorMatrix[complete.cases(factorMatrix), 1])
      factors1 <- strsplit(factors1, ".", fixed = TRUE)
      cols1 <- vector()
      for (i in 1:length(factors1)) {
            cols1[i] <- which(names(data) == factors1[[i]][1])
      }
      cols1 <- c(1, 2, cols1[order(cols1)])
      if (length(cols1) == 2) {
            print(factorMatrix[1, ])
            stop("No primary factors; please lower your cutoffs.")
      }
      factorMatrix <- factorMatrix[!complete.cases(factorMatrix), ]
      factors2 <- strsplit(as.vector(factorMatrix[factorMatrix[, 2] > rf.cutoff, 1]),
                           ".", fixed = TRUE)
      factors2 <- factors2[!is.na(factors2)]
      factors3 <- strsplit(as.vector(factorMatrix[factorMatrix[, 3] > gbm.cutoff, 1]),
                           ".", fixed = TRUE)
      factors3 <- factors3[!is.na(factors3)]
      cols2 <- vector()
      for (i in 1:length(factors2)){
            cols2[i] <- which(names(data) == factors2[[i]][1])
      }
      len <- length(cols2)
      for (i in 1:length(factors3)) {
            cols2[i+len] <- which(names(data) == factors3[[i]][1])
      }
      cols2 <- cols2[!duplicated(cols2)]
      cols2 <- cols2[!(cols2 %in% cols1)]
      if (length(cols2) == 0) {
            stop("No distinguishing factors; please lower your cutoffs.")
      }
      if (length(cols2) == 1) {
            warning("You have only one distinguishing factor. Consider lowering your cutoffs.")
      }
      cols2 <- cols2[order(cols2)]
      len <- length(cols2)
      print(paste("Building", 2*len+2, "data sets"))
      modData <- list()
      for (i in 1:len) {
            modData[[i]] <- nhlShape(end, end, cols = c(cols1, cols2[i]), outcome = outcome, rm.nhlnum = F)
            modData[[i+len+1]] <- nhlShape(start, end, cols = c(cols1, cols2[i]), outcome = outcome,
                                           rm.nhlnum = F, rm.NA = FALSE)
            modData[[i+len+1]] <- subset(modData[[i+len+1]], modData[[i+len+1]]$nhl_num %in% modData[[1]]$nhl_num)
      }
      if (len > 1) {
            modData[[len+1]] <- nhlShape(end, end, cols = c(cols1, cols2), outcome = outcome, rm.nhlnum = F)
            modData[[2*len+2]] <- nhlShape(start, end, cols = c(cols1, cols2), outcome = outcome,
                                           rm.nhlnum = F, rm.NA = FALSE)
            modData[[2*len+2]] <- subset(modData[[2*len+2]], modData[[2*len+2]]$nhl_num %in% modData[[1]]$nhl_num)
      } else {
            modData[[2]] <- nhlShape(end, end, cols = cols1, outcome = outcome, rm.nhlnum = F)
            modData[[4]] <- nhlShape(start, end, cols = cols1, outcome = outcome,
                                           rm.nhlnum = F, rm.NA = FALSE)
            modData[[4]] <- subset(modData[[4]], modData[[4]]$nhl_num %in% modData[[1]]$nhl_num)
      }
      models <- list()
      print(paste("Building", 2*len+2, "models"))
      for(i in 1:(len+1)) {
            print(paste("Building model", i))
            models[[i]] <- nhlBuild(data = modData[[i]][, -1], distribution = distribution)
            print(paste("Building model", i+len+1))
            models[[i+len+1]] <- nhlBuild(data = modData[[i+len+1]], type = "gbm",
                                          distribution = distribution, n.trees = 10000,
                                          cv.folds = 5, n.cores = 4)
      }
      predData <- as.data.frame(modData[[1]]$nhl_num)
      names(predData)[1] <- "nhl_num"
      print("Assembling data for cumulative model")
      for (i in 1:(2*len+2)) {
            temp <- predict(models[i], modData[[i]], type = "response")[[1]]
            temp <- as.data.frame(cbind(modData[[i]]$nhl_num, temp))
            names(temp)[1] <- "nhl_num"
            predData <- merge(predData, temp)
            if (i <= len + 1) {
                  names(predData)[i+1] <- paste("rf", i, sep = ".")
            } else {
                  names(predData)[i+1] <- paste("gbm", i-len-1, sep = ".")
            }
      }
      predData <- merge(predData, modData[[1]][, c(1, length(names(modData[[1]])))])
      models[["columns"]] <- c(cols1, cols2)
      print("Building cumulative model")
      models[["model"]] <- nhlBuild(data = predData[, -1], type = "gbm",
                                    distribution = distribution, n.trees = 15000,
                                    cv.folds = 5, n.cores = 4)
      return(models)
}

nhlPredict <- function (start, end, outcome, models, data = skaterstats) {
      rfData <- nhlShape(end, end, cols = models[["columns"]], rm.nhlnum = FALSE)
      gbmData <- nhlShape(start, end, cols = models[["columns"]], rm.nhlnum = FALSE)
      rfData <- rfData[rfData$nhl_num %in% gbmData$nhl_num, ]
      predData <- as.data.frame(gbmData$nhl_num)
      names(predData)[1] <- "nhl_num"
      len <- (length(models) - 2)/2
      for (i in 1:len) {
            predData <- cbind(predData, predict(models[i], rfData, type = "response"))
            predData <- cbind(predData, predict(models[len+i], gbmData, type = "response"))
            names(predData)[2*i] <- paste("rf", i, sep = ".")
            names(predData)[2*i+1] <- paste("gbm", i, sep = ".")
      }
      predData$out <- predict(models[["model"]], predData, type = "response")
      names(predData)[2*len+2] <- names(data)[outcome]
      return(predData[, c(1, 2*len+2)])
}