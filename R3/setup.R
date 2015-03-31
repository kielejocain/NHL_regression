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
library(gridExtra)

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

nhlClean <- function(data = skaterstats) {
      output <- data[, -c(3:5, 38:41, 49)]
      output$es_goals <- output$goals - output$pp_goals - output$sh_goals
      output$pp_assists <- output$pp_points - output$pp_goals
      output$sh_assists <- output$sh_points - output$sh_goals
      output$es_assists <- output$assists - output$pp_assists - output$sh_assists
      output$es_points <- output$points - output$pp_points - output$sh_points
      for (i in 4:length(output)) {
            if (i != 16) {
                  output[, i] <- output[, i] / output$games_played
            }
      }
      output[output$season != 2013, 3] <- output[output$season != 2013, 3] / 82
      output[output$season == 2013, 3] <- output[output$season == 2013, 3] / 48
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

nhlBuild <- function(data, method = "rf", perc = 1, seed = NA, ...) {
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
      model <- train(outcome ~ ., data = training, method = method, ...)
      if (perc < 1) {
            corr <- cor(testing$outcome, predict(model, testing))
      }
      else {
            corr <- cor(training$outcome, predict(model,training))
      }
      print(corr)
      return(model)
}


nhlAnalyze <- function(rfdata, gbmdata, gbm.cutoff = 4, rf.cutoff = 100, seed = NA) {
      if (!is.na(seed)) {
            set.seed(seed)
      }
      fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
      rfmod1 <- nhlBuild(data = rfdata, perc = 0.7, importance = TRUE,
                         trControl = fitControl)
      rfmod2 <- nhlBuild(data = rfdata, perc = 0.7, importance = TRUE, 
                         trControl = fitControl)
      rfmod3 <- nhlBuild(data = rfdata, perc = 0.7, importance = TRUE,
                         trControl = fitControl)
      rfmod4 <- nhlBuild(data = rfdata, perc = 0.7, importance = TRUE,
                         trControl = fitControl)
      rfsum1 <- data.frame(var = row.names(varImp(rfmod1)[['importance']]),
                           imp = varImp(rfmod1)[['importance']]$Overall)
      rfsum2 <- data.frame(var = row.names(varImp(rfmod2)[['importance']]),
                           imp = varImp(rfmod2)[['importance']]$Overall)
      rfsum3 <- data.frame(var = row.names(varImp(rfmod3)[['importance']]),
                           imp = varImp(rfmod3)[['importance']]$Overall)
      rfsum4 <- data.frame(var = row.names(varImp(rfmod4)[['importance']]),
                           imp = varImp(rfmod4)[['importance']]$Overall)
      rfsum <- merge(rfsum1, rfsum2, by = 1, suffixes = c(".1", ".2"))
      rfsumm <- merge(rfsum3, rfsum4, by = 1, suffixes = c(".3", ".4"))
      rfsum <- merge(rfsum, rfsumm)
      rfsum$rf.tot <- rfsum$imp.1 + rfsum$imp.2 + rfsum$imp.3 + rfsum$imp.4
      rfsum <- rfsum[order(rfsum$rf.tot, decreasing = TRUE),]
      rfsum <- rfsum[rfsum$rf.tot > rf.cutoff, c(1, 6)]
      row.names(rfsum) <- NULL
      gbmmod1 <- nhlBuild(data = gbmdata, method = "gbm", perc = 0.7, 
                          trControl = fitControl)
      gbmmod2 <- nhlBuild(data = gbmdata, method = "gbm", perc = 0.7, 
                          trControl = fitControl)
      gbmmod3 <- nhlBuild(data = gbmdata, method = "gbm", perc = 0.7, 
                          trControl = fitControl)
      gbmmod4 <- nhlBuild(data = gbmdata, method = "gbm", perc = 0.7, 
                          trControl = fitControl)
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

nhlModel <- function(start, end, outcome, data = skaterstats, cols = NA, methods = c("rf", "gbm"), controls = NA,
                     rf.cutoff = 200, gbm.cutoff = 4, seed = NA) {
      if (is.na(cols[1])) {
            rfdata <- nhlShape(end, end, outcome = outcome, data = data)
            gbmdata <- nhlShape(start, end, outcome = outcome, data = data)
            print(paste("Preliminary analysis on", names(data)[outcome]))
            factorMatrix <- nhlAnalyze(rfdata, gbmdata, gbm.cutoff = gbm.cutoff,
                                       rf.cutoff = rf.cutoff, seed = seed)
            print(factorMatrix)
            factors <- as.vector(factorMatrix[complete.cases(factorMatrix), 1])
            factors <- strsplit(factors, ".", fixed = TRUE)
            cols <- vector()
            for (i in 1:length(factors)) {
                  cols[i] <- which(names(data) == factors[[i]][1])
            }
            cols <- c(1, 2, cols[order(cols)])
            if (length(cols) == 2) {
                  print(factorMatrix[1, ])
                  stop("No factors; please lower your cutoffs.")
            }   
      }
      print("Building data set")
      modData <- nhlShape(start, end, cols = cols, outcome = outcome,
                                           data = data, rm.nhlnum = F, rm.NA = FALSE)
      inTrain <- createDataPartition(modData$outcome, list = FALSE)
      models <- list()
      print("Building models")
      for(i in 1:length(methods)){
            print(paste("Applying", methods[i], "method"))
            if (!is.null(controls[[i]])){
                  models[[methods[i]]] <- nhlBuild(data = modData[inTrain, -1], method = methods[i],
                                                   trControl = controls[[i]])
            } else {
                  models[[methods[i]]] <- nhlBuild(data = modData[inTrain, -1], method = methods[i])
            }
      }
      predData <- as.data.frame(modData[-inTrain, 1])
      names(predData)[1] <- "nhl_num"
      print("Assembling data for cumulative model")
      for (i in 1:length(methods)) {
            temp <- predict(models[[methods[i]]], modData[-inTrain, ])
            temp <- as.data.frame(cbind(modData[-inTrain, 1], temp))
            names(temp)[1] <- "nhl_num"
            predData <- merge(predData, temp)
            names(predData)[i+1] <- methods[i]
      }
      predData <- merge(predData, modData[-inTrain, c(1, length(names(modData)))])
      models[["cols"]] <- cols
      print("Building cumulative model")
      models[["model"]] <- nhlBuild(data = predData, method = "lm")
      return(models)
}

nhlPredict <- function (start, end, outcome, models, data = skaterstats, naive = TRUE) {
      cols = models[["cols"]]
      if (!is.na(outcome)) {
            cleanData <- nhlShape(start, end, cols = cols, outcome = outcome, data = data, rm.nhlnum = FALSE)      
      } else {
            cleanData <- nhlShape(start, end, cols = cols, data = data, rm.nhlnum = FALSE)
      }
      predData <- as.data.frame(cleanData$nhl_num)
      names(predData) <- c("nhl_num")
      for (i in 1:(length(models) - 2)) {
            predData <- cbind(predData, predict(models[[names(models)[i]]], cleanData))
            names(predData)[i+1] <- names(models)[i]
      }
      predData$out <- predict(models[["model"]], predData)
      names(predData)[length(models)] <- "cumulative"
      if (!is.na(outcome)) {
            predData <- cbind(predData, cleanData$outcome)
            names(predData)[length(names(predData))] <- c("outcome")
      }
      if (naive) {
            precursor <- which(names(cleanData) == paste(names(data)[outcome], ".1", sep=""))
            predData <- cbind(predData, cleanData[, precursor])
            names(predData)[length(names(predData))] <- c("naive")
      }
      return(predData)
}

nhlCorr <- function (start, end, outcome, models, data = skaterstats) {
      cols = models[["cols"]]
      output = as.data.frame(matrix(nrow = (end - start + 1), ncol = length(names(models)) + 1))
      names(output)[1] = "naive"
      for (i in 1:(length(names(models)) - 2)) {
            names(output)[i+1] <- names(models)[i]
      }
      names(output)[length(names(models))] <- "cumulative"
      names(output)[length(names(models)) + 1] <- "mean"
      counter = 1
      for (i in start:end) {
            cleanData <- nhlShape(i, i, cols = cols, outcome = outcome, data = data, rm.nhlnum = F)
            corData <- nhlPredict(i, i, outcome = outcome, models = models, data = data)
            corData$mean <- rowMeans(corData[, -c(1, (length(names(corData))-2):length(names(corData)))])
            precursor <- which(names(cleanData) == paste(names(data)[outcome], ".1", sep=""))
            output[counter, 1] <- cor(corData$outcome, corData$naive)
            for (model in 1:(length(names(models)) - 2)) {
                  output[counter, model + 1] <- cor(corData$outcome, corData[, model + 1])
            }
            output$cumulative[counter] <- cor(corData$outcome, corData$cumulative)
            output$mean[counter] <- cor(corData$outcome, corData$mean)
            counter <- counter + 1
      }
      row.names(output) <- c(start:end) + 1
      return(output)
}