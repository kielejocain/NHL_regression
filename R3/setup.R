## This script will load the NHL data and relevant functions that  are used in
## the manipulation and analysis of the data set.
# load packages
library(doMC)
registerDoMC(4)
library(RPostgreSQL)
library(caret)
library(randomForest)
library(gbm)
library(gridExtra)

# load data
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
      # subset data to relevant seasons
      output <- subset(data, season >= start & season <= end)
      #convert season to number of seasons prior to target season
      output$season <- end + 1 - output$season
      # if asked, restrict columns
      if (!is.na(cols[1])) {
            output <- output[, cols]
      }
      output <- reshape(output, timevar = "season", idvar = "nhl_num",
                      direction = "wide")
      # if asked, add the values of the outcome
      if (!is.na(outcome)) {
            values <- subset(data, season == end+1)
            values <- values[, c(1, outcome)]
            names(values)[2] <- "outcome"
            output <- merge(output, values)
      }
      # unless requested, remove nhl_num column
      if (rm.nhlnum) {
            output <- output[, -1]
      }
      # unless requested, only deal in complete cases 
      if (rm.NA) {
            output <- output[complete.cases(output), ]
      }      
      return(output)
}


# the nhlClean function strips out unused columns and coverts all stats to
# per game averages.

nhlClean <- function(data = skaterstats) {
      # strip out team, shootout stats
      output <- data[, -c(3:5, 38:41, 49)]
      
      # extrapolate a few additional data points
      output$es_goals <- output$goals - output$pp_goals - output$sh_goals
      output$pp_assists <- output$pp_points - output$pp_goals
      output$sh_assists <- output$sh_points - output$sh_goals
      output$es_assists <- output$assists - output$pp_assists - output$sh_assists
      output$es_points <- output$points - output$pp_points - output$sh_points
      # divide all columns but shot_pct by games_played
      for (i in 4:length(output)) {
            if (i != 16) {
                  output[, i] <- output[, i] / output$games_played
            }
      }
      # convert games_played to a percentage (only 48 games in 2012-2013)
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
# method = which method to pass to the train function (string)
# perc = percent of the data set to commit to the training set (0 < numeric < 1)
# seed = desired seed, for random number generator, if applicable (integer)
# ... = additional arguments to pass to the train function
#
# returns a train model object.

nhlBuild <- function(data, method = "rf", perc = 1, seed = NA, ...) {
      # set seed for repeatable model building
      if (!is.na(seed)){
            set.seed(seed)
      }
      # if asked, restrict training set
      if (perc < 1) {
            inTrain <- createDataPartition(data$outcome, p = perc, list = FALSE)
            training <- data[inTrain, ]
            testing <- data[-inTrain, ]
      } else {
            training <- data
      }
      model <- train(outcome ~ ., data = training, method = method, ...)
      # compute correlation of model on testing set if it exists
      if (perc < 1) {
            corr <- cor(testing$outcome, predict(model, testing))
      }
      # otherwise, give correlation on full set
      else {
            corr <- cor(training$outcome, predict(model,training))
      }
      print(corr)
      return(model)
}


# the nhlAnalyze function is used to help with factor analysis.  It has been generalized
# in nhlAnalyze2.
#
# rfdata = data to plug into the random forest models (data frame)
# gbmdata = data to plug into the boosting models (data frame)
# x.cutoff = cutoff for importance variables of x-method models (numeric)
# seed = seed to use if reproducibility is desired (integer)
#
# returns a matrix with factor names and associated importance metrics

nhlAnalyze <- function(rfdata, gbmdata, gbm.cutoff = 4, rf.cutoff = 100, seed = NA) {
      # if asked, set seed
      if (!is.na(seed)) {
            set.seed(seed)
      }
      # set validation controls
      fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
      #build random forest models
      rfmod1 <- nhlBuild(data = rfdata, perc = 0.7, importance = TRUE,
                         trControl = fitControl)
      rfmod2 <- nhlBuild(data = rfdata, perc = 0.7, importance = TRUE, 
                         trControl = fitControl)
      rfmod3 <- nhlBuild(data = rfdata, perc = 0.7, importance = TRUE,
                         trControl = fitControl)
      rfmod4 <- nhlBuild(data = rfdata, perc = 0.7, importance = TRUE,
                         trControl = fitControl)
      # pull importance data
      rfsum1 <- data.frame(var = row.names(varImp(rfmod1)[['importance']]),
                           imp = varImp(rfmod1)[['importance']]$Overall)
      rfsum2 <- data.frame(var = row.names(varImp(rfmod2)[['importance']]),
                           imp = varImp(rfmod2)[['importance']]$Overall)
      rfsum3 <- data.frame(var = row.names(varImp(rfmod3)[['importance']]),
                           imp = varImp(rfmod3)[['importance']]$Overall)
      rfsum4 <- data.frame(var = row.names(varImp(rfmod4)[['importance']]),
                           imp = varImp(rfmod4)[['importance']]$Overall)
      # compile into one matrix
      rfsum <- merge(rfsum1, rfsum2, by = 1, suffixes = c(".1", ".2"))
      rfsumm <- merge(rfsum3, rfsum4, by = 1, suffixes = c(".3", ".4"))
      rfsum <- merge(rfsum, rfsumm)
      # compute totals
      rfsum$rf.tot <- rfsum$imp.1 + rfsum$imp.2 + rfsum$imp.3 + rfsum$imp.4
      # order factors by importance metric
      rfsum <- rfsum[order(rfsum$rf.tot, decreasing = TRUE),]
      # drop unimportant variables and individual model data
      rfsum <- rfsum[rfsum$rf.tot > rf.cutoff, c(1, 6)]
      row.names(rfsum) <- NULL
      # repeat for gbm models
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


# the nhlAnalyze2 function generalizes the functionality of nhlAnalyze by allowing
# a general method to be passed and additional parameters to be passed to train.
#
# data = the data to be analyzed; needs an 'outcome' factor (data frame)
# method = the method to be passed to train (string)
# cutoff = the minimum importance variable to keep (numeric)
# seed = seed to use if reproducibility is desired (integer)
# ... = additional parameters to pass to each train call
#
# returns a matrix with factor names and associated importance metrics

nhlAnalyze2 <- function(data, method = "rf", cutoff = 0, seed = NA, ...) {
      # if asked, set the seed
      if (!is.na(seed)) {
            set.seed(seed)
      }
      # build the models
      model1 <- nhlBuild(data = data, method = method, perc = 0.7, ...)
      model2 <- nhlBuild(data = data, method = method, perc = 0.7, ...)
      model3 <- nhlBuild(data = data, method = method, perc = 0.7, ...)
      model4 <- nhlBuild(data = data, method = method, perc = 0.7, ...)
      # retrieve importance data
      sum1 <- data.frame(var = row.names(varImp(model1)[['importance']]),
                         imp = varImp(model1)[['importance']]$Overall)
      sum2 <- data.frame(var = row.names(varImp(model2)[['importance']]),
                         imp = varImp(model2)[['importance']]$Overall)
      sum3 <- data.frame(var = row.names(varImp(model3)[['importance']]),
                         imp = varImp(model3)[['importance']]$Overall)
      sum4 <- data.frame(var = row.names(varImp(model4)[['importance']]),
                         imp = varImp(model4)[['importance']]$Overall)
      # collate and sum iportance data
      sum <- merge(sum1, sum2, by = 1, suffixes = c(".1", ".2"))
      summ <- merge(sum3, sum4, by = 1, suffixes = c(".3", ".4"))
      sum <- merge(sum, summ)
      sum$tot <- rowSums(sum[, 2:5])
      # order by total importance
      sum <- sum[order(sum$tot, decreasing = TRUE),]
      # strip out unimportant factors and individual model data
      sum <- sum[sum$tot > cutoff, c(1, 6)]
      row.names(sum) <- NULL
      return(sum)
}


# the function nhlModel builds individual models as asked, then ensembles them with a linear model.
#
# start = the first season of data to use as factors (integer)
# end = the final season of data to use as factors (integer)
# outcome = the column number in the data to be used as the target variable (integer)
# cols = the columns to be used by each method (list of integer vectors named after methods)
# data = the data set (data frame)
# methods = the methods to train as first-level models (string vector)
# controls = the trControl objects to use for each method (list of trControl objects)
# seed = seed to use if reproducibility is desired (integer)
#
# returns a list object with each first-level model, the columns used, and the final model

nhlModel <- function(start, end, outcome, cols, data = skaterstats, methods = c("rf", "gbm"), controls = NA,
                     seed = NA) {
      print("Building data sets")
      # shape and structure the data
      modData <- list()
      for (i in 1:length(methods)) {
            modData[[i]] <- nhlShape(start, end, cols = cols[[methods[i]]], outcome = outcome,
                                data = data, rm.nhlnum = F, rm.NA = FALSE)
      }
      # define training set
      inTrain <- createDataPartition(modData[[1]]$outcome, list = FALSE)
      models <- list()
      print("Building models")
      for(i in 1:length(methods)){
            print(paste("Applying", methods[i], "method"))
            if (!is.null(controls[[i]])){
                  models[[methods[i]]] <- nhlBuild(data = modData[[i]][inTrain, -1], method = methods[i],
                                                   trControl = controls[[i]])
            } else {
                  models[[methods[i]]] <- nhlBuild(data = modData[[i]][inTrain, -1], method = methods[i])
            }
      }
      # collate first-level model predictions to build ensemble model
      predData <- as.data.frame(modData[[1]][-inTrain, 1])
      names(predData)[1] <- "nhl_num"
      print("Assembling data for cumulative model")
      for (i in 1:length(methods)) {
            temp <- predict(models[[methods[i]]], modData[[i]][-inTrain, ])
            temp <- as.data.frame(cbind(modData[[i]][-inTrain, 1], temp))
            names(temp)[1] <- "nhl_num"
            predData <- merge(predData, temp)
            names(predData)[i+1] <- methods[i]
      }
      predData <- merge(predData, modData[[1]][-inTrain, c(1, length(names(modData[[1]])))])
      models[["cols"]] <- cols
      print("Building cumulative model")
      models[["model"]] <- nhlBuild(data = predData, method = "lm")
      return(models)
}


# the nhlPredict function uses a list of models as output by the nhlModel function to
# predict the outcome.
#
# start = the first season of data to use as factors (integer)
# end = the final season of data to use as factors (integer)
# outcome = the column number in the data to be used as the target variable (integer)
# models = list of first-level models and the ensemble model, as well as the columns used
# to build the models (list)
# data = the data set (data frame)
# naive = whether to include the corresponding data from the previous season as a
# simplistic model (boolean)
#
# returns a data frame with the predictions of each of the models in the list

nhlPredict <- function (start, end, outcome, models, data = skaterstats, naive = TRUE) {
      cols = models[["cols"]]
      # shape the data to be fed to each first-level model
      cleanData <- list()
      for (i in 1:(length(names(models)) - 2)) {
            cleanData[[i]] <- nhlShape(start, end, cols = cols[[names(models[i])]], 
                                       outcome = outcome, data = data, rm.nhlnum = F)
      }
      # compute and collate collate first-level predictions
      predData <- as.data.frame(cleanData[[1]]$nhl_num)
      names(predData) <- c("nhl_num")
      for (i in 1:(length(models) - 2)) {
            predData <- cbind(predData, predict(models[[names(models)[i]]], cleanData[[i]]))
            names(predData)[i+1] <- names(models)[i]
      }
      # compute the ensemble predictions
      predData$cumulative <- predict(models[["model"]], predData)
      # add in the actual targets
      predData <- cbind(predData, cleanData[[1]]$outcome)
      names(predData)[length(names(predData))] <- c("outcome")
      # unless requested, add in the "naive" model
      if (naive) {
            # find which column in the data set contains the target variable
            precursor <- which(names(cleanData[[1]]) == paste(names(data)[outcome], ".1", sep=""))
            predData <- cbind(predData, cleanData[[1]][, precursor])
            names(predData)[length(names(predData))] <- c("naive")
      }
      return(predData)
}


# the nhlCorr function outputs the correlations of models in a nhlModel-built list
# to the target data over a list of seasons.
#
# start = the first season of data to use as factors (integer)
# end = the final season of data to use as factors (integer)
# outcome = the column number in the data to be used as the target variable (integer)
# models = list of first-level models and the ensemble model, as well as the columns used
# data = the data sets (data frame)
#
# returns a data frame with correlations of each model to the data in each season

nhlCorr <- function (start, end, outcome, models, data = skaterstats) {
      cols = models[["cols"]]
      # build the data frame
      output = as.data.frame(matrix(nrow = (end - start + 1), ncol = length(names(models)) + 1))
      names(output)[1] = "naive"
      for (i in 1:(length(names(models)) - 2)) {
            names(output)[i+1] <- names(models)[i]
      }
      names(output)[length(names(models))] <- "cumulative"
      names(output)[length(names(models)) + 1] <- "mean"
      # row counter
      counter = 1
      for (year in start:end) {
            # build list of data to feed to first-level models
            cleanData <- list()
            for (i in 1:(length(names(models)) - 2)) {
                  cleanData[[i]] <- nhlShape(year, year, cols = cols[[names(models[i])]], 
                                             outcome = outcome, data = data, rm.nhlnum = F)
            }
            # compute the predictions of the models
            corData <- nhlPredict(year, year, outcome = outcome, models = models, data = data)
            # compute a mean ensemble
            corData$mean <- rowMeans(corData[, -c(1, (length(names(corData))-2):length(names(corData)))])
            # find the column in data that contains the target variable
            precursor <- which(names(cleanData[[1]]) == paste(names(data)[outcome], ".1", sep=""))
            # put the responses in the output
            output[counter, 1] <- cor(corData$outcome, corData$naive)
            for (model in 1:(length(names(models)) - 2)) {
                  output[counter, model + 1] <- cor(corData$outcome, corData[, model + 1])
            }
            output$cumulative[counter] <- cor(corData$outcome, corData$cumulative)
            output$mean[counter] <- cor(corData$outcome, corData$mean)
            # iterate and repeat
            counter <- counter + 1
      }
      row.names(output) <- c(start:end) + 1
      return(output)
}