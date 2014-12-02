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

nhlBuild <- function(data, type = "rf", perc = 0.5, seed = NA) {
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
            model <- randomForest(outcome ~ ., data = training, importance = TRUE)
      } else if (type == "gbm") {
            model <- gbm(outcome ~ ., data = training, n.trees = 10000,
                         cv.folds = 5, n.cores = 4)
      }
      if (perc < 1) {
            corr <- cor(testing$outcome, predict(model, testing))
            print(corr)
      }
      return(model)
      }
