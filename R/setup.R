## This script will load the NHL data and relevant functions that  are used in
## the manipulation and analysis of the data set.

library(doMC)
registerDoMC(4)
library(RPostgreSQL)
driv <- dbDriver("PostgreSQL")
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres", password = "hollyleaf", host = "localhost")
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

nhlShape <- function(year, cols, data = skaterstats){
      year <- as.integer(year)
      data <- subset(data, season = year)
      data$season <- season - data$season
      data <- data[, cols]
      data <- reshape(data, timevar = "season", idvar = "nhlnum",
                      direction = "wide")
      data <- data[, -c(1, 2)]
      return(data)
}
