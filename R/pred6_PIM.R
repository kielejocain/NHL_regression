source("~/workspace/NHL_regression/R/setup.R")
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
rs <- dbSendQuery(conn, statement = "SELECT * FROM skatpred15
                  ORDER BY nhl_num;")
output <- fetch(rs, n = -1)
dbClearResult(rs)
skaterstats <- skaterstats[, -c(3:5, 38:41)]
minorsModel <- nhlModel(2012, 2013, 19, seed = 81235)
output <- merge(output, nhlPredict(2013, 2014, 19, minorsModel))
majorsModel <- nhlModel(2012, 2013, 20, seed = 169, distribution = "poisson")
output <- merge(output, nhlPredict(2013, 2014, 20, majorsModel))
miscModel <- nhlModel(2012, 2013, 21, seed = 143728, distribution = "poisson")
output <- merge(output, nhlPredict(2013, 2014, 21, miscModel))
gameMiscModel <- nhlModel(2012, 2013, 22, seed = 28444, distribution = "poisson")
output <- merge(output, nhlPredict(2013, 2014, 22, gameMiscModel))
output$penalty_minutes = 2 * output$minors + 5 * output$majors + 
      10 * (output$misconducts + output$game_misconducts)
dbWriteTable(conn, "skatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)