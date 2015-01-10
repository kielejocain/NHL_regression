source("~/workspace/NHL_regression/R/setup.R")
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
rs <- dbSendQuery(conn, statement = "SELECT * FROM skatpred15
                  ORDER BY nhl_num;")
output <- fetch(rs, n = -1)
dbClearResult(rs)
skaterstats <- skaterstats[, -c(3:5, 38:41)]
teamGModel <- nhlModel(2012, 2013, 24, seed = 272791)
output <- merge(output, nhlPredict(2013, 2014, 24, teamGModel))
teamPPGModel <- nhlModel(2012, 2013, 25, seed = 947874)
output <- merge(output, nhlPredict(2013, 2014, 25, teamPPGModel))
teamGAModel <- nhlModel(2012, 2013, 26, seed = 835044)
output <- merge(output, nhlPredict(2013, 2014, 26, teamGAModel))
teamPPGAModel <- nhlModel(2012, 2013, 27, seed = 778293)
output <- merge(output, nhlPredict(2013, 2014, 27, teamPPGAModel))
output$plus_minus = output$team_goals_for - output$team_pp_goals_for -
      output$team_goals_against + output$team_pp_goals_against
dbWriteTable(conn, "skatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)