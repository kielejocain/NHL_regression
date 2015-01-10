source("~/workspace/NHL_regression/R/setup.R")
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
rs <- dbSendQuery(conn, statement = "SELECT * FROM skatpred15
                  ORDER BY nhl_num;")
output <- fetch(rs, n = -1)
dbClearResult(rs)
skaterstats <- skaterstats[, -c(3:5, 38:41)]
skaterstats$es_points <- skaterstats$points - skaterstats$pp_points - skaterstats$sh_points
ppPointsModel <- nhlModel(2012, 2013, 10, seed = 61853)
output <- merge(output, nhlPredict(2013, 2014, 10, ppPointsModel))
shPointsModel <- nhlModel(2012, 2013, 12, seed = 409762, distribution = "poisson")
output <- merge(output, nhlPredict(2013, 2014, 12, shPointsModel))
esPointsModel <- nhlModel(2012, 2013, 42, seed = 832)
output <- merge(output, nhlPredict(2013, 2014, 42, esPointsModel))
output$points <- output$es_points + output$pp_points + output$sh_points
output$es_assists <- output$es_points - output$es_goals
output$pp_assists <- output$pp_points - output$pp_goals
output$sh_assists <- output$sh_points - output$sh_goals
output$assists <- output$es_assists + output$pp_assists + output$sh_assists
dbWriteTable(conn, "skatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)