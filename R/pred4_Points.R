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
shPointsModel <- nhlModel(2012, 2013, 12, seed = 409762, distribution = "poisson")
esPointsModel <- nhlModel(2012, 2013, 42, seed = 832)