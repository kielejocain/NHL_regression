source("~/workspace/NHL_regression/R/setup.R")
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
rs <- dbSendQuery(conn, statement = "SELECT * FROM skatpred15
                  ORDER BY nhl_num;")
output <- fetch(rs, n = -1)
dbClearResult(rs)
skaterstats <- skaterstats[, -c(3:5, 38:41)]
shotsModel <- nhlModel(2012, 2013, 15, seed = 681321)
output <- merge(output, nhlPredict(2013, 2014, 15, shotsModel))
output$shot_pct <- output$goals / output$shots * 100
dbWriteTable(conn, "skatpred15", output, overwrite=TRUE, row.names = FALSE)
dbDisconnect(conn)