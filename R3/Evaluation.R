## Script to build graphs evaluating the model's performance.

# access data and predictions
library(doMC)
registerDoMC(4)
library(RPostgreSQL)
library(ggplot2)
library(gridExtra)
driv <- dbDriver("PostgreSQL")
conn <- dbConnect(driv, dbname = "nhltest", user = "postgres",
                  password = "hollyleaf", host = "localhost")
rs <- dbSendQuery(conn, statement = "SELECT * FROM skaterstats
                  WHERE season >= 2014;")
skaterstats <- fetch(rs, n = -1)
dbClearResult(rs)
rs <- dbSendQuery(conn, statement = "SELECT * FROM newskatpred15;")
predstats <- fetch(rs, n = -1)
dbClearResult(rs)
dbDisconnect(conn)

# reshape data to have one observation per skater
skaterstats <- reshape(skaterstats, timevar = "season", idvar = "nhl_num",
                  direction = "wide")
data <- merge(skaterstats, predstats, by = 1)

## build graphs
# ES TOI
data$es_toi <- data$games_played * data$es_toi
corr <- round(cor(data$es_toi.2014, data$es_toi.2015), digits = 4)
plotNaive <- ggplot(data, aes(x=es_toi.2014, y=es_toi.2015)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: ES TOI, 2014 vs. 2015 (r = ", corr, ")", sep = "")) +
      xlab("Even Strength Time On Ice in 2014") + ylab("Even Strength Time On Ice in 2015")
corr <- round(cor(data$es_toi.2015, data$es_toi), digits = 4)
plotHML <- ggplot(data, aes(x=es_toi, y=es_toi.2015)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("HockeyML Model: ES TOI, 2014 vs. 2015 (r = ", corr, ")", sep = "")) +
      xlab("Even Strength Time On Ice (Predicted)") + ylab("Even Strength Time On Ice (Actual)")
png(width = 960, height = 1220, 
    filename = "~/workspace/NHL_regression/graphics/Validation/2015_1_ESTOI.png")
grid.arrange(plotNaive, plotHML, ncol = 1)
dev.off()

# SH TOI
data$sh_toi <- data$games_played * data$sh_toi
corr <- round(cor(data$sh_toi.2014, data$sh_toi.2015), digits = 4)
plotNaive <- ggplot(data, aes(x=sh_toi.2014, y=sh_toi.2015)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: SH TOI, 2014 vs. 2015 (r = ", corr, ")", sep = "")) +
      xlab("Short-handed Time On Ice in 2014") + ylab("Short-handed Time On Ice in 2015")
corr <- round(cor(data$sh_toi.2015, data$sh_toi), digits = 4)
plotHML <- ggplot(data, aes(x=sh_toi, y=sh_toi.2015)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("HockeyML Model: SH TOI, 2014 vs. 2015 (r = ", corr, ")", sep = "")) +
      xlab("Short-handed Time On Ice (Predicted)") + ylab("Short-handed Time On Ice (Actual)")
png(width = 960, height = 1220, 
    filename = "~/workspace/NHL_regression/graphics/Validation/2015_2_SHTOI.png")
grid.arrange(plotNaive, plotHML, ncol = 1)
dev.off()

# PP TOI
data$pp_toi <- data$games_played * data$pp_toi
corr <- round(cor(data$pp_toi.2014, data$pp_toi.2015), digits = 4)
plotNaive <- ggplot(data, aes(x=pp_toi.2014, y=pp_toi.2015)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: PP TOI, 2014 vs. 2015 (r = ", corr, ")", sep = "")) +
      xlab("Power Play Time On Ice in 2014") + ylab("Power Play Time On Ice in 2015")
corr <- round(cor(data$pp_toi.2015, data$pp_toi), digits = 4)
plotHML <- ggplot(data, aes(x=pp_toi, y=pp_toi.2015)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("HockeyML Model: PP TOI, 2014 vs. 2015 (r = ", corr, ")", sep = "")) +
      xlab("Power Play Time On Ice (Predicted)") + ylab("Power Play Time On Ice (Actual)")
png(width = 960, height = 1220, 
    filename = "~/workspace/NHL_regression/graphics/Validation/2015_3_PPTOI.png")
grid.arrange(plotNaive, plotHML, ncol = 1)
dev.off()

# Shots
data$shots <- data$games_played * data$shots
corr <- round(cor(data$shots.2014, data$shots.2015), digits = 4)
plotNaive <- ggplot(data, aes(x=shots.2014, y=shots.2015)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("Naive Model: Shots, 2014 vs. 2015 (r = ", corr, ")", sep = "")) +
      xlab("Shots in 2014") + ylab("Shots in 2015")
corr <- round(cor(data$shots.2015, data$shots), digits = 4)
plotHML <- ggplot(data, aes(x=shots, y=shots.2015)) +
      geom_smooth() + geom_point() + 
      ggtitle(paste("HockeyML Model: Shots, 2014 vs. 2015 (r = ", corr, ")", sep = "")) +
      xlab("Shots (Predicted)") + ylab("Shots (Actual)")
png(width = 960, height = 1220, 
    filename = "~/workspace/NHL_regression/graphics/Validation/2015_x_Shots.png")
grid.arrange(plotNaive, plotHML, ncol = 1)
dev.off()