NHL_regression
==============

This repo is a collection of the various .Rmd and knitR-generated .pdf files that describe some basic regression analysis of the National Hockey League (NHL) data collected via the code found in my `NHL_sql` repo.  This analysis focuses mostly on skaters; goalie analysis will come later.

Philosophy
----------

Though several stats will be modeled purely against their historical data for a particular player on a per game basis, goals and assists (and therefore points) will be extrapolated from situational Time On Ice (TOI) data; in particular, we will build a prediction for games played and for situational (even/PP/SH) TOI game averages to arrive at expected TOI in each situation.  We will then attempt to predict situational scoring rates to arrive at both situational and total offensive numbers.  We will also account for position when doing these analyses.

File Structure
--------------

The `R/` folder contains code for importing the data from the PostgreSQL database as built in my `NHL_sql` repo.  We will include .Rmd, .md, and .pdf files in the `Predictions/` folder for each statistic modeled.

If you do not care to replicate the analysis, either the .md or .pdf files should be sufficiently legible.  If you'd like to run the analysis locally, first clone and run the code in `NHL_sql`, then weave the .Rmd files using knitR (RStudio has this built in).
