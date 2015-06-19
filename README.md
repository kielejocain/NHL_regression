NHL_regression
==============


This repo consists of various attempts to predict NHL fantasy statistics using machine learning techniques in R.  

Philosophy
----------

Though several stats will be modeled purely against their historical data for a particular player on a per game basis, goals and assists (and therefore points) will be extrapolated from situational Time On Ice (TOI) data; in particular, we will build a prediction for games played and for situational (even/PP/SH) TOI game averages to arrive at expected TOI in each situation.  We will then attempt to predict situational scoring rates to arrive at both situational and total offensive numbers.  We will also account for position when doing these analyses.

File Structure
--------------

The `R/` folder contains my first basic attempt using linear regression so that I could practice making .Rmd files, and can be safely ignored.

The `R2/` folder is an aborted attempt to restart; again I leave it in for completeness, but can be ignored.

The `R3/` folder contains the first full attempt at a model, the results of which are at [hockeyml.com](http://www.hockeyml.com). The `source.R` file contains all the custom functions used in the analyis files.  Each of the numbered files are analyses for statistics that should be run in order, as sometimes later analyses use results of previous ones. 

Replication
-----------

If you'd like to run the analysis locally, first clone and run the code in `NHL_sql`, then run each of the numbered `.R` files in order (will take a while). 
