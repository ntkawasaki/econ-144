#***********************************************
# Randall R. Rojas
# Email: rrojas@econ.ucla.edu
# Date: 04/06/2015
# Comment(s): ACF and PACF R code example for Econ 144.
# Data File(s): From Yahoo Finance use and stock/index
#***********************************************


# Clear all variables and prior sessions
rm(list = ls(all = TRUE))
setwd("/Users/noahkawasaki/Desktop/ECON 144/Week 2")


# Load Libraries
library("fImport")
library(fOptions)
#library(RQuantLib)
library(nlstools)
library(tseries)
library(Quandl)
library(zoo)
library(PerformanceAnalytics)
library(quantmod)
library(car)
library(FinTS)
library(fOptions)
library(forecast)
require(stats)
library(stockPortfolio)
library(vars)
library(tseries, quietly = T)
library(forecast, quietly = T)
library(XML)
library(fBasics)
library(timsac)
library(TTR)
library(lattice)
library(foreign)
library(MASS)
require(stats4)
library(KernSmooth)
library(fastICA)
library(cluster)
library(leaps)
library(mgcv)
library(rpart)
require("datasets")
require(graphics)
library(RColorBrewer)

# Load the Data
getSymbols("AAPL")
quartz()
chartSeries(AAPL, subset='last 3 months')
addBBands()

# Compare the following two plot and try to identify what is wrong in the first one.
quartz()
plot(AAPL)

quartz()
plot(AAPL$AAPL.Adjusted)

# Let's look at the ACF and PACF of the (a) Stock Prices vs (b) The Returns (using the 1st Difference)

# Note: I need to interpolate because e.g., weekends and holidays return 'NAs' for the stock price.
ts_daily_prices <- get.hist.quote('AAPL', quote = "AdjClose", compression = "d",retclass="ts")
interp_ts_daily_prices=interpNA(ts_daily_prices,method="linear")

?interpNA

quartz()
par(mfrow=c(2,1))
acf(interp_ts_daily_prices,main="ACF of the S&P500 Index Prices")
acf(diff(interp_ts_daily_prices),main="ACF of the First Difference of the S&P500 Index Prices")

quartz()
par(mfrow=c(2,1))
pacf(interp_ts_daily_prices,main="PACF of the S&P500 Index Prices")
pacf(diff(interp_ts_daily_prices),main="PACF of the First Difference of the S&P500 Index Prices")











