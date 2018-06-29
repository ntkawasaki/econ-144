#***********************************************
# Randall R. Rojas
# Email: rrojas@econ.ucla.edu
# Date: 05/28/2015
# Comment(s): R code for fitting MAPA (Multiple Aggregation Prediction Algorithm) Model
# Data File(s): housecomp.dat
#***********************************************
# Variable Definitions
# Original Data are from: http://www.freddiemac.com/finance/fmhpi/archive.html
# MSA (1975-Current), MSA=Metropolitan State Area
# hpi.xls[,1] = Los Angles County -House Price Index
# hpi.xls[,2] = Riverside County -House Price Index
# We will be using a new library called 'MAPA'
#************************************************

# Set your 'working directory' to the folder where all the data and respective codes are located.
setwd("/Users/noahkawasaki/Desktop/ECON 144/Week 7")

# Clear all variables and prior sessions
rm(list=ls(all=TRUE))

# Load Libraries
library(lattice)
library(foreign)
library(MASS)
library(car)
require(stats)
require(stats4)
library(KernSmooth)
library(fastICA)
library(cluster)
library(leaps)
library(mgcv)
library(rpart)
library(pan)
library(mgcv)
library(DAAG)
library(TTR)
library(tis)
require("datasets")
require(graphics)
library(forecast)
#install.packages("astsa")
#require(astsa)
library(xtable)
# New libraries added:
library(stats)
library(TSA)
library(timeSeries)
library(fUnitRoots)
library(fBasics)
library(tseries)
library(timsac)
library(TTR)
library(fpp)
library(strucchange)
#library(MSBVAR)
library(vars)
library(lmtest)
library(dlnm)
# Note: The library hts = Hierarchical Time Series can be used for forecast combination
library(hts)
library(MAPA)
library(readxl)
set.seed(1)



data = read_xls("hpi.xls", col_names = c("V1", "V2"))
data
LA_ts = ts(data$V1,start=1975,freq=12)
RI_ts = ts(data$V2,start=1975,freq=12)
plot(RI_ts)
plot(LA_ts)


# Detailed view of the data at each temporal aggregation level (daily, monthly, quarterly, annual)
# Note: paral = 2 --> Run in parallel cluster mode with e.g., in my case 4 cores
mapasimple(LA_ts, outplot=2,paral=2)

#Dynamic Fit to the data
mapafor(LA_ts, mapafit, ifh=12,fh=0)

# Best fit MAPA model by temporal decomposition + Forecast
# N=None, A=Additive, M=Multiplicative, d=Damped
# Exmaple: MAM = Holt-Winters
mapafit <- mapaest(LA_ts, paral=2)
mapafor(LA_ts, mapafit)

# Forecast with Error Bands
mapa(LA_ts,conf.lvl=c(0.8,0.9,0.95,0.99), paral=2)


