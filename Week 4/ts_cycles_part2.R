#***********************************************
# Randall R. Rojas
# Email: rrojas@econ.ucla.edu
# Date: 04/22/2015
# Comment(s): R code example for characterizing cycles in ts data.
# Data File(s): The observations are generated via random walks.
#***********************************************

# Set your 'working directory' to the folder where all the data and respective codes are located.
setwd('/Users/noahkawasaki/Desktop/ECON 144/Week 4')

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
library("TTR")
library(tis)
require("datasets")
require(graphics)
library("forecast")
library(nlstools)

# Random Walk Simulation, Zt ~ N(0,1)

#1. Xt = Xt-1 + Zt
x = e = rnorm(1000)
for (t in 2:1000) x[t] = x[t-1] + e[t]
quartz()
plot(x, type="l")
acf(x)
acf(diff(x))

#2. Xt =  Zt
x = e = rnorm(1000)
for (t in 2:1000) x[t] = e[t]
quartz()
plot(x, type="l")
acf(x)
acf(diff(x))

#3. Xt = -0.3Xt-1 + Zt
x = e = rnorm(1000)
for (t in 2:1000) x[t] = -0.3*x[t-1] + e[t]
quartz()
plot(x, type="l")
acf(x)
acf(diff(x))

#4. Xt = sin(pi t/3) + Zt
x = e = rnorm(1000)
for (t in 2:1000) x[t] = sin(pi*t/3)+ e[t] -x[t-1]
par(mfrow=c(3,1))
plot(x, type='l',main="Original Time Series of the Data")
acf(x, main="ACF")
acf(diff(x), main="ACF (First Difference)", ylab=expression(ACF(Delta)))

# Plot the autocorrelation, autocovariance, and partial autocorrelation functions
quartz()
par(mfrow=c(2,2))
plot(x,type='l')
acf(x, type = "correlation")
acf(x, type = "covariance")
acf(x, type = "partial")

# Plot the autocorrlation function of the First and Second difference
quartz()
par(mfrow=c(2,1))
acf(diff(x))
acf(diff(diff(x)))



