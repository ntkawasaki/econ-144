#***********************************************
# Randall R. Rojas
# Email: rrojas@econ.ucla.edu
# Date: 04/20/2015
# Comment(s): R code example for fitting/forecasting a seasonality to ts data.
# Data File(s): beer.csv, and housing.dat
#***********************************************
# Variable Definitions
# beer = monthly beer production in Australia from Jan 1956 - Aug 1995
# house = monthly housing starts from 1946 to 1993
#************************************************

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
#install.packages("astsa")
#require(astsa)
library(RColorBrewer)
library(plotrix)
library(nlstools)

# Basic Seasonality Example using 'tslm'
y=ts(rnorm(120,0,3) + 20*sin(2*pi*(1:120)/12), frequency=12)  #rnorm adds noise
?tslm
y
fit1=tslm(y~trend)
summary(fit1)

fit2=tslm(y~season)
summary(fit2)

fit3=tslm(y~trend+season)
summary(fit3)

quartz()
par(mfrow=c(3,1))
plot(y,main="Time Series Data: Trend")
lines(fit1$fitted.values, col="red")

plot(y,main="Time Series Data: Seasonality")
lines(fit2$fitted.values, col="red")

plot(y,main="Time Series Data: Trend + Seasonality")
lines(fit3$fitted.values, col="red")

# Read in the beer data into a data file
beer=read.csv("beer.csv",header=T,dec=",",sep=";")
beer=ts(beer[,1],start=1956,freq=12)
beer
lbeer<-log(beer)

# Compare 3 different fit models:
fit1=tslm(lbeer ~ trend)
summary(fit1)

fit2=tslm(lbeer ~ season)
summary(fit2)

fit3=tslm(lbeer ~ trend + season)
summary(fit3)


quartz()
par(mfrow=c(3,1))
plot(lbeer,main="Time Series Data: Trend")
lines(fit1$fitted.values, col="red")
plot(lbeer,main="Time Series Data: Seasonality")
lines(fit2$fitted.values, col="red")
plot(lbeer,main="Time Series Data: Trend + Seasonality")
lines(fit3$fitted.values, col="red")

AIC(fit1,fit2,fit3)
BIC(fit1,fit2,fit3)

# Compute forecasts based on the 3 fit models:
quartz()
par(mfrow=c(3,1))
plot(forecast(fit1,h=60),main="Model 1: Forecast Trend")
lines(fit1$fitted.values, col="red")
plot(forecast(fit2,h=60),main="Model 2: Forecast Seasonality")
lines(fit2$fitted.values, col="red")
plot(forecast(fit3,h=60),main="Model 3: Forecast Trend + Seasonality")
lines(fit3$fitted.values, col="red")


# The forecast above can be improved considerably via 'ets'
?ets
fit=ets(lbeer)
quartz()
plot(fit)
accuracy(fit)
plot(forecast(fit,level=c(50,80,95)))

#Plot the seasonal factors:
quartz()
fit=tslm(lbeer ~ season+0)
plot(fit$coef,type='l',ylab='Seasonal Factors', xlab="Season",lwd=2, main="Plot of Seasonal Factors")

#--------Book Example: Housing Starts----------------
house<-read.table("housing.dat")
housets<-ts(house[,1],start=1946,freq=12)
t<-seq(1946,1993.12,length=length(housets))
quartz()
plot(housets)
quartz()
plot(housets[200:256],type="l") #zoom in

# Seasonal Decomposition of Time Series by Loess
# Trend is not deterministic, looks stochastic
quartz()
plot(stl(housets,s.window="periodic"))
forecast(housets)
summary(forecast(housets))

fit1=tslm(housets ~ trend + season+0)
fit2=tslm(housets ~ trend+0)
fit3=tslm(housets ~ season) 

quartz()
par(mfrow=c(3,1))
plot(forecast(fit1,h=24),main="Model 1: Forecast Trend + Seasonality")
lines(fit1$fitted.values,col="red")
plot(forecast(fit2,h=24),main="Model 2: Forecast Trend Only")
lines(fit2$fitted.values,col="red")
plot(forecast(fit3,h=24),main="Model 2: Forecast Seasonality Only")
lines(fit3$fitted.values,col="red")

AIC(fit1,fit2,fit3)
BIC(fit1,fit2,fit3)

#Plot the seasonal factors:
quartz()
par(mfrow=c(2,1))
plot(fit3$coef,type='l',ylab='Seasonal Factors', xlab="Season",lwd=2, main="Plot of Seasonal Factors")
hist(fit3$res,main="Histogram of Residuals",col="skyblue3")

# We can improve the forecast using ets
quartz()
plot(housets, s.window="periodic")
forecast(housets)
plot(forecast(housets))


fit=ets(housets)
quartz()
plot(fit)
accuracy(fit)
plot(forecast(fit,level=c(50,80,95),h=12))

