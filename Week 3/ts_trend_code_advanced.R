#***********************************************
# Randall R. Rojas
# Email: rrojas@econ.ucla.edu
# Date: 04/13/2015
# Comment(s): R code example for fitting/forecasting a trend to ts data.
# Data File(s): beer.csv
#***********************************************
# Variable Definitions
# beer = monthly beer production in Australia from Jan 1956 - Aug 1995
#************************************************

# For reference, go to:
#http://www.statoek.wiso.uni-goettingen.de/veranstaltungen/zeitreihen/sommer03/ts_r_intro.pdf
setwd('/Users/noahkawasaki/Desktop/ECON 144/Week 3')

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


# Read in the data into a data file
beer=read.csv("beer.csv",header=T,dec=",",sep=";")
beer=ts(beer[,1],start=1956,freq=12)
plot(beer,xlab="Year", ylab="Monthly Beer Production")

#-------------[1] TREND FITTING--------------

# Mode1: Log-quadratic model
lbeer<-log(beer)
t<-seq(1956,1995.2,length=length(beer))
t2<-t^2
quartz()
plot(lbeer,xlab="Year", ylab="Log(Beer Production)")
m1=lm(lbeer~t+t2)
lines(t,m1$fit,col="red3",lwd=2)

acf(m1$residuals, 100)
pacf(m1$residuals, 100)

# Model 2: Log-quadratic-periodic
sin.t<-sin(2*pi*t)
cos.t<-cos(2*pi*t)
quartz()
plot(lbeer,xlab="Year", ylab="Log(Beer Production)")
m2=lm(lbeer~t+t2+sin.t+cos.t)
lines(t, m2$fit,col="red3",lwd=2)

acf(m2$residuals, 100)
pacf(m2$residuals, 100)


#-------------[2] MODEL SELECTION--------------
# Compare models using AIC and BIC
AIC(m1,m2)
BIC(m1,m2)

#-------------[3] Holt-Winters Filter--------------
HoltWinters(beer)
quartz()
plot(beer,xlab="Year", ylab="Beer Production")
lines(HoltWinters(beer)$fitted[,1],col="red")

#Try Holt-Winters Prediction
beer.hw<-HoltWinters(beer)
predict(beer.hw,n.ahead=12)
quartz()
plot(beer,xlim=c(1956,1999),xlab="Year", ylab="Beer Production")
lines(predict(beer.hw,n.ahead=48),col=2)

# Forecast
quartz()
par(mfrow=c(2,1))
plot(beer,main="Data",xlab="Year", ylab="Beer Production")
plot(forecast(beer),main="Data with Respective Point and Interval Forecasts",xlab="Year", ylab="Beer Production",shadecols="oldstyle")
# For further info on parameters: http://www.inside-r.org/packages/cran/forecast/docs/plot.forecast


quartz()
acf(HoltWinters(beer)$fitted[,-1]-beer, 100)
quartz()
pacf(HoltWinters(beer)$fitted[,-1]-beer, 100)




#-------------[4] TS Anatomy Plot--------------
# Seasonal Decomposition of Time Series by Loess
quartz()
plot(stl(log(beer),s.window="periodic"))
forecast(beer)
summary(forecast(beer))
