#***********************************************
# Randall R. Rojas
# Email: rrojas@econ.ucla.edu
# Date: 05/10/2015
# Comment(s): R code for forecasting Trend+Seasonal+Cycle models.
# Data File(s): liquor.dat
#***********************************************
# Variable Definitions
# Monthly U.S.liquor sales from 1968.01 - 1996.01
#************************************************

# Set your 'working directory' to the folder where all the data and respective codes are located.
setwd("/Users/noahkawasaki/Desktop/ECON 144/Week 5")

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
library(xtable)
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

# Look at the data and log(data)
data=read.table("liquor.dat")
data_ts<-ts(data[,1],start=1968.1,freq=12)
t<-seq(1968, 1996.1,length=length(data_ts))
lgdata=log(data_ts)
quartz()
par(mfrow=c(2,1))
plot(data_ts,xlab='Time', ylab="Sales", lwd=2)
plot(lgdata,xlab='Time', ylab="Log (Sales)", lwd=2)
#grid()

# Fit a quadrtaic trend model
t2<-t^2
m1=lm(lgdata~t+t2)
quartz()
par(mfrow=c(2,1))
plot(lgdata,ylab="Log (Sales)", xlab="Time", lwd=2, col='skyblue3', xlim=c(1968,1995))
lines(t,m1$fit,col="red3",lwd=2)
plot(t,m1$res, ylab="Residuals",type='l',xlab="Time")

# Look at the ACF and PACF
quartz()
par(mfrow=c(2,1))
acf(m1$res,lag=36,main="Residual Sample Autocorrelations",xlab="Displacement")
pacf(m1$res,lag=36,main="Residual Sample Partial Autocorrelations", xlab="Displacement")

# Fit a quadrtaic trend + seasonality model (no y-intercept)
m2=tslm(lgdata~0+t+t2+season)
quartz()
par(mfrow=c(2,1))
plot(lgdata,ylab="Log (Sales)", xlab="Time", lwd=2, col='skyblue3')
lines(t,m2$fit,col="red3",lwd=2,lty=2)
plot(t,m2$res, ylab="Residuals",type='l',xlab="Time",lwd=2)

# Look at the ACF and PACF
quartz()
par(mfrow=c(2,1))
acf(m2$res,lag=36,main="Residual Sample Autocorrelations",xlab="Displacement")
pacf(m2$res,lag=36,main="Residual Sample Partial Autocorrelations", xlab="Displacement")

# Summary of all four plots
quartz()
par(mfrow=c(2,2))
plot(lgdata,ylab="Log (Sales)", xlab="Time", lwd=2, col='skyblue3',main="log(data) + fit (trend +seasonality)")
lines(t,m2$fit,col="red3",lwd=1,lty=2,main="Residuals")
plot(t,m2$res, ylab="Residuals",type='l',xlab="Time",lwd=2)
acf(m2$res,lag=36, main="Residual Sample ACF",xlab="Displacement")
pacf(m2$res,lag=36, main="Residual Sample PACF", xlab="Displacement")
# The residuals + ACF and PACF suggest cycles!


### Important: To check for Seasonality, we need to look at 
### the ACF and PACF but at higher lags (multiples of 12 for monthly obs, mult of 4 for quarterly, etc).
quartz()
par(mfrow=c(2,1))
acf(diff(lgdata,12),lag=360,main="Residual Sample Autocorrelations",xlab="Displacement")
pacf(diff(lgdata,12),lag=360,main="Residual Sample Partial Autocorrelations", xlab="Displacement")

quartz()
plot(lgdata,ylab="Log (Sales)", xlab="Time", lwd=2, col='gray')
# Model: Quadratic Trend + Cycles
m4=arima(lgdata,order=c(3,0,0),xreg = cbind(t, t2))
lines(fitted(m4),col="red")
# Look at the ACF and PACF

# Model: Seasonality + Cycles
quartz()
plot(lgdata,ylab="Log (Sales)", xlab="Time", lwd=2, col='gray')
m4=arima(lgdata,order=c(3,0,0),seasonal=list(order=c(1,0,1)))
lines(fitted(m4),col="green")

# Model: Quadratic Trend + Seasonality + Cycles
quartz()
plot(lgdata,ylab="Log (Sales)", xlab="Time", lwd=2, col='gray')
m5=arima(lgdata,order=c(3,0,0), xreg=cbind(t, t2), seasonal=list(order=c(1,0,1)))
summary(m5)
lines(fitted(m4),col="black")

quartz()
par(mfrow=c(2,1))
acf(m5$res,lag=360,main="Residual Sample Autocorrelations",xlab="Displacement")
pacf(m5$res,lag=360,main="Residual Sample Partial Autocorrelations", xlab="Displacement")
# The ACF and PACF look better, but there's still room for improvement. Why??


# Even better if we include 'I' in ARMA
m4=Arima(lgdata,order=c(3,1,0),xreg = cbind(t, t2),seasonal=list(order=c(1,0,1)))
quartz()
par(mfrow=c(2,1))
acf(m4$res,lag=360,main="Residual Sample Autocorrelations",xlab="Displacement")
pacf(m4$res,lag=360,main="Residual Sample Partial Autocorrelations", xlab="Displacement")
summary(m4)

# Now we can forecast safely

# 1. Forecast using your model:
m4=Arima(lgdata,order=c(3,1,0),include.drift=TRUE,seasonal=list(order=c(1,0,1)))
quartz()
plot(forecast(m4,h=36),shadecols="oldstyle")

# 2. Let R figure it all out for you -lazy approach but very effective ;-) 
fit=auto.arima(lgdata)
fit
quartz()
plot(forecast(fit,h=36),shadecols="oldstyle")

# Recursive Residuals:
#http://www.oga-lab.net/RGM2/func.php?rd_id=strucchange:recresid
#http://svitsrv25.epfl.ch/R-doc/library/strucchange/html/recresid.html
y=recresid(m4$res~1)
plot(y, pch=16,ylab="Recursive Residuals")
plot(efp(m4$res~1, type = "Rec-CUSUM"))
?efp

#Cross-Validation
library(cvTools)
library(boot)
library(rpart)
#Please example from: https://stat.ethz.ch/R-manual/R-devel/library/boot/html/cv.glm.html

