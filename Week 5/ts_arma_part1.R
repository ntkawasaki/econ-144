#***********************************************
# Randall R. Rojas
# Email: rrojas@econ.ucla.edu
# Date: 04/26/2015
# Comment(s): R fiting ARMA(p,q) models to data
# Data File(s): caemp.txt
#***********************************************
# Variable Definitions
# Canadian Employment Index (quarterly, seasonally adjusted starting from 1962)
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
library("TTR")
library(tis)
require("datasets")
require(graphics)
library("forecast")
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

# Read in the data into a ts data file:
caemp=read.table("caemp.txt")
caemp_ts<-ts(caemp,start=1962.1,freq=4)
quartz()
plot(caemp_ts,xlab='Year', ylab="Canadian Employment", lwd=2)
grid()

# Look at the correlogram
quartz()
par(mfrow=c(3,1))
acf(caemp_ts, type = "covariance", main="Autocovariance",lag.max=50, ylab="COV")
acf(caemp_ts, type = "correlation", main="Autocorrelation",lag.max=50,ylab="ACF")
acf(caemp_ts, type = "partial",main="Partial Autocorrelation",lag.max=50, ylab="PACF")
# The plots show evidence of serial corrleation --> cycles
# ACF plot: High during business cylce booms, and low during recessions.
# For further discussion read pages 130-132 of the textbook.

# Test the white noise Ho:
acf_val=acf(caemp_ts)$acf
Box.test(acf_val, type = "Ljung-Box")
Box.test(acf_val, type = "Box-Pierce")

# MA(q) Model Fitting (see page 155)
ma1=arma(caemp_ts,order=c(0,1)) #Same as MA(1) = AR(0) + MA(1)
summary(ma1)
ma2=arma(caemp_ts,order=c(0,2)) #Same as MA(2) = AR(0) + MA(2)
summary(ma2)
ma3=arma(caemp_ts,order=c(0,10)) #Same as MA(10) = AR(0) + MA(10)
summary(ma3)
#plot(ma3)

# Look at all the MA(q) fits
quartz()
plot(caemp_ts,xlab='Year', ylab="Canadian Employment", lwd=2)
grid()
lines(ma1$fitted.values,col="blue",lwd=2)
lines(ma2$fitted.values,col="seagreen2",lwd=2)
lines(ma3$fitted.values,col="red",lwd=2)
legend("topright",legend=c("Data","MA(10)","MA(2)","MA(1)"),text.col=1:4,bty="n")

# Examine the best fit MA(q) model
quartz()
par(mfrow=c(2,2))
plot(caemp_ts,xlab='Year', ylab="Canadian Employment", lwd=2)
lines(ma3$fitted.values,col="red",lwd=1,lty=1)
plot(ma3$residuals,ylab="Residuals")
acf(ma3$residuals[12:136], type = "correlation", main="Autocorrelation",lag.max=13,ylab="ACF")
acf(ma3$residuals[12:136], type = "partial",main="Partial Autocorrelation",lag.max=13, ylab="PACF")
# Overall, even with the high q-order, the MA(q) model is not that good!

# AR(p) Model Fitting (see page 157)
ar1=ar(caemp_ts,FALSE,1) #Same as AR(1), FALSE is needed to allow for different values of p 
ar1
ar2=ar(caemp_ts,FALSE,2) #Same as AR(2), Note: if no set FALSE, it will figure the best order (p) 
ar2
ar3=ar(caemp_ts,FALSE,3) #Same as AR(3) 
ar3

# You can also use ARMA(p,q) but setting q=0 this time.
ar1=arma(caemp_ts, order=c(1,0)) #Same as AR(1) = AR(1) + MA(0)
summary(ar1)
ar2=arma(caemp_ts, order=c(2,0)) #Same as AR(2) = AR(2) + MA(0)
summary(ar2)
ar3=arma(caemp_ts, order=c(3,0)) #Same as AR(3) = AR(3) + MA(0)
summary(ar3)
#plot(ar2)

# Look at all the AR(p) fits
quartz()
plot(caemp_ts,xlab='Year', ylab="Canadian Employment", lwd=2)
grid()
lines(ar1$fitted.values,col="blue",lwd=2,lty=2)
lines(ar2$fitted.values,col="seagreen2",lwd=2,lty=2)
lines(ar3$fitted.values,col="red",lwd=2,lty=2)
legend("topright",legend=c("Data","AR(3)","AR(2)","AR(1)"),text.col=1:4,bty="n")

# Examine the best fit AR(p) model
quartz()
par(mfrow=c(2,2))
plot(caemp_ts,xlab='Year', ylab="Canadian Employment", lwd=2)
lines(ar2$fitted.values,col="red",lwd=1,lty=1)
plot(ar2$residuals,ylab="Residuals")
acf(ar2$residuals[3:136], type = "correlation", main="Autocorrelation",lag.max=13,ylab="ACF")
acf(ar2$residuals[3:136], type = "partial",main="Partial Autocorrelation",lag.max=13, ylab="PACF")
# We can see a significant improvement e.g., from looking at the ACF and PACF plots, the 
# residuals now look consistent with noise, suggesting we accounted for most (or all) of
# the dynamics left after detrending and seasonally adjusting the data.
# NOTE: With AR we only need p=2 unlike MA which required a much higher order polynomial.

# ARMA(p,q) Model Fitting (see page 157)
arma1=arma(caemp_ts,order=c(1,1)) # AR(1) + MA(1)
summary(arma1)
arma2=arma(caemp_ts,order=c(1,2)) # AR(1) + MA(2)
summary(arma2)
arma3=arma(caemp_ts,order=c(2,1)) # AR(2) + MA(1)
summary(arma3)
arma4=arma(caemp_ts,order=c(2,2)) # AR(2) + MA(2)
summary(arma4)
#plot(ar2)

# Look at all the ARMA(p,q) fits
quartz()
plot(caemp_ts,xlab='Year', ylab="Canadian Employment", lwd=2)
grid()
lines(arma1$fitted.values,col="blue",lwd=2,lty=2)
lines(arma2$fitted.values,col="seagreen2",lwd=2,lty=2)
lines(arma3$fitted.values,col="red",lwd=2,lty=2)
legend("topright",legend=c("Data","ARMA(2,1)","ARMA(1,2)","ARMA(1,1)"),text.col=1:4,bty="n")

# Examine the best fit ARMA(p,q) model
quartz()
par(mfrow=c(2,2))
plot(caemp_ts,xlab='Year', ylab="Canadian Employment", lwd=2)
lines(arma3$fitted.values,col="red",lwd=1,lty=1)
plot(arma3$residuals,ylab="Residuals")
acf(arma3$residuals[3:136], type = "correlation", main="Autocorrelation",lag.max=13,ylab="ACF")
acf(arma3$residuals[3:136], type = "partial",main="Partial Autocorrelation",lag.max=13, ylab="PACF")

# Simulate AR(p), MA(q), and ARMA(p,q) process and test the theory :)

# 1. Simulate an MA(2) process. Accroding to theory, the ACF cuts-off at lag=2
ma.sim<-arima.sim(model=list(ma=c(-.7,.1)),n=1000) #Note: 'ma' has 2 coefficients, hence MA(2)
quartz()
par(mfrow=(c(3,1)))
plot(ma.sim)
acf(ma.sim)
pacf(ma.sim)

# 2. Simulate an AR(4) process. Accroding to theory, the PACF cuts-off at lag=4
ar.sim<-arima.sim(model=list(ar=c(.9,-.2,-.8,0.5)),n=10000) #Note: 'ar' has 2 coefficients, hence AR(4)
quartz()
par(mfrow=c(3,1))
plot(ar.sim)
acf(ar.sim)
pacf(ar.sim)

# 2. Simulate an ARMA(2,2) process. According to theory,...?? 
arma.sim<-arima.sim(model=list(ar=c(.9,-.2),ma=c(-.7,.1)),n=10000) 
quartz()
par(mfrow=c(3,1))
plot(arma.sim)
acf(arma.sim)
pacf(arma.sim)






