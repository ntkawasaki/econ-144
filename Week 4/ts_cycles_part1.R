#***********************************************
# Randall R. Rojas
# Email: rrojas@econ.ucla.edu
# Date: 04/20/2015
# Comment(s): R code example for characterizing cycles in ts data.
# Data File(s): returns.txt
#***********************************************
# Variable Definitions (returns.txt)
# date   = Monthly from January 1926 - December 2008
# ibmrtn = Monthly simple IBM Stock Returns
# vwrtn  = Monthly simple returns for the CRSP equal-weighted index
# ewrtn  = Monthly simple returns of the CRSP equal-weighted index
# sprtn  = Monthly simple returns for the S&P 500
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
#install.packages("astsa")
#require(astsa)
library(nlstools)

# Sample Time-Series Data. 
y1 = ts(rnorm(360,0,1), frequency=12)
y2 = ts(rnorm(360,0,1)+ 20*sin(2*pi*(1:360)/12), frequency=12)
y3 = ts(rnorm(360,0,1)+ 2*(1:360)/12, frequency=12)
y4 = ts(runif(360,0,1), frequency=12)

# Autocovariance, ACF, and PACF Functions 
quartz()
plot(stl(y2,s.window="periodic"))
?stl
quartz()
par(mfrow=c(3,1))
acf(y2, type = "covariance")
acf(y2, type = "correlation", lag.max=12)
acf(y2, type = "partial")

# Test for White noise using Ljung-Box and Box-Pierce
# Small statistics value --> Reject Ho: yt ~ white noise--> cyles are present
# Ljung-Box Q-Statistic
Box.test(y2, type = "Ljung-Box")
# Box-Pierce Q-Statistic
Box.test(y2, type = "Box-Pierce")

#Monthly Simple IBM Returns from 1926-2008
z=read.table("returns.txt",header=T)
z[1,]
sibm=z[,2]
sibm_ts=ts(z[,2],frequency=12,start=c(1926,1))
libm_ts=log(sibm_ts+1) 

quartz()
plot(stl(sibm_ts,s.window="periodic"))

quartz()
par(mfrow=c(3,1))
acf(sibm_ts, type = "covariance",lag.max=100,ylim=c(-0.0015,0.0015))
acf(sibm_ts, type = "correlation", lag.max=100,ylim=c(-0.15,0.15))
acf(sibm_ts, type = "partial",lag.max=100,ylim=c(-0.15,0.15))

Box.test(sibm_ts, lag=5, type = "Ljung-Box") 
# Notice the large Q-stat p-value = 0.64! --> Fail to reject Ho 

Box.test(sibm_ts, lag=10, type = "Ljung-Box")
# Notice the large Q-stat p-value = 0.17! --> Fail to reject Ho 

# Conclusion: From the ACF we can see very small(~0) serial corrleations of monthly
# IBM stock returns from Jan 1926 -Dec 2008. The ACFs are within the 
# 2 Std. Error limits --> Not statisticaly different from 0 at the 5% level. 


# Monthly Value-Weighted Index (S&P 500) Returns from 1926-2008
vwrtn_ts=ts(z[,3],frequency=12,start=c(1926,1))
quartz()
plot(stl(sibm_ts,s.window="periodic"))

quartz()
par(mfrow=c(3,1))
acf(vwrtn_ts, type = "covariance",lag.max=100,ylim=c(-0.0015,0.0015))
acf(vwrtn_ts, type = "correlation", lag.max=100,ylim=c(-0.15,0.15))
acf(vwrtn_ts, type = "partial",lag.max=100,ylim=c(-0.15,0.15))

Box.test(vwrtn_ts, lag=5, type="Ljung-Box") 
# Notice the low Q-stat p-value ~0 --> Reject Ho 
Box.test(vwrtn_ts, lag=10, type="Ljung-Box")
# Notice the low Q-stat p-value ~0 --> Reject Ho

# Conclusion: From the ACF we can see that monthly market index returns
# are serially corrleated. Therefore, market indices have stronger serial dependence
# than individual stock returns. 



