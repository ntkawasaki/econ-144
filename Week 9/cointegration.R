#### Co-integration and pairs trading
setwd('/Users/noahkawasaki/Desktop/ECON 144/Week 9')

# Source: http://faculty.chicagobooth.edu/ruey.tsay/teaching/bs41202/sp2017/lec10-17.pdf

# Another good example of pairs trading can be found here: 
# https://www.quantstart.com/articles/Cointegrated-Augmented-Dickey-Fuller-Test-for-Pairs-Trading-Evaluation-in-R

require(MTS)
library(urca)
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

da=read.table("d-bhp0206.txt",header=T)
da1=read.table("d-vale0206.txt",header=T)
bhp=log(da$adjclose)
vale=log(da1$adjclose)
bhp1=ts(bhp,frequency=252,start=c(2002,127))
vale1=ts(vale,frequency=252,start=c(2002,127))

# Plot the two series
quartz()
plot(bhp1, type ='l', col = "blue",ylim=c(0.45,3.7),ylab="Stock Price ($)", main= "Pairs Trading Example")
lines(vale1, type ='l', col = "red")
legend(2003, 3.5, c("BHP","Vale"), fill=c("blue","red"),cex =1.2,bty='n')

# Try a simple regression of S1 on S2
mreg=lm(bhp~vale)
summary(mreg)

# Perform the Johansen Test
x=cbind(bhp,vale)
m1=ar(x)
m1$order
m2=ca.jo(x,K=2)
summary(m2)
m3=ca.jo(x,K=2,type=c("trace"))
summary(m3)


# Compute the co-intergration expression
wt=bhp1-0.718*vale1
quartz()
plot(wt,type='l',ylab="wt = BHP - 0.72Vale",main = "Co-integration Relation")
abline(h=c(1.82),col="red")
legend(2003, 1.95,c("wt","E[wt] = 1.82"),fill=c("black","red"))

quartz()
tsdisplay(wt)

# Now that we have μ = 1.82 and 𝝲 = 0.72 -->Trade

# (1) Buy Stock 1 and short 𝝲 shares of Stock 2 when the wt = μ - 𝝳. 
# (2) Sell Stock 1 and buy 𝝲 shares of Stock 2,when wt+h = μ + 𝝳
# (3) Profit: r(h) = wt+h  - wt = μ + 𝝳
#Some practical considerations: 
#The threshold is chosen so that the profit out-weights the costs of two trading. In high frequency, must be greater than trading slippage, which is the same linear combination of bid-ask spreads of the two stock, i.e. bid-ask spread of Stock 1 + ⇥(bid-ask spread) of Stock 2.  
#Speed of mean-reverting of wt plays an important role as h is directly related to the speed of mean-reverting. 
#There are many ways available to search for co-integrating pairs of stocks. For example, via fundamentals, risk factors, etc. 
#For unit-root and co-integration tests, see the textbook and ref- erences therein. 

# We can now fit a model to wt and forecast or simply forcst using ETS

# Using ETS
quartz()
plot(forecast(wt))

quartz()
tsdiag(ets(wt))



