#### KALMAN FILTER

t=1:100

x1 = 4*cos(2*pi*t*20/100) + 4*sin(2*pi*t*20/100)
x2 = 3*cos(2*pi*t*40/100) + 3*sin(2*pi*t*40/100)

x1 = 2*cos(2*pi*t*6/100) + 3*sin(2*pi*t*6/100)
x2 = 4*cos(2*pi*t*10/100) + 5*sin(2*pi*t*10/100)
x3 = 6*cos(2*pi*t*40/100) + 7*sin(2*pi*t*40/100)
x = x1+x2+x3+50*rnorm(100)
par(mfrow=c(2,2))
plot.ts(x1, ylim=c(-16,16), main ="freq = 0.06")
plot.ts(x2, ylim=c(-16,16), main ="freq = 0.10")
plot.ts(x3, ylim=c(-16,16), main ="freq = 0.40")
plot.ts(x, ylim=c(-16,16), main ="Sum")

dev.print(device=postscript,"arma21.eps",width=7,height=7, horizontal=FALSE)
dev.off()
#http://www.ucl.ac.uk/jdi/events/int-CIA-conf/ICIAC11_Slides/ICIAC11_1E_LTompson

P =abs(2*fft(x)/100)^2
f=0:50/100
plot(f,P[1:51], type ="o", xlab="Frequency", ylab ="Periodogram")
dev.print(device=postscript,"periodogram.eps",width=7,height=7, horizontal=FALSE)
dev.off()
library("FKF")

## <--------------------------------------------------------------------------->
## Example 1: ARMA(2, 1) model estimation.
## <--------------------------------------------------------------------------->
## This example shows how to fit an ARMA(2, 1) model using this Kalman
## filter implementation (see also stats  makeARIMA and KalmanRun).
n <- 1000
## Set the AR parameters
ar1 <- 0.6
ar2 <- 0.2
ma1 <- -0.2
sigma <- sqrt(0.2)
## Sample from an ARMA(2, 1) process
a <- arima.sim(model = list(ar = c(ar1, ar2), ma = ma1), n = n,
               innov = rnorm(n) * sigma)
## Create a state space representation out of the four ARMA parameters
arma21ss <- function(ar1, ar2, ma1, sigma) {
  Tt <- matrix(c(ar1, ar2, 1, 0), ncol = 2)
  Zt <- matrix(c(1, 0), ncol = 2)
  ct <- matrix(0)
  dt <- matrix(0, nrow = 2)
  GGt <- matrix(0)
  H <- matrix(c(1, ma1), nrow = 2) * sigma
  HHt <- H %*% t(H)
  a0 <- c(0, 0)
  P0 <- matrix(1e6, nrow = 2, ncol = 2)
  return(list(a0 = a0, P0 = P0, ct = ct, dt = dt, Zt = Zt, Tt = Tt, GGt = GGt,HHt = HHt)) 
}
## The objective function passed to  optim 
objective <- function(theta, yt) {
  sp <- arma21ss(theta["ar1"], theta["ar2"], theta["ma1"], theta["sigma"])
  ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
             Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = yt)
  return(-ans$logLik)
}
theta <- c(ar = c(0, 0), ma1 = 0, sigma = 1)
fit <- optim(theta, objective, yt = rbind(a), hessian = TRUE)
fit
## Confidence intervals
rbind(fit$par - qnorm(0.975) * sqrt(diag(solve(fit$hessian))),
      fit$par + qnorm(0.975) * sqrt(diag(solve(fit$hessian))))

## Filter the series with estimated parameter values
sp <- arma21ss(fit$par["ar1"], fit$par["ar2"], fit$par["ma1"], fit$par["sigma"])
ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
           Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = rbind(a))

## Compare the prediction with the realization
plot(ans, at.idx = 1, att.idx = NA, CI = NA)
lines(a, lty = "dotted")

## Compare the filtered series with the realization
plot(ans, at.idx = NA, att.idx = 1, CI = NA)
lines(a, lty = "dotted")

## Check whether the residuals are Gaussian
plot(ans, type = "resid.qq")

## Check for linear serial dependence through  acf 
plot(ans, type = "acf")

#Plot residuals
tsdisplay(ans$vt[1,],main='Kalma-Filter Residuals')
#Concistsen with white noise

afit=auto.arima(a)

tsdisplay(afit$residuals,main="ARMA(2,1) Residuals")

dev.print(device=postscript,"arma_Rres.eps",width=7,height=7, horizontal=FALSE)
dev.off()

file.exists("~/.ssh/id_rsa.pub")
