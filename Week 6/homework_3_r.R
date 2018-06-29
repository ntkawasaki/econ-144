## Homework 3 R script
## 6.4, 6.10, 7.2, 7.6, 7.7

library(tidyverse)
library(quantmod)
library(tseries)
library(forecast)
library(data.table)
library(readxl)

setwd('/Users/noahkawasaki/Desktop/ECON 144/Textbook_data')

## 6.4
# MA(2): yt = 0.7 - 2e_1 + 1.35e_2 + et

# a) Obtain the theoretical autocorrelation up to lag 10
# ρ1 =(θ1θ2 +θ1)/(1+θ12 +θ2)=((−2)×1.35−2)/(1+(−2)2 +1.352)= −0.688 
# ρ2 = θ2/(1 + θ12 + θ2) = 1.35/(1 + (−2)2 + 1.352) = 0.197
# ρ3 =ρ4 =...=ρ10 =0.

# b) 
y = e = rnorm(100)
for (t in 3:100) y[t] = 0.7 - 2*e[t-1] + 1.35*e[t-2] + e[t]

acf <- acf(y, lag.max=10)
acf$acf

pacf <- pacf(y, lag.max=10)

# We can see the sample ρ1 = -0.714 which is close to -0.688, and ρ2 = 2.76 which is close to 0.197. Both values 
# have the same sign. The difference between our sample values and the theoretical values is due to a small 
# sample size. In terms of the visuals, we observe two significant spikes in the ACF and The PACF visual and a 
# decay to zero in the PACF, which is characteristic of an MA(2) process.


# 6.10
getSymbols("AAPL")
head(AAPL)

ggplot(AAPL, aes(x=Index)) +
  geom_line(aes(y=AAPL.Open, color='Observed Values'), color='green') +
  ggtitle('AAPL Opening Stock Prices', 'AR(2)') +
  ylab('Price') +
  xlab('Date') +
  theme_dark()

# Returns
returns <- log(AAPL$AAPL.Open) - log(lag(AAPL$AAPL.Open))[2:2856]

ggplot(returns, aes(x=Index)) +
  geom_line(aes(y=AAPL.Open), color='green') +
  geom_hline(yintercept=0, color='black', linetype='dashed') +
  ggtitle('AAPL Daily Returns') +
  ylab('Return') +
  xlab('Date')

acf(returns)
pacf(returns)

# From the returns plot we can see that the returns process appears have significant spikes at a lag of 1
# for both the ACF and PACF.
ar1 <- arma(returns, c(1, 0))
summary(ar1)
tail(returns)

cols <- c("Observed Values"="green", "Fitted Values"="black")
ggplot(returns, aes(x=Index)) +
  geom_line(aes(y=AAPL.Open, color='Observed Values')) +
  geom_line(aes(y=ar1$fitted.values, color='Fitted Values'), na.rm=TRUE) +
  ggtitle('Daily Returns', 'AAPL') +
  ylab('Return') +
  xlab('Date') +
  scale_color_manual("Legend", values=cols)

# From this plot can see that the fitted values all fluctuate around zero as a stationary process. Therefore
# our forecasted returns will basically be 0 as the unconditional mean. So the price forecasts will be the same 
# price as the day before, and follow a linear trend at 0.


## 7.2
df <- read_excel("Chapter7_exercises_data.xls", sheet=1)
setnames(df, c('date', 'unemployed'))

x <- acf(df$unemployed)
y <- pacf(df$unemployed)         

x$acf
y$acf
# The ACF plot shows high persistence, with the persistence parameter at 0.9906. Since it decays downward this suggests
# an autoregressive process. The PACF shows a significant spike at lag 1 with the same value at 0.9906 and then
# approximately zero afterwards. The PACF and ACF plots suggest an AR(1) process, but since the persistence parameter
# is so close to 1, we should also consider whether the process is covariance stationary in the first place. 


## 7.6
df76 <- read_excel("Chapter7_exercises_data.xls", sheet=3)[-1,]
setnames(df76, c("date", "housing_cpi", "housing_inf", "transp_cpi", "transp_inf"))
head(df76)

map <- c("Housing"="red", "Transportation"="blue")
ggplot(df76, aes(x=date)) +
  geom_line(aes(y=housing_inf, color='Housing'), na.rm=TRUE) +
  geom_line(aes(y=transp_inf, color='Transportation'), na.rm=TRUE) +
  ggtitle("Housing and Transportation Inflation", "1967-2011") +
  ylab("Rate") +
  xlab("Date") +
  scale_color_manual("Legend", values=map)

# Housing Inflation
acf(df76$housing_inf)         
pacf(df76$housing_inf)  
# The housing inflation rates suggest the potential of an AR(2) process. There is a decay to zero at 6 lags in the 
# ACF and two significant spikes at lags of 1 and 2 in the PACF. There is also a questionable spike at lag 3 which could
# be tested, and bring an AR(3) into consideration.

# Transportation Inflation
acf(df76$transp_inf)         
pacf(df76$transp_inf) 
# The transportation inflation shows weaker time dependence than the housing inflation series. The ACF decays to zero
# after just two lags. The PACF also only shows one significant spike. This could suggest an AR(1) model with
# a lower persistence parameter around 0.4.
         
         
## 7.7
# From question 6 we already computed the ACF and PACF functions of the housing and transportation inflation rates.
# We will continue by testing an AR(3) on housing inflation, and an AR(1) on the transportation inflation. 

# Housing 
housing <- arima(df76$housing_inf, c(3,0, 0))
plot(forecast(housing, h=3))


# Transportation
transp <- arima(df76$transp_inf, c(1, 0, 0))
plot(forecast(transp, h=3))




















     
