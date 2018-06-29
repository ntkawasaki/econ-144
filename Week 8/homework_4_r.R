## Homework 4 R Script
library(tidyverse)
library(lubridate)
library(ggplot2); theme_set(theme_grey())
library(tseries)
library(forecast)
library(strucchange)
library(openxlsx)
library(readxl)
library(zoo)


setwd("/Users/noahkawasaki/Desktop/ECON 144/Week 8")


## PROBLEM 1
# The file w-gs1yr.txt contains the U.S. weekly interest rates (in percentages) from January 5, 1962, 
# to April 10, 2009. For this assignment you will fit an appropriate ARMA(p,q) model and make a 24-steps-ahead 
# forecast.

# Weekly interest rates
int_rates <- read.table(file='/Users/noahkawasaki/Desktop/ECON 144/Week 8/w-gs1yr.txt', header=TRUE) %>%
  mutate(
    date = lubridate::ymd(paste(year, mon, day, sep="-")),
    growth = log(rate) - log(lag(rate))
  )  %>%
  dplyr::select(date, rate, growth)

ts <- ts(int_rates$rate)

# (a) Show a plot of the data, along with the respective ACF and PACF functions. Discuss the plots.
ggplot(int_rates, aes(date, rate)) +
  geom_line(color='green') +
  ggtitle("US Weekly Interest Rates", "1962-2009") +
  xlab("Date") +
  ylab("Rate")

acf <- acf(int_rates$rate, lag.max=20, plot=FALSE)
acf_df <- as.data.frame(acf$acf)
ci <- qnorm((1 + 0.95)/2)/sqrt(acf$n.used)

ggplot(acf_df, aes(x=seq(nrow(acf_df)-1, from=0), y=V1)) +
  geom_col(fill='green') +
  geom_hline(yintercept=ci, linetype='dashed') +
  geom_hline(yintercept=-ci, linetype='dashed') +
  geom_hline(yintercept=0) +
  ylim(-0.25, 1) +
  ggtitle('ACF', "US Weekly Interest Rates") +
  xlab('Lag') +
  ylab('ACF')

pacf <- pacf(int_rates$rate, lag.max=20, plot=FALSE)
pacf_df <- as.data.frame(pacf$acf)
ci_pacf <- qnorm((1 + 0.95)/2)/sqrt(pacf$n.used)

ggplot(pacf_df, aes(x=seq(nrow(pacf_df), from=1), y=V1)) +
  geom_col(fill='green') +
  geom_hline(yintercept=ci_pacf, linetype='dashed') +
  geom_hline(yintercept=-ci_pacf, linetype='dashed') +
  geom_hline(yintercept=0) +
  ylim(-0.5, 1) +
  ggtitle('PACF', "US Weekly Interest Rates") +
  xlab('Lag') +
  ylab('PACF')

# From the time series plot we can see some persistent behavior. There are peaks and troughs that persist over longer time 
# periods as if values are dependent on the past. The ACF plot shows strong persistence with its very slow decay past 
# 35 lags. The PACF plot has siginificant barlett bands at lags of 1, 2, and 3, althought the third band is weaker. From  
# this I would suggest an AR(2) or AR(3) model.


# (b) Based on your discussion in (a), fit 3 different ARMA(p,q) models, and show the fits over the original data. 
#     Discuss your results, and select one model as your preferred choice.
cols1 <- c("Observed Values"="green", "Fitted Values"="black")

arma1 <- arma(ts, order=c(3,0))
ggplot(int_rates, aes(date, rate)) +
  geom_line(aes(color='Observed Values')) +
  geom_line(aes(y=arma1$fitted.values, color='Fitted Values'), linetype='dashed') +
  ggtitle("Observed Values vs Fitted Values", "ARMA(3, 0)") +
  xlab("Date") +
  ylab("Rate") +
  scale_color_manual("Legend", values=cols1)

arma2 <- arma(ts, order=c(1,1))
ggplot(int_rates, aes(date, rate)) +
  geom_line(aes(color='Observed Values')) +
  geom_line(aes(y=arma2$fitted.values, color='Fitted Values'), linetype='dashed') +
  ggtitle("Observed Values vs Fitted Values", "ARMA(1, 1)") +
  xlab("Date") +
  ylab("Rate") +
  scale_color_manual("Legend", values=cols1)

arma3 <- arma(ts, order=c(1,2))
ggplot(int_rates, aes(date, rate)) +
  geom_line(aes(color='Observed Values')) +
  geom_line(aes(y=arma3$fitted.values, color='Fitted Values'), linetype='dashed') +
  ggtitle("Observed Values vs Fitted Values", "ARMA(1, 2)") +
  xlab("Date") +
  ylab("Rate") +
  scale_color_manual("Legend", values=cols1)

# I fit ARMRA(3,0), ARMA(1,1), and ARMA(1,2) models to the data. From the plots alone it is hard to tell which
# model has the best fit visually. From the summary statistics of each model however, I will continue the assignment
# with the arma1 or ARMA(3,0) model. It has the lowest AIC score and statistical significance on the AR components.


# (c) Now that you fit an ARMA(p,q) model to the data, plot the ACF and PACF of the residuals from your preferred 
#     fit model, and discuss your results.
acf_resid <- acf(arma1$residuals[4:length(arma1$residuals)], lag.max=20, plot=FALSE)
acf_resid_df <- as.data.frame(acf_resid$acf)
ci_acf_resid <- qnorm((1 + 0.95)/2)/sqrt(acf_resid$n.used)

ggplot(acf_resid_df, aes(x=seq(nrow(acf_resid_df)-1, from=0), y=V1)) +
  geom_col(fill='green') +
  geom_hline(yintercept=ci_acf_resid, linetype='dashed') +
  geom_hline(yintercept=-ci_acf_resid, linetype='dashed') +
  geom_hline(yintercept=0) +
  ylim(-0.25, 1) +
  ggtitle('ACF', "ARMA(3,0) Residuals") +
  xlab('Lag') +
  ylab('ACF')

pacf_resid <- pacf(arma1$residuals[4:length(arma1$residuals)], lag.max=20, plot=FALSE)
pacf_resid_df <- as.data.frame(pacf_resid$acf)
ci_pacf_resid <- qnorm((1 + 0.95)/2)/sqrt(pacf_resid$n.used)

ggplot(pacf_resid_df, aes(x=seq(nrow(pacf_resid_df), from=1), y=V1)) +
  geom_col(fill='green') +
  geom_hline(yintercept=ci_pacf_resid, linetype='dashed') +
  geom_hline(yintercept=-ci_pacf_resid, linetype='dashed') +
  geom_hline(yintercept=0) +
  ylim(-0.08, 0.08) +
  ggtitle('PACF', "ARMA(3,0) Residuals") +
  xlab('Lag') +
  ylab('PACF')

# The ACF of our ARMA(3,0) model shows some significant spikes at lags 4, 7, 14, and 18 in this plot. From this
# plot alone its not definitive that the residuals resemble white noise. The PACF plot shows significant spikes
# at lags of 4, 7, 9, 14, and 17 in this x range. Note the scale of the y-axis, so these spikes do not indicate
# strong correlations. From this we can assume that the ARMA(3,0) model did a good job of capturing most of the
# patterns in the time series, but can be improved upon.


# (d) Compute and plot the recursive residuals from your best fit model. Interpret the plot.
recursives <- as.data.frame(recresid(arma1$res~1)) %>% 
  set_names("residuals")

ggplot(recursives, aes(x=seq(nrow(recursives)), y=residuals)) +
  geom_point(color='orange', alpha=0.7) +
  ggtitle('Recursive Residuals') +
  xlab('Index') +
  ylab('Value')

# For the most part, the recursive residuals are densely clustered around zero, however there is a much wider
# region of deviations around index of 1000, which would correspond to the time of 1980-1985 in the original
# time series. This was a period of high volatility, so it is not surprising that the residuals have higher
# variance.  This tells us that the ARMA(3,0) model did not sufficiently capture this cluster of data well.


# (e) Compute and plot the CUSUM from your best fit model. Interpret the plot.
plot(efp(arma1$res~1, type="Rec-CUSUM"))

# The CUSUM of the residuals plot shows the trajectory of the residuals is well within the red bands, so there
# are no significant breaks in the model. We do still observe some persistence behavior in the CUSUM series
# that could be better captured.


# (f) Compute the best fit model according to ‘R’ and compare it against your model. Discuss these results.
best <- auto.arima(ts)
summary(best)

# R's best model from auto.arima is an ARIMA(1,1,2). So R included as AR component to one lag, and two MA components
# to two lags. Also note that R output the first difference, so this model is not reading the exact same time series
# as the model I chose earlier. Comparing the AIC's, R's model was better. the ARMA(3,0) had an AIC of -1525 while
# the ARIMA(1,1,2) had a slightly better score ar -1528

 
# (g) Using your best fit model as well as ‘R’s’ best fit model, compute the respective 24-steps-ahead forecast
#     and compare your results.

# Remake ARMA(3,0) with Arima so I can use forecast
arima1 <- Arima(ts, order=c(3,0,0))
f1 <- forecast(arima1, h=24)
plot(f1, xlim=c(2000, 2490), ylim=c(-2,10))

f2 <- forecast(best, h=24)
plot(f2, xlim=c(2000, 2490), ylim=c(-2,10))

# My ARMA(3,0) model's forecasts are consistently higher than the ARIMA(1,1,2) model's. They
# also show an upward trajectory while R's model looks to be a flatter line. Althought we 
# cannot necessarily judge whether which of these model forecasts are better, from a visual
# perspective it looks like the ARMA(3,0) forecasts are more believable.



## TEXTBOOK PROBLEMS
setwd("/Users/noahkawasaki/Desktop/ECON 144/Textbook_data")

## PROBLEM 8.7
# When the US stock market opens in New York, the European markets have already been in session for several hours.
# Does the activity in the European markets have predictive content for the US market? Download the British
# stock index (FTSE) and the SP500 index at the daily frequency. Examine whether FTSE returns can help forecast
# SP500 returns
stocks <- read.xlsx("Chapter8_exercises_data.xlsx", sheet=8, detectDates=TRUE) %>%
  set_names("date", "SP", "FTSE", "SP_Returns", "FTSE_Returns") %>%
  replace(is.na(.), 0) %>%
  mutate(date = mdy(date)) %>%
  dplyr::filter(date > "1999-01-01") %>%
  dplyr::select(date, SP_Returns, FTSE_Returns)

ggplot(stocks, aes(x=date, y=SP_Returns, group=1)) +
  geom_line(color='red', na.rm=TRUE) +
  ggtitle("S&P500 Daily Returns", "2000-2012") +
  xlab("Date") +
  ylab("Rate") +
  theme_grey()

ggplot(stocks, aes(x=date, y=FTSE_Returns, group=1)) +
  geom_line(color='blue', na.rm=TRUE) +
  ggtitle("FTSE Daily Returns", "2000-2012") +
  xlab("Date") +
  ylab("Rate") +
  theme_grey()

# Subset to look closer at relationship
year <-  2005
subset <- stocks %>%
  dplyr::filter(year(date) == year)

ggplot(subset, aes(x=date, group=1)) +
  geom_line(aes(y=SP_Returns), color='red', na.rm=TRUE) +
  geom_line(aes(y=FTSE_Returns), color='blue', na.rm=TRUE) +
  ggtitle("Daily Returns", year) +
  xlab("Date") +
  ylab("Rate")

# From this plot alone it is difficult to tell whether one series leads the other like the housing starts
# and stops from the housing prices. So we can continue by looking at some models and testing.

sp <- ts(stocks$SP_Returns)
ftse <- ts(stocks$FTSE_Returns)

# S&P500 Correlation with FTSE
cor(sp, ftse)
ccf(sp, ftse, ylab="Cross-Correlation Function", main="S&P500 and FTSE CCF")

# Here we do see a moderate positive correlation between the two returns. The CCF plot shows some significant spikes
# and some oscillating behavior. To look more into this I will build a VAR model and look at the summary. From the
# PACF and ACF curves I will use an order of 8.

# VAR model
df <- cbind(sp, ftse)
var <- VAR(df, p=8)
summary(var)

# From here we can see that the S&P500 does have some statistically significant FTSE coefficients, so we can suggest that
# FTSE activity can hold predictive power. However we also see that the S&P500 has much stronger significance in modeling the FTSE
# returns. Even though the FTSE market opens before, the US market has more influence over the former. 

plot(irf(var))

# Looking at the impulse response plots, we also observe that the responses from the S&P500 are more influential than those from
# the FTSE. 

# Granger Test
grangertest(sp~ftse, order=8)
grangertest(ftse~sp, order=8)

# The last thing Ill look at is the Granger causality test. The output tables indicate that lagged values of each index 
# can granger cause the other. Notice though, that the S&P500 is much more significant. From these tests we can conclude
# that the FTSE does have some predictive power in forecasting the S&P returns, but the S&P overall is much more robust in
# understanding the FTSE.



## PROBLEM 11.9
houses <- read_excel("Chapter11_exercises_data.xls", sheet=1)[1:7] %>%
  set_names("date", "ISF", "ISJ", "IAL", "ISF_Growth", "ISJ_Growth", "IAL_Growth") %>%
  mutate(
    date = as.Date(as.yearqtr(date, format = "%YQ%q")),
    ISF_Growth = as.numeric(ISF_Growth),
    ISJ_Growth = as.numeric(ISJ_Growth),
    IAL_Growth = as.numeric(IAL_Growth)
    )

cols2 <- c("ISF"="green", "ISJ"="red", "IAL"="blue")
ggplot(houses, aes(x=date)) +
  geom_line(aes(y=ISF_Growth, color="ISF")) +
  geom_line(aes(y=ISJ_Growth, color="ISJ")) +
  geom_line(aes(y=IAL_Growth, color="IAL")) +
  ggtitle("Quarterly House Price Growth", "1975-2012") +
  xlab("Date") +
  ylab("Rate") +
  scale_color_manual("Legend", values=cols2)
  
houses_df <- data.frame(cbind(houses$ISF_Growth[-1], houses$ISJ_Growth[-1], houses$IAL_Growth[-1])) %>%
  set_names("ISF_Growth", "ISJ_Growth", "IAL_Growth")
head(houses_df)

model <- VAR(houses_df, p=8)
plot(model)

predict = predict(object=model, n.ahead=12)
plot(predict)






























