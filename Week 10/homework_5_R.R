### Homework 5 R Script

library(tidyverse)
library(tseries)
library(forecast)
library(readxl)
library(TSA)
library(rugarch)
library(fGarch)
library(quantmod)


setwd('/Users/noahkawasaki/Desktop/ECON 144/Textbook_data')

### TEXTBOOK A

## Problem 14.2
m1 <- garch.sim(alpha=c(3, 0.35), beta=c(0.6), n=500)  # 0.75
mean1 <- mean(m1)
sd1 <- sd(m1)
sm1 <- (m1-mean1)/sd1
sds1 <- sqrt((m1-mean1)^2)

m2 <- garch.sim(alpha=c(3, 0.1), beta=c(0.6), n=500)  # 0.16
mean2 <- mean(m2)
sd2 <- sd(m2)
sm2 <- (m2-mean2)/sd2
sds2 <- sqrt((m2-mean2)^2)


# Time Series
ggplot(data.frame(m1), aes(x=seq(from=1, to=length(m1)), y=m1)) +
  geom_line(color="red") +
  ylim(-18, 18) +
  ggtitle("GARCH(1,1)", "High Persistence") +
  xlab("Index") +
  ylab("Value")

ggplot(data.frame(m2), aes(x=seq(from=1, to=length(m2)), y=m2)) +
  geom_line(color="blue") +
  ylim(-18, 18) +
  ggtitle("GARCH(1,1)", "Low Persistence") +
  xlab("Index") +
  ylab("Value")


# Conditional Standard Deviations
ggplot(data.frame(m1), aes(x=seq(from=1, to=length(m1)), y=sds1)) +
  geom_line(color="red") +
  ylim(0,20) +
  ggtitle("GARCH(1,1) Conditional Standard Deviation", "High Persistence") +
  xlab("Index") +
  ylab("Value")

ggplot(data.frame(m1), aes(x=seq(from=1, to=length(m1)), y=sds2)) +
  geom_line(color="blue") +
  ylim(0,20) +
  ggtitle("GARCH(1,1) Conditional Standard Deviation", "Low Persistence") +
  xlab("Index") +
  ylab("Value")


# Histogram of original time series
ggplot(data.frame(m1), aes(x=m1)) +
  geom_histogram(fill="red", color="black") +
  xlim(-21, 21) +
  ggtitle("Histogram GARCH(1,1)", "High Persistence") +
  xlab("Value") +
  ylab("Count") 

ggplot(data.frame(m2), aes(x=m2)) +
  geom_histogram(fill="blue", color="black") +
  xlim(-21, 21) +
  ggtitle("Histogram GARCH(1,1)", "Low Persistence") +
  xlab("Value") +
  ylab("Count")


# Histogram of standardized time series
ggplot(data.frame(sm1), aes(x=sm1)) +
  geom_histogram(fill="red", color="black") +
  xlim(-4, 4) +
  ggtitle("Histogram of Standardized GARCH(1,1)", "High Persistence") +
  xlab("Value") +
  ylab("Count") 

ggplot(data.frame(sm2), aes(x=sm2)) +
  geom_histogram(fill="blue", color="black") +
  xlim(-4, 4) +
  ggtitle("Histogram of Standardized GARCH(1,1)", "Low Persistence") +
  xlab("Value") +
  ylab("Count")


# ACF/PACF original time series
par(mfrow=c(1,2))
acf(m1, main="ACF - GARCH(1,1) High Persistence")
pacf(m1, main="PACF - GARCH(1,1) High Persistence")

par(mfrow=c(1,2))
acf(m2, main="ACF - GARCH(1,1) Low Persistence")
pacf(m2, main="PACF - GARCH(1,1) Low Persistence")

# ACF/PACF squared time series
par(mfrow=c(1,2))
acf(m1^2, main="ACF - GARCH(1,1) Squared, High Persistence")
pacf(m1^2, main="PACF - GARCH(1,1) Squared, High Persistence")

par(mfrow=c(1,2))
acf(m2^2, main="ACF - GARCH(1,1) Squared, Low Persistence")
pacf(m2^2, main="PACF - GARCH(1,1) Squared, Low Persistence")


# ACF/PACF standardized series
par(mfrow=c(1,2))
acf(sm1, main="ACF - GARCH(1,1) Standardized, High Persistence")
pacf(sm1, main="PACF - GARCH(1,1) Standardized, High Persistence")

par(mfrow=c(1,2))
acf(sm2, main="ACF - GARCH(1,1) Standardized, Low Persistence")
pacf(sm2, main="PACF - GARCH(1,1) Standardized, Low Persistence")

# ACF/PACF squared standardized series
par(mfrow=c(1,2))
acf(sm1^2, main="ACF - GARCH(1,1) Standardized Squared, High Persistence")
pacf(sm1^2, main="PACF - GARCH(1,1) Standardized Squared, High Persistence")

par(mfrow=c(1,2))
acf(sm2^2, main="ACF - GARCH(1,1) Standardized Squared, Low Persistence")
pacf(sm2^2, main="PACF - GARCH(1,1) Standardized Squared, Low Persistence")



## Problem 14.3
sp <- read_xls("/Users/noahkawasaki/Desktop/ECON 144/Textbook_data/Chapter14_exercises_data.xls")
df <- data.frame(sp$Date, sp$`Adj Close`) %>%
  set_names("date", "adjusted") %>%
  mutate(
    return = log(adjusted) - log(lag(adjusted))
  )
df <- na.omit(df)


# Plot time series and return series
ggplot(df, aes(x=date, y=adjusted)) +
  geom_line(color="red") +
  ggtitle("S&P500 Price", "2000-2013") +
  xlab("Date") +
  ylab("Price")

ggplot(df, aes(x=date, y=return)) +
  geom_line(color="red", na.rm=TRUE) +
  ggtitle("S&P500 Returns", "2000-2013") +
  xlab("Date") +
  ylab("Rate")


# ACF and PACFs
par(mfrow=c(1,2))
acf(df$return, main="ACF - S&P500 Returns")
pacf(df$return, main="PACF - S&P500 Returns")

par(mfrow=c(1,2))
acf((df$return)^2, main="ACF - S&P500 Squared Returns")
pacf((df$return)^2, main="PACF - S&P500 Squared Returns")


# Find best ARCH Model
arch = garch(df$return, order=c(0,11), trace=F)
summary(arch)
logLik(arch)


par(mfrow=c(1,2))
acf(arch$residuals[12:length(arch$residuals)], main="ACF - ARCH(11) Residuals")
pacf(arch$residuals[12:length(arch$residuals)], main="PACF - ARCH(11) Residuals")

# GARCH Model
garch <- garchFit(~garch(2,2), data=df$return, trace=FALSE)
summary(garch)


par(mfrow=c(1,2))
acf(garch@residuals, main="ACF - GARCH(2,2) Residuals")
pacf(garch@residuals, main="ACF - GARCH(2,2) Residuals")


## Problem 14.4
# One and two step ahead forecasts
predicts <- predict(garch, n.ahead=2, plot=TRUE)



### TEXTBOOK B

## Problem 14.1
## a
getSymbols("^NYA", from="1988-01-01", to="2001-12-31")
nyse <- data.frame(log(NYA$NYA.Adjusted) - log(lag(NYA$NYA.Adjusted))[-1])
nyse <- tibble::rownames_to_column(nyse) %>%
  set_names("date", "return")

train <- nyse[1:3461, ]
test <- nyse[3462:nrow(nyse), ]

ts <- ts(train$return)
ts_test <- ts(test$return)

# Plot
ggplot(train, aes(x=seq(from=1, to=length(return)), y=return, group=1)) +
  geom_line(color="green") +
  ggtitle("NYSE Daily Returns", "1988-2001") +
  xlab("Index") +
  ylab("Rate")

# AR
m = auto.arima(ts)
summary(m)

resids <- m$residuals[13:length(archx$residuals)]

par(mfrow=c(1,2))
acf(resids)
pacf(resids)

par(mfrow=c(1,2))
acf((resids)^2)
pacf((resids)^2)

# GARCH
garch <- garchFit(~garch(1,2), data=resids, trace=FALSE)
summary(garch)

par(mfrow=c(1,2))
acf((garch@residuals)^2)
pacf((garch@residuals)^2)

## b
model = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(2, 2)),
  mean.model = list(armaOrder = c(5, 3), include.mean = TRUE),
  distribution.model = "sstd")


## Problem 14.4
ar5 <- arma(ts, order=c(5,0))
plot(ar5)

smooth <- ets(ts, model="ANN", alpha=0.10)
plot(smooth)

garch11 <- garchFit(~garch(1,2), data=ts, trace=FALSE)
plot(garch11)

## Problem 14.5





























































