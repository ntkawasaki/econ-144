# Set global rmarkdown settings
knitr::opts_chunk$set(root.dir='/Users/noahkawasaki/Desktop/ECON 144/Week 9', 
                      echo=FALSE, 
                      warning=FALSE, 
                      message=FALSE, 
                      fig.width=9, fig.height=5, fig.align="center")

# Load libraries
library(tidyverse)
library(tseries)
library(forecast)
library(strucchange)
library(vars)
library(lmtest)
library(car)
library(Metrics)
library(stats)
library(moments)


# Read in data and select needed variables
data <- read_csv("/Users/noahkawasaki/Desktop/ECON 144/Week 9/BTS-FUEL.csv") %>%
  dplyr::select(1, 2, 4) %>%
  set_names("date", "consumption", "cost") 

# Reverse dataframe from early to latest dates, separate train and test sets
df <- data[seq(dim(data)[1],1),][1:192, ]
test_df <- data[seq(dim(data)[1],1),][192:211, ] # 2016 - July 2017

# Make ts objects
cons_ts <- ts(df$consumption)
cost_ts <- ts(df$cost)


## (a)
# Consumption
ggplot(df, aes(x=date, y=consumption)) +
  geom_line(color="#3796db", lwd=0.8) +
  ggtitle("US Consumption of Airline Fuel", "2000-2015") +
  xlab("Date") +
  ylab("Gallons (millions)")
  
par(mfrow=c(2, 1))
acf(df$consumption, main="ACF - Consumption", lag.max=36)
pacf(df$consumption, main="PACF - Consumption", lag.max=36)

# Cost
ggplot(df, aes(x=date, y=cost)) +
  geom_line(color="orange", lwd=0.8) +
  ggtitle("US Cost per Gallon of Airline Fuel", "2000-2015") +
  xlab("Date") +
  ylab("Dollars")

par(mfrow=c(2,1))
acf(df$cost, main="ACF - Cost", lag.max=36)
pacf(df$cost, main="PACF - Cost", lag.max=36)


## (b)
# Create Models
cons_model <- Arima(cons_ts, order=c(2,1,2), seasonal=list(order=c(2,1,2), period=12))
cost_model <- Arima(cost_ts, order=c(1,1,0))

# Add model attributes as data to df for plotting purposes
df <- df %>%
  mutate(
    consumption_fitted = cons_model$fitted,
    consumption_residuals = cons_model$residuals,
    cost_fitted = cost_model$fitted,
    cost_residuals = cost_model$residuals
  )

# Consumption
cons_cols <- c("Fitted Values"="black", "Observed Values"="#3796db")  # Named vector for legend mapping
ggplot(df, aes(x=date, y=consumption)) +
  geom_line(aes(color="Observed Values"), lwd=0.8) +
  geom_line(aes(y=consumption_fitted, color="Fitted Values")) +
  ggtitle("US Consumption of Airline Fuel", "2000-2015") +
  xlab("Date") +
  ylab("Gallons (millions)") +
  scale_color_manual("Legend", values=cons_cols)

summary(cons_model)

# Cost
cost_cols <- c("Fitted Values"="black", "Observed Values"="orange")  # Named vector for legend mapping
ggplot(df, aes(x=date, y=cost)) +
  geom_line(aes(color="Observed Values"), lwd=0.8) +
  geom_line(aes(y=cost_fitted, color="Fitted Values")) +
  ggtitle("US Cost per Gallon of Airline Fuel", "2000-2015") +
  xlab("Date") +
  ylab("Dollars") +
  scale_color_manual("Legend", values=cost_cols)

summary(cost_model)


## (c)
# Consumption
ggplot(df, aes(x=consumption_fitted, y=consumption_residuals)) +
  geom_line(color='green', alpha=0.9) +
  geom_point(color='green', alpha=0.9) +
  geom_hline(yintercept=mean(df$consumption_residuals), color='black', linetype='dashed') +
  ggtitle("Residuals vs Fitted Values", "Consumption Model") +
  xlab("Fitted Values") +
  ylab("Residuals")

# Cost
ggplot(df, aes(x=cost_fitted, y=cost_residuals)) +
  geom_line(color='green', alpha=0.9) +
  geom_point(color='green', alpha=0.9) +
  geom_hline(yintercept=mean(df$cost_residuals), color='black', linetype='dashed') +
  ggtitle("Residuals vs Fitted Values", "Cost Model") +
  xlab("Fitted Values") +
  ylab("Residuals")


## (e)
# Consumption
par(mfrow=c(2,1))
acf(df$consumption_residuals, main="ACF - Consumption Model Residuals")
pacf(df$consumption_residuals, main="PACF - Consumption Model Residuals")

# Cost
par(mfrow=c(2,1))
acf(df$cost_residuals, main="ACF - Cost Model Residuals")
pacf(df$cost_residuals, main="PACF - Cost Model Residuals")

Box.test(df$cost_residuals, lag=(11-2-2))  # 11 lags - 2 AR - 2 MA


## (f)
# Consumption
plot(efp(df$consumption_residuals~1, type="Rec-CUSUM"), main="Consumption Model CUSUM Test")

# Cost
plot(efp(df$cost_residuals~1, type="Rec-CUSUM"), main="Cost Model CUSUM Test")


## (g)
# Consumption
plot(recresid(df$consumption_residuals~1), pch=16, main="Consumption Model Recursive Residuals",
     ylab="Residuals")

# Cost
plot(recresid(df$cost_residuals~1), pch=16, main="Consumption Model Recursive Residuals",
     ylab="Residuals")


## (h)
# Consumption
coeftest(cons_model)  # Statistical Significance of parameters

ggplot(df) +
  geom_qq(aes(sample=consumption_residuals), color='black', alpha=0.6) +
  ggtitle('QQ Normal Plot of Residuals', 'Consumption Model') +
  ylab('Sample Quantiles') +
  xlab('Theoretical Quantiles')

print(paste("Skewness:", skewness(df$consumption_residuals)))
print(paste("Kurtosis:", kurtosis(df$consumption_residuals)))

# Cost
coeftest(cost_model)  # Statistical Significance of parameters

ggplot(df) +
  geom_qq(aes(sample=cost_residuals), color='black', alpha=0.6) +
  ggtitle('QQ Normal Plot of Residuals', 'Cost Model') +
  ylab('Sample Quantiles') +
  xlab('Theoretical Quantiles')

print(paste("Skewness:", skewness(df$cost_residuals)))
print(paste("Kurtosis:", kurtosis(df$cost_residuals)))


## (i)
# Consumption
cons_forecasts <- forecast(cons_model, h=20)
plot(cons_forecasts)

print(paste("RMSE:", rmse(test_df$consumption, cons_forecasts$mean)))

# Cost
cost_forecasts <- forecast(cost_model, h=20)
plot(cost_forecasts)

print(paste("RMSE:", rmse(test_df$cost, cost_forecasts$mean)))


## (i)
# Model
var_df <- data.frame(cbind(cons_ts, cost_ts))  # VAR takes df
var <- VAR(var_df, p=2, season=12)
summary(var)

plot(var)

# CCF
ccf(cons_ts, cost_ts, main="CCF - Consumption and Cost")


## (j)
plot(irf(var))


## (k)
grangertest(cons_ts ~ cost_ts, order=2)
grangertest(cost_ts ~ cons_ts, order=2)


## (l)
var_forecasts = predict(object=var, n.ahead=20)
plot(var_forecasts)

# Access forecast vectors from var_forecasts object
var_cons_forecasts <- var_forecasts[[1]][[1]][,1]
var_cost_forecasts <- var_forecasts[[1]][[2]][,1]

# RMSE
print(paste("RMSE Consumption:", rmse(test_df$consumption, var_cons_forecasts)))
print(paste("RMSE Cost:", rmse(test_df$cost, var_cost_forecasts)))


