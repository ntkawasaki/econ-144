### Project 2

# For this project you will fit a forecasting model with trend, seasonal dummies, and cycles. You may choose any data of your choice 
# (except any data used in class and/or previous homework assignments) provided your time-series data suggest the presence of all 
# three components. To make sure you capture any dynamics related to cycles, you will need a large time horizon, therefore, you 
# might want to make sure that your observations span at least ∼ 10 years. 

# https://www.transtats.bts.gov/fuel.asp

library(tidyverse)
library(tseries)
library(forecast)
library(strucchange)
library(vars)
library(lmtest)
library(car)
library(Metrics)


# Read in data and select needed variables
data <- read_csv("/Users/noahkawasaki/Desktop/ECON 144/Week 9/BTS-FUEL.csv") %>%
  dplyr::select(1, 2, 4) %>%
  set_names("date", "consumption", "cost") 
head(df)
# Reverse dataframe from early to latest dates, separate train and test sets
df <- data[seq(dim(data)[1],1),][1:192, ]
test_df <- data[seq(dim(data)[1],1),][192:211, ] # 2016 - July 2017

# Make ts objects
cons_ts <- ts(df$consumption)
cost_ts <- ts(df$cost)


### For Each Variable  ---------------------------------------------------------------------------------------------------
### Compute the following:
  

## (a) Produce a time-series plot of your data including the respective ACF and PACF plots.

# Consumption
ggplot(df, aes(x=date, y=consumption)) +
  geom_line(color="#3796db", lwd=0.8) +
  ggtitle("US Consumption of Airline Fuel", "2000-2015") +
  xlab("Date") +
  ylab("Millons Gallons")
  
acf(df$consumption, main="ACF - Consumption", lag.max=36)
pacf(df$consumption, main="PACF - Consumption", lag.max=36)

# Cost
ggplot(df, aes(x=date, y=cost)) +
  geom_line(color="orange", lwd=0.8) +
  ggtitle("US Cost per Gallon of Airline Fuel", "2000-2015") +
  xlab("Date") +
  ylab("Dollars")

acf(df$cost, main="ACF - Cost", lag.max=36)
pacf(df$cost, main="PACF - Cost", lag.max=36)
  
  
## (b) Fit a model that includes, trend, seasonality and cyclical components. Make sure to discuss your model in detail.

# Create Models
cons_model <- Arima(cons_ts, order=c(2,1,2), seasonal=list(order=c(2,1,2), period=12))
cost_model <- Arima(cost_ts, order=c(1,1,0))

# Add data to df
df <- df %>%
  mutate(
    consumption_fitted = cons_model$fitted,
    consumption_residuals = cons_model$residuals,
    cost_fitted = cost_model$fitted,
    cost_residuals = cost_model$residuals
  )

# Consumption
summary(cons_model)

cons_cols <- c("Fitted Values"="black", "Observed Values"="#3796db")
ggplot(df, aes(x=date, y=consumption)) +
  geom_line(aes(color="Observed Values"), lwd=0.8) +
  geom_line(aes(y=consumption_fitted, color="Fitted Values")) +
  ggtitle("US Consumption of Airline Fuel", "2000-2015") +
  xlab("Date") +
  ylab("Millons Gallons") +
  scale_color_manual("Legend", values=cons_cols)


# Cost
summary(cost_model)

cost_cols <- c("Fitted Values"="black", "Observed Values"="orange")
ggplot(df, aes(x=date, y=cost)) +
  geom_line(aes(color="Observed Values"), lwd=0.8) +
  geom_line(aes(y=cost_fitted, color="Fitted Values")) +
  ggtitle("US Cost per Gallon of Airline Fuel", "2000-2015") +
  xlab("Date") +
  ylab("Dollars") +
  scale_color_manual("Legend", values=cost_cols)


## (c) Plot the respective residuals vs. fitted values and discuss your observations.

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


## (e) Plot the ACF and PACF of the respective residuals and interpret the plots.

# Consumption
acf(df$consumption_residuals, main="ACF - Consumption Model Residuals")
pacf(df$consumption_residuals, main="PACF - Consumption Model Residuals")

# Cost
acf(df$cost_residuals, main="ACF - Cost Model Residuals")
pacf(df$cost_residuals, main="PACF - Cost Model Residuals")


## (f) Plot the respective CUSUM and interpret the plot.

# Consumption
plot(efp(df$consumption_residuals~1, type="Rec-CUSUM"), main="Consumption Model CUSUM Test")

# Cost
plot(efp(df$cost_residuals~1, type="Rec-CUSUM"), main="Cost Model CUSUM Test")


## (g) Plot the respective Recursive Residuals and interpret the plot.

# Consumption
plot(recresid(df$consumption_residuals~1), pch=16, main="Consumption Model Recursive Residuals",
     ylab="Residuals")

# Cost
plot(recresid(df$cost_residuals~1), pch=16, main="Consumption Model Recursive Residuals",
     ylab="Residuals")


## (h) For your model, discuss the associated diagnostic statistics.

# Consumption
coeftest(cons_model)
summary(cons_model)

ggplot(df) +
  geom_qq(aes(sample=consumption_residuals), color='black', alpha=0.6) +
  ggtitle('QQ Normal Plot of Residuals', 'Consumption Model') +
  ylab('Sample Quantiles') +
  xlab('Theoretical Quantiles')

# Cost
coeftest(cost_model)
summary(cost_model)

ggplot(df) +
  geom_qq(aes(sample=cost_residuals), color='black', alpha=0.6) +
  ggtitle('QQ Normal Plot of Residuals', 'Cost Model') +
  ylab('Sample Quantiles') +
  xlab('Theoretical Quantiles')


## (i) Use your model to forecast 12-steps ahead. Your forecast should include the respective error bands.

# Consumption
cons_forecasts <- forecast(cons_model, h=20)
plot(cons_forecasts)

rmse(test_df$consumption, cons_forecasts$mean)

# Cost
cost_forecasts <- forecast(cost_model, h=20)
plot(cost_forecasts)

rmse(test_df$cost, cost_forecasts$mean)


### Both Variables Combined ----------------------------------------------------------------------------------------------

# (i) Fit an appropriate VAR model using your two variables. Make sure to show the relevant plots and discuss your results 
#     from the fit.

# Model
var_df <- data.frame(cbind(cons_ts, cost_ts))
var <- VAR(var_df, p=2)
summary(var)

# CCF
ccf(cons_ts, cost_ts, main="CCF - Consumption and Cost")


# (j) Compute, plot, and interpret the respective impulse response functions.
plot(irf(var))


# (k) Perform a Granger-Causality test on your variables and discuss your results from the test.
grangertest(cons_ts ~ cost_ts, order=2)
grangertest(cost_ts ~ cons_ts, order=2)


# (l) Use your VAR model to forecast 12-steps ahead. Your forecast should include the respective error bands.
#     Comment on the differences between the two forecasts (VAR vs. ARMA).
var_forecasts = predict(object=var, n.ahead=20)
plot(var_forecasts)

var_cons_forecasts <- var_forecasts[[1]][[1]][,1]
var_cost_forecasts <- var_forecasts[[1]][[2]][,1]

# RMSE
rmse(test_df$consumption, var_cons_forecasts)
rmse(test_df$cost, var_cost_forecasts)





