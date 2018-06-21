#### Homework 2 R script

library(tidyverse)
library(ggplot2)
library(reshape)
library(openxlsx)
library(readxl)
library(data.table)
library(tis)
library(zoo)
library(stats)
library(quantreg)
library(roll)
library(cowplot)
library(psych)


setwd("/Users/noahkawasaki/Desktop/ECON 144/Week 4")


## PROBLEM 1:
# The data file labordata.dat consists of the labor force participation rates by gender 
# (including the total, i.e., sum of male and female) for the years 1948-1991. 
# Each observation is a monthly data point. The objective of this assignment is to fit a 
# trend to the time series data, and based on the best fit model, make a 10 year forecast.

# Load data
readLines('/Users/noahkawasaki/Desktop/ECON 144/Week 4/labordata.dat')
temp <- read_tsv('/Users/noahkawasaki/Desktop/ECON 144/Week 4/labordata.dat', 
         col_names=c('both', 'female', 'male'))

# Manipulate
xmin <- parse_date('1948-01-01', format='%Y-%m-%d')
xmax <- parse_date('1990-12-31', format='%Y-%m-%d')

df <- temp %>%
  mutate(
    date=seq(from=xmin, to=xmax, by='month'),
    tseries=seq(0, 516, length=nrow(temp))  # Non datetime series for regression
  ) %>%
  dplyr::select(date, tseries, male, female, both)  # Re-order


attach(df)
View(df)

# (a) Show a time-series plot of your data. You can show all three variables on the same plot.
cols1 <- c('Total'='black', 'Male'='blue', 'Female'='#E81072')
ggplot(df, aes(x=tseries)) +
  geom_line(aes(y=male, color='Male'), lwd=0.8) +
  geom_line(aes(y=female, color='Female'), lwd=0.8) +
  geom_line(aes(y=both, color='Total'), lwd=0.8) + 
  ylim(25,100) +
  ggtitle('Labor Force Participation Rate ','1948-1991') +
  xlab('Year') +
  ylab('Participation Rate') +
  scale_fill_manual(name="Legend", values=cols1) +
  scale_color_manual(name="Legend", values=cols1) +
  theme_update()


# (b) Fit a linear, polynomial, and exponential model to the female labor force participation rate. 
#     For each model, plot the time series and overlay the respective fit. Discuss your results for each one.

## Linear
lr = lm(female~tseries)
summary(lr)

nberShade(ggplot(data=df, aes(x=date))) +
  geom_line(aes(y=female), color='#E81072', lwd=1) +
  geom_line(aes(y=lr$fitted.values), color='black', linetype='dashed') +
  xlim(xmin, xmax) +
  ylim(25, 60) +
  ggtitle('Linear Model', '1948-1991') +
  xlab('Year') +
  ylab('Female Labor Force Participation Rate') 


## Polynomial
poly <- lm(female~tseries+I(tseries^2))
summary(poly)

nberShade(ggplot(data=df, aes(x=date))) +
  geom_line(aes(y=female), color='#E81072', lwd=1) +
  geom_line(aes(y=poly$fitted.values), color='black', linetype='dashed') +
  xlim(xmin, xmax) +
  ylim(25, 60) +
  ggtitle('Polynomial Model (2 Degrees)', '1948-1991') +
  xlab('Year') +
  ylab('Female Labor Force Participation Rate') 


## Exponential
er <- nls(female~exp(a+b*tseries), data=df, start=list(a=0, b=0))
summary(er)

er_fits <- predict(er, female)
er_resids <- resid(er)

nberShade(ggplot(data=df, aes(x=date))) +
  geom_line(aes(y=female), color='#E81072', lwd=1) +
  geom_line(aes(y=er_fits), color='black', linetype='dashed') +
  xlim(xmin, xmax) +
  ylim(25, 60) +
  ggtitle('Exponential Model', '1948-1991') +
  xlab('Year') +
  ylab('Female Labor Force Participation Rate') 


# (c) Plot the residuals vs. the fitted values for each model in the previous question. 
#     Discuss your results.

## Linear
ggplot(data=df, aes(x=lr$fitted.values, y=lr$residuals)) +
  geom_point(color='#3796db', alpha=0.7, lwd=1) +
  ggtitle('Linear Model', 'Residuals vs Fitted Values') +
  xlab('Fitted') +
  ylab('Residuals') 


## Polynomial
ggplot(data=df, aes(x=poly$fitted.values, y=poly$residuals)) +
  geom_point(color='#3796db', alpha=0.7, lwd=1) +
  ggtitle('Polynomial Model', 'Residuals vs Fitted Values') +
  xlab('Fitted') +
  ylab('Residuals') 

## Exponential
ggplot(data=df, aes(x=er_fits, y=er_resids)) +
  geom_point(color='#3796db', alpha=0.7, lwd=1) +
  ggtitle('Exponential Model', 'Residuals vs Fitted Values') +
  xlab('Fitted') +
  ylab('Residuals') 


# (d) Based on AIC and BIC, choose the best fit model. 
#     Make sure to provide a table with the AIC and BIC computed values.

AIC(lr, poly, er)
BIC(lr, poly, er)  # Polynomial wins


# (e) Using your selected best fit model, forecast and plot your estimated female 
#     labor force participation rate for the years 1992-2002. Your forecast should 
#     include the 95% confidence and prediction intervals. Discuss your results.

# Simulated forcast horizon 1992-2002, * 12 for months
fh <- data.frame(tseries=seq(517, 527, length=120))  
fxmin <- parse_date('1992-01-01', format='%Y-%m-%d')
fxmax <- parse_date('2001-12-31', format='%Y-%m-%d')
fh['date'] <- seq(from=fxmin, to=fxmax, by='month')


pred <- predict(poly, fh, se.fit=TRUE)
summary(pred)

c_interval = as.data.frame(predict(poly, fh, level=0.95, interval="confidence"))
p_interval = as.data.frame(predict(poly, fh, level=0.95, interval="prediction"))


fh['pred'] <- pred$fit
fh['p_upr'] <- p_interval$upr
fh['p_lwr'] <- p_interval$lwr
fh['c_upr'] <- c_interval$upr
fh['c_lwr'] <- c_interval$lwr

cols2 <- c('Prediction Interval'='red', 'Confidence Interval'='blue', 'Fitted Values'='black')
ggplot(fh, aes(x=date)) +
  geom_ribbon(aes(ymin=p_lwr, ymax=p_upr, fill='Prediction Interval'), alpha=0.5) +
  geom_ribbon(aes(ymin=c_lwr, ymax=c_upr, fill='Confidence Interval'), alpha=0.6) +
  geom_line(aes(y=pred, color='Fitted Values'), lwd=1) +
  ylim(57.5, 62.5) +
  ggtitle('Forecasted Female Labor Participation Rate', '1992-2002') +
  xlab('Year') +
  ylab('Forecasted Rate') +
  scale_fill_manual(name="Shades", values=cols2) +
  scale_color_manual(name="Lines", values=cols2) +
  theme_grey()

# (f) Fit a Holt-Winters filter to your data and show the fit. 
#     How does this model compare to your best fit model?
female_ts <- ts(female, start=1948, frequency=12)
hw = HoltWinters(female_ts)
summary(hw)

cols3 <- c('Fitted Values'='black', 'Holt Winters Prediction'='#E81072')
nberShade(ggplot(data=df, aes(x=date))) +
  geom_line(aes(y=female, color='Holt Winters Prediction'), lwd=1) +
  geom_line(aes(y=hw$x, color='Fitted Values')) +
  xlim(xmin, xmax) +
  ylim(30, 60) +
  ggtitle('Holt Winters', '1948-1991') +
  xlab('Year') +
  ylab('Female Labor Force Participation Rate') +
  scale_color_manual(name='Legend', values=cols3) +
  theme_gray()

# (g) Based on the Holt-Winters fit, forecast and plot your estimated female labor force 
#     participation rate for the years 1992-2002. Your forecast should include the error bands. 
#     Discuss your results.
fh['hw_pred'] <- predict(hw, 120, prediction.interval=TRUE, level=0.5)

quartz()
plot(hw, fh$hw_pred, ylab="Female Labor Force Participation Rate", xlab="Year", xlim=c(1948,1999))



### TEXTBOOK PROBLEMS
setwd("/Users/noahkawasaki/Desktop/ECON 144/Textbook_data")



## PROBLEM 3.2
df32_temp <- read.xlsx("Chapter3_exercises_data.xlsx", sheet=2, detectDates=TRUE)
df32 <- df32_temp %>%
  mutate(
    inflation=log(cpi)-log(lag(cpi)),
    nomrate_monthly=(100*(((1+(nomrate/100))^(1/12))-1)),
    ex_post_real=nomrate_monthly-inflation
  )
head(df32)

# 3.1 Data
df31_temp <- read.xlsx("Chapter3_exercises_data.xlsx", sheet=1, detectDates=TRUE)[,1:3]
df31 <- df31_temp %>%
  mutate(
    rpce_growth=log(rpce)-log(lag(rpce)),
    rdpi_growth=log(rdpi)-log(lag(rdpi))
  )
head(df31)

# Join
join <- merge(df32, df31, by='date')
head(join)

lr_join <- lm(rpce_growth~rdpi_growth+ex_post_real, data=join)
summary(lr_join)



## PROBLEM 4.4
# Load data
annual_temp <- read_excel("Chapter4_exercises_data.xls", sheet=1)
setnames(annual_temp, c('year', 'price', 'interest'))

quarterly_temp <- read_excel("Chapter4_exercises_data.xls", sheet=2)
setnames(quarterly_temp, c('quarter', 'price', 'interest'))

# Set up dataframes
annual <- annual_temp %>%
  mutate(
    price_growth=log(price)-log(lag(price)),
    interest_growth=log(interest)-log(lag(interest)),
    price_growth_lag1=lag(price_growth, 1),
    price_growth_lag2=lag(price_growth, 2),
    price_growth_lag3=lag(price_growth, 3),
    price_growth_lag4=lag(price_growth, 4)
  )
head(annual)

quarterly <- quarterly_temp %>%
  mutate(
    price_growth=log(price)-log(lag(price)),
    interest_growth=log(interest)-log(lag(interest)),
    price_growth_lag1=lag(price_growth, 1),
    price_growth_lag2=lag(price_growth, 2),
    price_growth_lag3=lag(price_growth, 3),
    price_growth_lag4=lag(price_growth, 4)
  )
head(quarterly)

# Annual Models
lr44a_1 <- lm(price_growth~price_growth_lag1, data=annual)
summary(lr44a_1) 

lr44a_2 <- lm(price_growth~price_growth_lag1+price_growth_lag2, data=annual)
summary(lr44a_2)  # Optimal annual model

lr44a_3 <- lm(price_growth~price_growth_lag1+price_growth_lag2+price_growth_lag3, data=annual)
summary(lr44a_3)  

lr44a_4 <- lm(price_growth~price_growth_lag1+price_growth_lag2+price_growth_lag3+price_growth_lag4, data=annual)
summary(lr44a_4)

AIC(lr44a_1, lr44a_2, lr44a_3, lr44a_4)
BIC(lr44a_1, lr44a_2, lr44a_3, lr44a_4)


# Quarterly Models
lr44q_1 <- lm(price_growth~price_growth_lag1, data=quarterly)
summary(lr44q_1) 

lr44q_2 <- lm(price_growth~price_growth_lag1+price_growth_lag2, data=quarterly)
summary(lr44q_2)  

lr44q_3 <- lm(price_growth~price_growth_lag1+price_growth_lag2+price_growth_lag3, data=quarterly)
summary(lr44q_3)  # Optimal quarterly model

lr44q_4 <- lm(price_growth~price_growth_lag1+price_growth_lag2+price_growth_lag3+price_growth_lag4, data=quarterly)
summary(lr44q_4)

AIC(lr44q_1, lr44q_2, lr44q_3, lr44q_4)
BIC(lr44q_1, lr44q_2, lr44q_3, lr44q_4)


# Favorite model: lr44q_3
# Recursive Coefficients
X <- as.matrix(na.omit(quarterly[c('price_growth', 'price_growth_lag1', 'price_growth_lag2', 'price_growth_lag3')]))
explanatories <- X[,2:4]
target <- as.matrix(X[,1])

rec_model <- quantreg::lm.fit.recursive(X=explanatories, y=target)

lag1_rec_coeffs <- rec_model[2,]
lag2_rec_coeffs <- rec_model[3,]
lag3_rec_coeffs <- rec_model[4,]
rec_date_vector <- seq(from=1979, to=1991, length=length(rec_model[1,]))
rec_df <- data.frame(rec_date_vector, lag1_rec_coeffs, lag2_rec_coeffs, lag3_rec_coeffs)

cols4 <- c("Lag 1 Coefficients"="red","Lag 2 Coefficients"="blue","Lag 3 Coefficients"="green")
ggplot(data=rec_df, aes(x=rec_date_vector)) +
  geom_line(aes(y=lag1_rec_coeffs, color='Lag 1 Coefficients'), lwd=0.7, na.rm=TRUE) +
  geom_line(aes(y=lag2_rec_coeffs, color='Lag 2 Coefficients'), lwd=0.7, na.rm=TRUE) +
  geom_line(aes(y=lag3_rec_coeffs, color='Lag 3 Coefficients'), lwd=0.7, na.rm=TRUE) +
  ggtitle('Recursive Linear Regression Coefficients', '1979-1991') +
  ylab('Coefficient') +
  xlab('Year') +
  scale_color_manual(name="Legend", values=cols4) +
  theme_gray()

# Rolling Coefficients
rollover <- roll_lm(x=explanatories, y=target, width=30)
rollover$coefficients

lag1_roll_coeffs <- rollover$coefficients[,2]
lag2_roll_coeffs <- rollover$coefficients[,3]
lag3_roll_coeffs <- rollover$coefficients[,4]
roll_date_vector <- seq(from=1948, to=1991, length=length(rec_model[1,]))
roll_df <- data.frame(roll_date_vector, lag1_roll_coeffs, lag2_roll_coeffs, lag3_roll_coeffs)

ggplot(data=roll_df, aes(x=roll_date_vector)) +
  geom_line(aes(y=lag1_roll_coeffs, color='Lag 1 Coefficients'), lwd=0.7, na.rm=TRUE) +
  geom_line(aes(y=lag2_roll_coeffs, color='Lag 2 Coefficients'), lwd=0.7, na.rm=TRUE) +
  geom_line(aes(y=lag3_roll_coeffs, color='Lag 3 Coefficients'), lwd=0.7, na.rm=TRUE) +
  ggtitle('Rolling Linear Regression Coefficients', '1948-1991 (30 observation window)') +
  ylab('Coefficient') +
  xlab('Year') +
  scale_fill_manual(name="Legend", values=cols4) +
  scale_color_manual(name="Legend", values=cols4) +
  theme_gray()



## PROBLEM 4.6
greenbook_temp <- read_excel("Chapter4_exercises_data.xls", sheet=3)
setnames(greenbook_temp, c('date', 'actual_growth', 'forecasted_growth'))

# Set up dataframe
greenbook <- greenbook_temp %>%
  mutate(
    date=as.Date(as.yearqtr(greenbook_temp$date, format="%Y:Q%q")),
    forecast_errors=actual_growth-forecasted_growth
  )

# Plots
p1 <- ggplot(greenbook, aes(x=date)) +
  geom_line(aes(y=actual_growth), color='blue') +
  geom_hline(yintercept=mean(greenbook$actual_growth), linetype='dashed', color='black', alpha=0.8) +
  ylim(-4, 4) +
  ggtitle('Actual GDP Growth') +
  xlab('Year') +
  ylab('Rate')

p2 <- ggplot(greenbook, aes(x=date)) +
  geom_line(aes(y=forecasted_growth), color='green') +
  geom_hline(yintercept=mean(greenbook$forecasted_growth), linetype='dashed', color='black', alpha=0.8) +
  ylim(-4, 4) +
  ggtitle('Forecasted GDP Growth') +
  xlab('Year') +
  ylab('Rate')

p3 <- ggplot(greenbook, aes(x=date)) +
  geom_line(aes(y=forecast_errors), color='red') +
  geom_hline(yintercept=mean(greenbook$forecast_errors), linetype='dashed', color='black', alpha=0.8) +
  ylim(-4, 4) +
  ggtitle('Forecast Errors') +
  xlab('Year') +
  ylab('Rate')
  
plot_grid(p1, p2, p3, ncol=1)

# Descriptive statistics
psych::describe(greenbook, na.rm=TRUE)

# ACF and PACFs
acf(greenbook$actual_growth)
pacf(greenbook$actual_growth)

acf(greenbook$forecasted_growth)
pacf(greenbook$forecasted_growth)

acf(greenbook$forecast_errors)
pacf(greenbook$forecast_errors)



## PROBLEM 5.4
amex_temp <- read_xlsx("Chapter5_exercises_data.xlsx", sheet=3)[,1:2]
setnames(amex_temp, c('date', 'price'))

amex <- amex_temp %>%
  mutate(
    return=log(price)-log(lag(price))
  )

sp500_temp <- read_xlsx("Chapter5_exercises_data.xlsx", sheet=3)[,4:5]
setnames(sp500_temp, c('date', 'price'))

sp500 <- sp500_temp %>%
  mutate(
    return=log(price)-log(lag(price))
  )

# ACF and PACF plots
acf(amex$return, na.action=na.omit)
pacf(amex$return, na.action=na.omit)

acf(sp500$return, na.action=na.omit)
pacf(sp500$return, na.action=na.omit)




















