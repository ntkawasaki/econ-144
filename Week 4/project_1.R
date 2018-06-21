#### Project 1 R script
library(tidyverse)
library(ggplot2); theme_set(theme_dark())
library(reshape)
library(data.table)
library(grid)
library(gridExtra)
library(zoo)
library(tis)
library(forecast)
library(cowplot)  # Plot grids


# Load data
temp_c02 <- read.table(file='/Users/noahkawasaki/Desktop/ECON 144/Week 4/c02.txt')
setnames(temp_c02, c('year', 'month_num', 'ts_raw', 'ppm', 'trend'))

# Mutate dataframe and convert month_num and year with '01' as day into a date type. Select only the variables
# needed. 
c02 <- temp_c02 %>%
  mutate(
    date=as.Date(paste(temp_c02$month_num, '01', temp_c02$year), format="%m %d %Y"),
    ts=seq(1980, 2018, length=nrow(temp_c02)),
    # Manually creating seasonal dummies for the full model in part 2 because because the 'trend'
    # part of tslm() function will be a linear model, not my polynomial model. 
    is_1 = month_num==1,
    is_2 = month_num==2,
    is_3 = month_num==3,
    is_4 = month_num==4,
    is_5 = month_num==5,
    is_6 = month_num==6,
    is_7 = month_num==7,
    is_8 = month_num==8,
    is_9 = month_num==9,
    is_10 = month_num==10,
    is_11 = month_num==11,
    is_12 = month_num==12
    ) %>%
  dplyr::select(
    date,
    ts, 
    ppm,
    is_1, is_2, is_3, is_4, is_5, is_6, is_7, is_8, is_9, is_10, is_11, is_12
    )

# Dataframe looks good.
head(c02)

## 1. Modeling and Forecasting Trend (5% each)
# (a) Show a time-series plot of your data.
ggplot(c02, aes(x=date, y=ppm)) +
  geom_line(color='green') +
  geom_hline(yintercept=mean(c02$ppm), color='grey', linetype='dashed') +
  ggtitle(expression('Global CO'[2]*' Monthly Means'), '1980-2018') +
  xlab('Date') +
  ylab('PPM')
  

# (b) Does your plot in (a) suggest that the data are covariance stationary? Explain your answer.
# No, this time series plot is not covariance stationary in respect to carbon dioxide ppm. We can see that there is no mean reversion. If we take
# the log first difference as a growth rate variable we may observe this.


# (c) Plot and discuss the ACF and PACF of your data.
# Because the default acf and pacf function plots are ugly, I will create my own version with ggplot.
# The acf object
acf_ppm <- acf(c02$ppm, lag.max=20, plot=FALSE)
# Turn into a dataframe to send to ggplot
acf_ppm_df <- as.data.frame(acf_ppm$acf)
# Calculate CI with qnorm and n.used attribute of acf_ppm, which is just the number of observations in the 
# time series.
ci_acf_ppm <- qnorm((1 + 0.95)/2)/sqrt(acf_ppm$n.used)

ggplot(acf_ppm_df, aes(x=seq(nrow(acf_ppm_df)-1, from=0), y=V1)) +
  geom_col(fill='orange') +
  geom_hline(yintercept=ci_acf_ppm, linetype='dashed') +
  geom_hline(yintercept=-ci_acf_ppm, linetype='dashed') +
  geom_hline(yintercept=0) +
  ylim(-0.25, 1) +
  ggtitle('ACF', expression('CO'[2]*' Monthly Means')) +
  xlab('Lag') +
  ylab('ACF')
# The Autocorrelation Function of global carbon dioxide monthly means suggests strong time dependence. Even increasing
# the maximum lag in the plot shows a slow decline in the autocorrelation scores. In assessing this plot with domain knowledge,
# this observation seems reasonable. The first time series plot of the monthly means showed a clear upward trend,
# and we know that heat levels change continuously (temperature cannot go from 70 degrees to 72 degrees without
# first hitting 71 degrees). 
pacf_ppm <- pacf(c02$ppm, lag.max=20, plot=FALSE)
pacf_ppm_df <- as.data.frame(pacf_ppm$acf)
ci_pacf_ppm <- qnorm((1 + 0.95)/2)/sqrt(pacf_ppm$n.used)

ggplot(pacf_ppm_df, aes(x=seq(nrow(pacf_ppm_df), from=1), y=V1)) +
  geom_col(fill='orange') +
  geom_hline(yintercept=ci_pacf_ppm, linetype='dashed') +
  geom_hline(yintercept=-ci_pacf_ppm, linetype='dashed') +
  geom_hline(yintercept=0) +
  ylim(-0.5, 1) +
  ggtitle('PACF', expression('CO'[2]*' Monthly Means')) +
  xlab('Lag') +
  ylab('PACF')
# The Partial Autocorrelation Function of global carbon dioxide monthly means has a high score at one period of lag, and 
# no evidence of time depence past this. Again, this concept aligns with our basic intuition on heat levels that only
# the first previous value should affect the current. Note that we also observe oscillation within th confidence interval.
# Both the ACF and PACF suggest that this process is an Autoregressive process.


# (d) Fit a linear and nonlinear (e.g., polynomial, exponential, quadratic + periodic, etc.) model to your series. 
#     In one window, show both figures of the original times series plot with the respective fit.
# Linear
lr <- lm(ppm~ts, data=c02)

# Color mapper to pass to ggplot for legend
cols2 <- c('Observed Values'='green', 'Fitted Values'='black')
ggplot(c02, aes(x=date)) +
  geom_line(aes(y=ppm, color='Observed Values')) +
  geom_line(aes(y=lr$fitted.values, color='Fitted Values'), linetype='dashed') +
  ggtitle(expression('Global CO'[2]*' Monthly Means'), 'Linear Model, 1980-2018') +
  xlab('Date') +
  ylab('PPM') +
  scale_color_manual(name="Lines", values=cols2)

# Nonlinear: Polynomial Degree 2
poly <- lm(ppm~ts+I(ts^2), data=c02)

ggplot(c02, aes(x=date)) +
  geom_line(aes(y=ppm, color='Observed Values')) +
  geom_line(aes(y=poly$fitted.values, color='Fitted Values'), linetype='dashed') +
  ggtitle(expression('Global CO'[2]*' Monthly Means'), 'Nonlinear Model, 1980-2018') +
  xlab('Date') +
  ylab('PPM') +
  scale_color_manual(name="Lines", values=cols2)


# (e) For each model, plot the respective residuals vs. fitted values and discuss your observations.
# Linear
ggplot(c02) +
  geom_point(aes(x=lr$fitted.values, y=lr$residuals), color='orange', alpha=0.9) +
  geom_hline(yintercept=mean(lr$residuals), color='black', linetype='dashed') +
  ylim(-6,6) +
  ggtitle('Fitted Values vs Residuals', 'Linear Model') +
  xlab('Fitted Values') +
  ylab('Residuals') 
# The linear model fitted values and residuals plot tells us that this model roughly underpredicts, overpredicts, and then 
# underpredicts again on the actual values of the time series. This U-shaped representation of our fits and 
# residuals tells us that there still remains some uncaptured structure in the data that the linear model 
# is not sufficiently incorporating. Note the dashed line representing the mean of the residuals, essentially being zero.


# Nonlinear
ggplot(c02) +
  geom_point(aes(x=poly$fitted.values, y=poly$residuals), color='orange', alpha=0.9) +
  geom_hline(yintercept=mean(poly$residuals), color='black', linetype='dashed') +
  ylim(-6,6) +
  ggtitle('Fitted Values vs Residuals', 'Nonlinear Model') +
  xlab('Fitted Values') +
  ylab('Residuals') 
# The nonlinear models fitted values show a great improvement relative to the linear model. In terms of the horizontal
# aspect of the plot, the fitted values look uniformly distributed over the range of the x-axis. 
# The vertical aspect, being the residuals, shows more densely clustered positive points, and more varied 
# negative points. There is much less noticable structure in this plot. Also note the dashed line representing the mean 
# of the residuals, essentially being zero. When scaling the y-axis equally to the linear residuals, we note much more
# dense points near 0. We can conclude that the polynomial model explains global carbon dioxide 
# levels significantly better than a plain linear model. 


# (f) For each model, plot a histogram of the residuals and discuss your observations.
# Linear
ggplot(c02) +
  geom_histogram(aes(x=lr$residuals), bins=40, fill='orange', color='black', alpha=1) +
  xlim(-6,6) +
  ggtitle('Histogram of Residuals', 'Linear Model') +
  xlab('Value') +
  ylab('Frequency') 
# From looking at this histogram alone, we can see that the linear models residuals are somewhat normally distributed.
# Rememeber from the time series and fitted values vs residual plot, there was an underpredicted, overpredicted,
# and underpredicted pattern. This histogram tells us that these under and over predictions cancel each other out
# which could deceptively tell us that the linear model was a good model. However, we know from above that it 
# fails to capture cyclical components.

# Nonlinear
ggplot(c02) +
  geom_histogram(aes(x=poly$residuals), bins=40, fill='orange', color='black', alpha=1) +
  xlim(-6,6) +
  ggtitle('Histogram of Residuals', 'Nonlinear Model') +
  xlab('Value') +
  ylab('Frequency') 
# The nonlinear model residuals, on the other hand, are negatively skewed. While the mean value is 0, we can see
# that there is more dense clustering of positive values and more sparse clustering of the negative values. Since
# residuals are the difference of observed and predicted values, this model tends to underpredict with a lower range of 
# values and overpredict with a larger range of values. This could be due to the rising trend of the carbon dioxide
# levels mixed with cyclical components that are not explicitly captured in this non linear model.


# (g) For each model, discuss the associated diagnostic statistics (R2, t−distribution, F−distribution, etc.)
# Linear
summary(lr)
# The summary tells us that both the intercept and the ts variables are statistically significant from their low p-values and
# high t values. The adjusted R2 score is high at 98.56%, meaning that the linear model performs quite well and that this 
# particular data is arguably simple. The F-statistic is essentially zero, meaning that some variable (ts) in this model 
# is statistically significant.

cooks_lr <- cooks.distance(lr)
p1 <- ggplot(c02, aes(x=seq(length(cooks_lr)), y=cooks_lr)) +
  geom_line(color='black', size=0.3, alpha=0.8) +
  geom_point(color='orange', size=0.7) +
  ylim(0, 0.03) +
  ggtitle("Cook's Distance", 'Linear Model') +
  xlab('Index') +
  ylab("Cook's Distance")

p2 <- ggplot(data=c02) +
  geom_qq(aes(sample=lr$residuals), color='orange', alpha=0.6) +
  ggtitle('QQ Normal Plot of Residuals', 'Linear Model') +
  ylab('Sample Quantiles') +
  xlab('Theoretical Quantiles')

plot_grid(p1, p2, nrow=1)
# We can also examine the Cook's Distance and QQ Normal plot of the residuals. The Cook's distances
# are all very low (note the y-axis scale), indicating that there are no extreme values in our data 
# that could influence our model. The highest value is just between 0.03 and 0.02. The QQ normal plot aligns with our observation on the residuals histogram, 
# that the linear models residuals appear somewhat normally distributed. 

# Nonlinear
summary(poly)
# The polynomial model has a slightly higher R2 score at 99.29%, which is a 0.73% improvement on the
# linear model. The p-values and t values of both the ts and quadtratic ts paramteres indicate statistical
# significance. And the F-statistic also tells us that some parameters in this model are statistically significant.

cooks_poly <- cooks.distance(poly)
p3 <- ggplot(c02, aes(x=seq(length(cooks_poly)), y=cooks_poly)) +
  geom_line(color='black', size=0.3, alpha=0.8) +
  geom_point(color='orange', size=0.7) +
  ylim(0, 0.03) +
  ggtitle("Cook's Distance", 'Nonlinear Model') +
  xlab('Index') +
  ylab("Cook's Distance")

p4 <- ggplot(data=c02) +
  geom_qq(aes(sample=poly$residuals), color='orange', alpha=0.6) +
  ggtitle('QQ Normal Plot of Residuals', 'Nonlinear Model') +
  ylab('Sample Quantiles') +
  xlab('Theoretical Quantiles')

plot_grid(p3, p4, nrow=1)
# The Cook's distances for the polynomial model are also low. The maximum value here is only slightly above
# 0.02, which is lower than the linear model's. Likewise, the polynomial model does not appear to have 
# suffered from any influential data points. We can however, see (again) that the polynomial model residuals
# are less characteristic of a normal distribution compared to the linear model residuals. 

# (h) Select a trend model using AIC and one using BIC (show the values obtained from each criterion). 
#     Do the selected models agree?
aic <- AIC(lr, poly)
bic <- BIC(lr, poly)
# Set up dataframe with both AIC and BIC scores, add row names.
temp_scores <- data.frame(aic[,2], bic[,2], row.names=c('Linear', 'Nonlinear'))
# Change column names
scores <- setnames(temp_scores, c('AIC', 'BIC'))
grid.table(scores)
# Both the AIC and BIC scores indicate the nonlinear model should be the selected trend model.


# (i) Use your preferred model to forecast h-steps (at least 16) ahead. 
#     Your forecast should include the respective uncertainty prediction interval. 
#     Depending on your data, h will be in days, months, years, etc.
# Simulated monthly forcast horizon Feb 2018 - Jan 2020
fmin <- parse_date('2018-02-01', '%Y-%m-%d')
fmax <- parse_date('2020-01-01', '%Y-%m-%d')

forecasts <- data.frame(
  ts = seq(2018.1, 2020, length=24),
  date = seq(from=fmin, to=fmax, by='month')
  ) %>%
  # Add seasonal dummy variables for the full model.
  mutate(
    is_1 = month(date)==1,  # Will return TRUE on condition, FALSE otherwise.
    is_2 = month(date)==2,
    is_3 = month(date)==3,
    is_4 = month(date)==4,
    is_5 = month(date)==5,
    is_6 = month(date)==6,
    is_7 = month(date)==7,
    is_8 = month(date)==8,
    is_9 = month(date)==9,
    is_10 = month(date)==10,
    is_11 = month(date)==11,
    is_12 = month(date)==12
  )

head(forecasts)

# Make predictions from nonlinear model
poly_pred <- predict(poly, newdata=forecasts, se.fit=TRUE)

# Get prediction and confidence intervals
poly_c_int = as.data.frame(predict(poly, forecasts, level=0.95, interval="confidence"))
poly_p_int = as.data.frame(predict(poly, forecasts, level=0.95, interval="prediction"))

# To plot forecasts with original data in ggplot, i will stack the forecasts and c02 dataframes into one dataframe. I
# will also add the nonlinear models fitted values. The overall number of rows will be 457 original points + 24 forecasts 
# = 481, so I will create dummy NA vectors to pad space on respective variables so each variable vector is 481 in length. 
na_457 <- rep(NA, length.out=457)
na_24 <- rep(NA, length.out=24)

# Coerce all to forecasts data to new combined dataframe based on the raw forecasts dataframe. Make a new one
# from the base forecasts one so I can reuse it for the full model forecasts.
poly_df <- data.frame(
    date = c(c02$date, forecasts$date),
    ts = c(c02$ts, forecasts$ts),
    pred = c(na_457, poly_pred$fit),
    p_upr = c(na_457, poly_p_int$upr),
    p_lwr = c(na_457, poly_p_int$lwr),
    c_upr = c(na_457, poly_c_int$upr),
    c_lwr = c(na_457, poly_c_int$lwr),
    ppm = c(c02$ppm, na_24),
    fits = c(poly$fitted.values, na_24)
  )

# This is now a dataframe with the ts series, date, point predictions, and prediction/confidence upper 
# and lower bounds.
head(poly_df)

# Subset from 2010-2020 to zoom in on more recent data.
poly_subset <- poly_df[361:481,]

# Because legends work differently in ggplot for plotting multiple variables, this is a 
# color mapper for the aesthetics of each variable for the ggplot to interpret in the legend.
cols <- c('Prediction Interval'='darkgray', 'Confidence Interval'='black', 'Point Forecasts'='#3796db', 
          'Observed Values'='green', 'Fitted Values'='black')
ggplot(poly_subset, aes(x=date)) +
  geom_line(aes(y=ppm, color='Observed Values'), na.rm=TRUE) +
  geom_line(aes(y=fits, color='Fitted Values'), linetype='dashed', na.rm=TRUE) +
  geom_ribbon(aes(ymin=p_lwr, ymax=p_upr, fill='Prediction Interval'), alpha=1, na.rm=TRUE) +
  geom_ribbon(aes(ymin=c_lwr, ymax=c_upr, fill='Confidence Interval'), alpha=0.8, na.rm=TRUE) +
  geom_line(aes(y=pred, color='Point Forecasts'), lwd=0.6, na.rm=TRUE) +
  ggtitle(expression('Forecasted CO'[2]*' Monthly Means'), '2010-2020') +
  xlab('Date') +
  ylab('PPM') +
  scale_fill_manual(name="Shades", values=cols) +
  scale_color_manual(name="Lines", values=cols)



## 2. Modeling and Forecasting Seasonality (6% each)
# (a) Construct and test (by looking at the diagnostic statistics) a model with a full set of seasonal dummies.
seasons <- lm(ppm~is_2+is_3+is_4+is_5+is_6+is_7+is_8+is_9+is_10+is_11+is_12, data=c02)
summary(seasons)
# The seasonal only model performs very badly on our time series. None of the variables are statistically significant and the 
# adjusted R squared score is -0.02, which means this model fits our data worse than a plain horizontal line. This is because
# the trend component of this time series is very important. The only statistically significant coefficient is our intercept at
# 369.4. 

ggplot(c02, aes(x=date)) +
  geom_line(aes(y=ppm, color='Observed Values')) +
  geom_line(aes(y=seasons$fitted.values, color='Fitted Values'), linetype='dashed') +
  ggtitle(expression('Global CO'[2]*' Monthly Means'), 'Seasonal Model, 1980-2018') +
  xlab('Date') +
  ylab('PPM') + 
  scale_color_manual(name="Lines", values=cols2)
# Here is a plot of the seasonal models fitted values overlayed on the observed values. We can see that the 
# cyclical nature is represented well, but without the rising trend this model underperforms significantly.
     
 
# (b) Plot the estimated seasonal factors and interpret your plot.
# Vector for ordering season coefficients correctly
vars <- c('Intercept', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12')
# Make dataframe to pass to ggplot
d <- data.frame(seasons$coef, vars)
# Factor vars column with correct level order
d$vars <- factor(d$vars, levels=c('Intercept', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'))

ggplot(d, aes(x=vars, y=seasons$coef)) + 
  geom_col(fill='orange') +
  ggtitle('Seasonal Factor Coefficients') +
  xlab('Season') +
  ylab('Coefficient')   
# Our plot shows us the large intercept at 369.4, and the other seasonal parameters with much lower values. From
# February till about June we hardly see any weight placed on the seasonal dummies. From July until October we observed 
# negative coefficients, suggesting a period of temperature decline. 


# (c) In order to improve your model, add the trend model from problem 1 to your seasonal model. 
#     We will refer to this model as the full model. For the full model, plot the respective residuals vs. 
#     fitted values and discuss your observations.
full <- lm(ppm~ts+I(ts^2)+is_2+is_3+is_4+is_5+is_6+is_7+is_8+is_9+is_10+is_11+is_12, data=c02)

ggplot(c02, aes(x=date)) +
  geom_line(aes(y=ppm, color='Observed Values')) +
  geom_line(aes(y=full$fitted.values, color='Fitted Values'), linetype='dashed') +
  ggtitle(expression('Global CO'[2]*' Monthly Means'), 'Full Model, 1980-2018') +
  xlab('Date') +
  ylab('PPM') + 
  scale_color_manual(name="Lines", values=cols2)
# Here is the plot of the full models fitted values overlayed in the observed values. Just from a visual perspective,
# we can see a large improvement when combining both trend and seasonal components.

ggplot(c02) +
  geom_point(aes(x=full$fitted.values, y=full$residuals), color='orange', alpha=0.7) +
  geom_hline(yintercept=mean(full$residuals), color='black', linetype='dashed') +
  ylim(-6,6) +
  ggtitle('Fitted Values vs Residuals', 'Full Model') +
  xlab('Fitted Values') +
  ylab('Residuals') 
# The residuals of the full model are even more densely concentrated about the horizontal zero line. Compared to the linear
# and nonlinear models, the y-axis range is considerably lower. We can see some low magnitude structure following a oscillation of
# overpredicting and underpredicting of the observed values.


ggplot(c02) +
  geom_histogram(aes(x=full$residuals), bins=40, fill='orange', color='black', alpha=1) +
  xlim(-6,6) +
  ggtitle('Histogram of Residuals', 'Full Model') +
  xlab('Value') +
  ylab('Frequency') 



# (d) Interpret the respective summary statistics including the error metrics of your full model.
summary(full)
# The full model performed the best so far. We have statistical significance on about every variable except for
# the month of June. Also note that February barely passed at the 95% significance level. As expected, the F-statistic
# tells us that some variable in the model is important in explaining CO2 levels. The adjusted R2 score is the best 
# at 99.88, which is a 1.32% increase from the linear model and a 0.59% increase from the nonlinear model. This tells us
# that the polynomial trend combined with some seasonal factors improved our model of this time series. The standard
# residual error is also at its lowest of 0.68, as compared with 2.34 and 1.65 from the earlier models.

cooks_full <- cooks.distance(full)
p5 <- ggplot(c02, aes(x=seq(length(cooks_full)), y=cooks_full)) +
  geom_line(color='black', size=0.3, alpha=0.8) +
  geom_point(color='orange', size=0.7) +
  ylim(0, 0.03) +
  ggtitle("Cook's Distance", 'Full Model') +
  xlab('Index') +
  ylab("Cook's Distance")

p6 <- ggplot(data=c02) +
  geom_qq(aes(sample=full$residuals), color='orange', alpha=0.6) +
  ggtitle('QQ Normal Plot of Residuals', 'Full Model') +
  ylab('Sample Quantiles') +
  xlab('Theoretical Quantiles')

plot_grid(p5, p6, nrow=1)
# Plotting the histogram of residuals plot of the full model reaffirms the better overall fit of the 
# full model with the lower average deviations from the mean. The Cook's Distances are the lowest of any
# of the models, and the QQ normal plot suggests that our residuals are distributed more horizontally 
# compressed inwards compared to a normal distribution.


# (e) Use the full model to forecast h-steps (at least 16) ahead. 
#     Your forecast should include the respective prediction interval.
# Make predictions for full model.
full_pred <- predict(full, newdata=forecasts, se.fit=TRUE)

# Get prediction and confidence intervals
full_c_int = as.data.frame(predict(full, forecasts, level=0.95, interval="confidence"))
full_p_int = as.data.frame(predict(full, forecasts, level=0.95, interval="prediction"))

# Append each variable to the NA padding vectors. Now they are all 481 in length.
full_df <- data.frame(
  date = c(c02$date, forecasts$date),
  ts = c(c02$ts, forecasts$ts),
  pred = c(na_457, full_pred$fit),
  p_upr = c(na_457, full_p_int$upr),
  p_lwr = c(na_457, full_p_int$lwr),
  c_upr = c(na_457, full_c_int$upr),
  c_lwr = c(na_457, full_c_int$lwr),
  ppm = c(c02$ppm, na_24),
  fits = c(full$fitted.values, na_24)
)

# Subset from 2010-2020 to zoom in on most recent data.
full_subset <- full_df[361:481, ]

ggplot(full_subset, aes(x=date)) +
  geom_line(aes(y=ppm, color='Observed Values'), na.rm=TRUE) +
  geom_line(aes(y=fits, color='Fitted Values'), linetype='dashed', na.rm=TRUE) +
  geom_ribbon(aes(ymin=p_lwr, ymax=p_upr, fill='Prediction Interval'), alpha=1, na.rm=TRUE) +
  geom_ribbon(aes(ymin=c_lwr, ymax=c_upr, fill='Confidence Interval'), alpha=0.8, na.rm=TRUE) +
  geom_line(aes(y=pred, color='Point Forecasts'), lwd=0.6, na.rm=TRUE) +
  ggtitle(expression('Forecasted CO'[2]*' Monthly Means'), '2010-2020') +
  xlab('Date') +
  ylab('PPM') +
  scale_fill_manual(name="Shades", values=cols) +
  scale_color_manual(name="Lines", values=cols)


