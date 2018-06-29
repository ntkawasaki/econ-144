#### Homework 1
library(tidyverse)
library(DAAG)
library(data.table)
library(openxlsx)
library(leaps)
library(reshape2)
library(car)
library(corrplot)
library(gtable)
require(cowplot)
library('readxl')
library(zoo)


## PROBLEM 1
# Load data 
df <- nsw74psid1
attach(df)

?nsw74psid1
head(df)
summary(df)

# (a) Plot a histogram of each variable and discuss its properties.
# Binary variables

b1 <- ggplot(data=df) +
  geom_bar(mapping=aes(x=black)) +
  ggtitle('Black') 

b2 <- ggplot(data=df) +
  geom_bar(mapping=aes(x=hisp)) +
  ggtitle('Hispanic')

b3 <- ggplot(data=df) +
  geom_bar(mapping=aes(x=marr)) +
  ggtitle('Married')

b4 <- ggplot(data=df) +
  geom_bar(mapping=aes(x=nodeg)) +
  ggtitle('No Degree')

plot_grid(b1, b2, b3, b4, nrow=2)

# Continuous variables
# age
c1 <- ggplot(data=df) +
  geom_histogram(mapping=aes(x=age), binwidth=1) +
  ggtitle("Age Distribution")
# educ
c2 <- ggplot(data=df) +
  geom_histogram(mapping=aes(x=educ), binwidth=1) +
  ggtitle("Years of Education Distribution")

plot_grid(c1, c2, nrow=1)

# re74
c3 <- ggplot(data=df) +
  geom_histogram(mapping=aes(x=re74), fill='red', color='black') +
  xlim(0, 150000) +
  ggtitle('Real Earnings 1974') +
  xlab(NULL)
# re75
c4 <- ggplot(data=df) +
  geom_histogram(mapping=aes(x=re75), fill='blue', color='black') +
  xlim(0, 150000) +
  ggtitle('Real Earnings 1975') +
  xlab(NULL)
# re78
c5 <- ggplot(data=df) +
  geom_histogram(mapping=aes(x=re78), fill='green', color='black', bins=80) +
  xlim(0, 150000) +
  ggtitle('Real Earnings 1978') +
  xlab(NULL)
c5

plot_grid(c3, c4, c5, ncol=1)

# (b) Estimate your full regression model yˆ = β0 + β1x1 + β2x2 + β3x3 + and show/discuss your results.
linreg <- lm(re78~trt+(age+educ+re74+re75)+(black+hisp+marr+nodeg))
summary(linreg)

# (c) Compute the Mallows CP statistic for all the plausible models and choose only one model. 
# Discuss why you chose this model. For the next questions, only use your selected model from this part.
subsets <- regsubsets(re78~trt+(age+educ+re74+re75)+(black+hisp+marr+nodeg), 
                      method=c("exhaustive"), nbest=3, data=df)

subsets(subsets, statistic="cp", legend=F, main="Mallows CP", col="steelblue4", 
        min.size=1, max.size=4, cex.subsets=1)

lr <- lm(re78~age+educ+re74+re75)
summary(lr)

# (d) Plot the residuals vs. the fitted values.
ggplot(data=df, mapping=aes(x=lr$fitted.values, y=lr$residuals)) +
  geom_point(color='skyblue4', alpha=0.7) +
  geom_abline(intercept=0, slope=0, color='red') +
  ggtitle('Residuals vs Fitted Values') +
  xlab('Fitted Values') +
  ylab('Residuals')
  
# (e) Plot and discuss the VIF plot.
# General rule is > 4 is concerning
vars <- c('age', 'educ', 're74', 're75')
vif_lr <- vif(lr)
d <- data.frame(vars, vif_lr)

ggplot(d, aes(vars, vif_lr)) + 
  geom_point() +
  ylim(0, 4) +
  geom_abline(intercept=4, slope=0, color='black', alpha=0.8, linetype='dashed') +
  ggtitle('Variance Inflation Factors of Regressors') +
  xlab('Regressors') +
  ylab('Variance Inflation Factor')

anova_lr <- anova(lr)
anova_lr

# (f) Plot and discuss the correlation graph (use the corrplot library)
df_2 <- df[vars]
corr <- melt(cor(df_2))
corrplot(corr, method="number")

ggplot(data=corr, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(label=round(value, 2))) +
  ggtitle('Correlation of Regressors') +
  xlab('Regressor 1') +
  ylab('Regressor 2')

# (g) Plot the Cook’s Distance values. Are there any outliers? If so, discuss what you would do with them.
lr_cooks <- cooks.distance(lr)
plot(lr_cooks, ylab="Cook's distance", type='o', main="Cook's Distance", 
     col="skyblue4", pch=20, lwd=.25)

# (h) Plot a histogram of the residuals and discuss the results.
# truehist(lr$residuals, nbins=30, col="skyblue3", xlab="Residuals", ylab="Fraction",
#          main="Histogram of Residuals")

ggplot(data=df) +
  geom_histogram(mapping=aes(x=lr$residuals), bins=40, color='black', fill='red') +
  ggtitle('Residuals Distribution') +
  xlab('Residuals') +
  ylab('Count')

# (i) Plot the QQ Normal Plot and discuss the results.
# qqnorm(lr$res, col="skyblue4", pch=20, lwd=1, main="QQ Normal Plot")
ggplot(data=df) +
  geom_qq(aes(sample=lr$residuals), alpha=0.6) +
  ggtitle('QQ Normal Plot of Residuals') +
  ylab('Sample Quantiles') +
  xlab('Theoretical Quantiles')

# (j) Plot the observed vs. predicted values, overlay a Lowess smoother, and discuss the results.
# plot(lr$fit, re78, pch=20, col="skyblue4", cex=1, xlab="Predicted Response", ylab="Observed Response", 
#      main="Observed vs. Predicted Response", cex.axis=0.8, cex.main=1.0)
# lines(lowess(lr$fit, re78), lwd=2)
# abline(0, 1, col="red", lwd=2, lty=2) 

ggplot(data=df_2, mapping=aes(lr$fit, re78)) +
  geom_point(alpha=0.7) +
  geom_smooth(method='loess') +
  geom_abline(color='red') +
  ggtitle('Observed vs Predicted Response') +
  xlab('Predicted Response') +
  ylab('Observed Response') 

         
     
### TEXTBOOK PROBLEMS
setwd("/Users/noahkawasaki/Desktop/ECON 144/Textbook_data")

## PROBLEM 2.2
growth_rates <- read.xlsx("Chapter2_exercises_data.xlsx", sheet=1)
head(growth_rates)
summary(growth_rates)

ggplot(data=growth_rates) +
  geom_histogram(mapping=aes(x=GRGDP))

ggplot(data=growth_rates) +
  geom_histogram(mapping=aes(x=RETURN))



## PROBLEM 3.1
fred <- read.xlsx("Chapter3_exercises_data.xlsx", sheet=1, detectDates=TRUE)[,1:3]
# (a) 
fred['rpce_lag'] <- shift(fred$rpce, n=1L, type=c("lag"))
fred['rpce_growth'] <- log(fred$rpce) - log(fred$rpce_lag)

fred['rdpi_lag'] <- shift(fred$rdpi, n=1L, type=c("lag"))
fred['rdpi_growth'] <- log(fred$rdpi) - log(fred$rdpi_lag)

# Re-order
fred <- fred[, c('date', 'rpce', 'rpce_lag', 'rpce_growth', 'rdpi', 'rdpi_lag', 'rdpi_growth')]
head(fred)

p1 <- ggplot(data=fred) +
  geom_line(mapping=aes(date, y=rpce_growth), color='red', na.rm=TRUE) +
  geom_abline(intercept=0, slope=0) +
  ggtitle('Expenditure Growth Over Time') +
  xlab('Year') +
  ylab('Growth')

p2 <- ggplot(data=fred) +
  geom_line(mapping=aes(date, y=rdpi_growth), color='green', na.rm=TRUE) +  
  geom_abline(intercept=0, slope=0) +
  ggtitle('Income Growth Over Time') +
  xlab('Year') +
  ylab('Growth')

plot_grid(p1, p2, ncol=1)

# (b)
lr_growth <- lm(rpce_growth~rdpi_growth, data=fred)
summary(lr_growth)

# (c)
lr_growth_lag <- lm(rpce_growth~rdpi_growth+lag(rdpi_growth), data=fred)
summary(lr_growth_lag)



## PROBLEM 3.3
us_real_gdp <- read.xlsx("Chapter3_exercises_data.xlsx", sheet=3, detectDates=TRUE)[,1:2]
exchange_rate <- read.xlsx("Chapter3_exercises_data.xlsx", sheet=4, detectDates=TRUE)[,1:2]
maturity_yield <- read.xlsx("Chapter3_exercises_data.xlsx", sheet=5, detectDates=TRUE)[,1:2]
unemployment <- read.xlsx("Chapter3_exercises_data.xlsx", sheet=6, detectDates=TRUE)[,1:2]

ggplot(data=us_real_gdp, mapping=aes(date, rgdp)) +
  geom_line() +
  ggtitle('Title') +
  xlab('Label') +
  ylab('Label')

ggplot(data=exchange_rate, mapping=aes(DATE, jpy_usd)) +
  geom_line() +
  ggtitle('Title') +
  xlab('Label') +
  ylab('Label')

ggplot(data=maturity_yield, mapping=aes(DATE, CMRate10Yr)) +
  geom_line() +
  ggtitle('Title') +
  xlab('Label') +
  ylab('Label')

ggplot(data=unemployment, mapping=aes(DATE, unemrate)) +
  geom_line() +
  ggtitle('Title') +
  xlab('Label') +
  ylab('Label')


## PROBLEM 4.3
annual <- read_excel("Chapter4_exercises_data.xls", sheet=1)
setnames(annual, c('year', 'price', 'interest'))

quarterly <- read_excel("Chapter4_exercises_data.xls", sheet=2)
setnames(quarterly, c('quarter', 'price', 'interest'))

# Computations
annual['price_lag'] <- shift(annual$price, n=1L, type=c("lag"))
annual['price_growth'] <- log(annual$price) - log(annual$price_lag)
annual['interest_lag'] <- shift(annual$interest, n=1L, type=c("lag"))
annual['interest_growth'] <- log(annual$interest) - log(annual$interest_lag)
# Re-order
annual <- annual[, c('year', 'price', 'price_lag', 'price_growth', 'interest', 'interest_lag', 'interest_growth')]
head(annual)

# Computations
quarterly['price_lag'] <- shift(quarterly$price, n=1L, type=c("lag"))
quarterly['price_growth'] <- log(quarterly$price) - log(quarterly$price_lag)
quarterly['interest_lag'] <- shift(quarterly$interest, n=1L, type=c("lag"))
quarterly['interest_growth'] <- log(quarterly$interest) - log(quarterly$interest_lag)
# Re-order
quarterly <- quarterly[, c('quarter', 'price', 'price_lag', 'price_growth', 'interest', 'interest_lag', 'interest_growth')]
head(quarterly)

# Plot prices and interests
ggplot(data=annual) +
  geom_smooth(mapping=aes(x=year, y=price)) +
  geom_smooth(mapping=aes(x=year, y=interest))

ggplot(data=annual) +
  geom_smooth(mapping=aes(x=year, y=price)) +
  geom_smooth(mapping=aes(x=year, y=interest))
  



