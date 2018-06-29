# Save this file and the respective data file to a common folder. When you launch R, 
# make sure to go to the common folder where the files are before running it.
# To run the code, on the R terminal, simply type: source("simple_regression_example.R")
#
# Load Libraries

# Set your 'working directory' to the folder where all the data and respective codes are located.
setwd("/Users/noahkawasaki/Desktop/ECON 144/Week 1")

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
library(corrplot) # We'll use corrplot later on in this example too.
library(visreg) # This library will allow us to show multivariate graphs.
library(rgl)
library(knitr)
library(scatterplot3d)

# Read in the data
z=read.table('cars.dat', header=T)
attach(z)

# Compute a linear regression model for: mpg ~ weight
y1=lm(mpg~wt)

# Show regression results
summary(y1)

# Plot regression scatterplot + LS line
quartz()
plot(wt,mpg,pch=20,col="skyblue4",lwd=10)
abline(y1,lty=1, lwd=2,col ="red3")

# Plot residuals vs. fitted values
quartz()
plot(y1$fit,y1$res, pch=20,col="skyblue4",lwd=10)
abline(h=0,lwd=2,col = "red3")

#------------------------------------------------------------------

# Diagnostics
# 1. Variance Inflation Factor (Remove variables with large vif)
vif(y1)

# 2. Look for outliers via large Cooks' values.
quartz()
y1_cook=cooks.distance(y1)
plot(y1_cook,ylab="Cook's distance",type='o',main="Cook's Distance Plot",col="skyblue4", pch=20,lwd=.25)

# 3.Histogram of Residuals. Do they look normally distributed?
quartz()
y1$res
truehist(y1$res,15,col="skyblue3",xlab="Residuals",ylab="Fraction",main="Histogram of Residuals",xlim=c(-6.5,6.5))

# 4. QQ Plot
quartz()
qqnorm(y1$res,col="skyblue4", pch=20,lwd=1,main="QQ Normal Plot")

# 5. Mallows CP
ss=regsubsets(mpg~hp+wt+cyl,method=c("exhaustive"),nbest=3,data=z)
subsets(ss,statistic="cp",legend=F,main="Mallows CP",col="steelblue4")
legend(1.5,35,bty="n",legend=c('h=Horsepower','w=Weight','c=Number of Cylinders'),col="steelblue4",cex=1.5)


# 6. Plot the Observed vs Predicted Values 
quartz()
plot(y1$fit,mpg,pch=20,col="skyblue4",cex=1,xlab="Predicted Response",ylab="Observed Response",main="Observed vs. Predicted Response \n Full Model",cex.axis=0.8,cex.main=1.0)
lines(lowess(y1$fit,mpg),lwd=2)
abline(0,1,col="red",lwd=2,lty=2) 
text(25,18,expression(R^2==0.826))
legend(15,31, c(expression(y[obs]==y[pred]), "Lowess Smoother"), fill =c("red", "black"),cex=1,bty="y")

#7 Correlation Plot
newdatacor = cor(z)
corrplot(newdatacor, method = "number")

#8 More detailed analysis
attach(mtcars)
plot(wt, mpg, main="Scatterplot Example", xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
scatterplot(mpg ~ wt | cyl, data=mtcars,xlab="Weight of Car", ylab="Miles Per Gallon",main="Enhanced Scatter Plot")

scatterplot.matrix(~mpg+disp+drat+wt|cyl, data=mtcars,main="Three Cylinder Options")


# 3D Scatterplot
library(scatterplot3d)
# 3D Scatterplot with Coloring and Vertical Drop Lines
scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE, type="h", main="3D Scatterplot")
scatter3d(wt, disp, mpg)

library(ggplot2)
ggplot(mtcars, aes(y=wt, x=mpg, colour=factor(cyl)))+ stat_smooth(method=lm) + 
  geom_point()

c <- ggplot(mtcars, aes(y=wt, x=mpg, colour=factor(cyl)))
c + stat_smooth(method=lm) + geom_point()

c + stat_smooth(method=lm, fullrange=TRUE, alpha = 0.1) + geom_point()
