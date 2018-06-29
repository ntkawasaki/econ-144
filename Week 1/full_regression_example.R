#***********************************************
# Randall R. Rojas
# Email: rrojas@econ.ucla.edu
# Date: 03/30/2015
# Comment(s): First R code example for Econ 144.
# Data File(s): 
#***********************************************
# Variable Definitions
# mpg = miles per gallon (fuel efficiency) = y (response variable)
# id = car id (indicator variable I)
# cyl = the number of cylinders (x1)
# hp = horsepower (x2)
# wt = weight in thousands of pounds (x3)
#************************************************

# Set your 'working directory' to the folder where all the data and respective codes are located.
setwd("/Users/noahkawasaki/Desktop/ECON 144/Week 1")

# Clear all variables and prior sessions
rm(list=ls(all=TRUE))

# Load Libraries
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

# Read in the data into a data file and attach names:
z=read.table('cars.dat', header=T)
attach(z)

#-------------[1] UNIVARIATE DISTRIBUTIONS---------------------
#-------------HISTOGRAM w/ DENSITIES --------------------------
quartz()
par(mfrow=c(2,2))
truehist(mpg,col='steelblue3',main="Miles per Gallon",xlab="mpg", ylab="Fraction",xlim=c(6,40),ylim=c(0,0.10))
lines(density(mpg),lwd=2)

truehist(cyl,col='steelblue3',main="Number of Cylinders",xlab="Number of Cylinders", ylab="Fraction",xlim=c(3,9))
#lines(density(cyl),lwd=2)

truehist(hp,col='steelblue3',main="Horsepower",xlab="Horsepower", ylab="Fraction",xlim=c(0,450),ylim=c(0,0.010))
lines(density(hp),lwd=2)

truehist(wt,col='steelblue3',main="Weight in Thousands of Pounds",xlab="Weight (1000lbs)", ylab="Fraction",xlim=c(0,6),ylim=c(0,0.6))
lines(density(wt),lwd=2)

#dev.print(device=postscript,"hist1.eps",width=7,height=7, horizontal=FALSE)
#dev.off()

#-------------[2] UNIVARIATE DISTRIBUTIONS---------------------
#-------------HISTOGRAM w/ DENSITIES by CYL--------------------
quartz()
par(mfrow=c(2,2))
truehist(mpg,col='gray80',main="Miles per Gallon",xlab="mpg", ylab="Fraction",xlim=c(6,40),ylim=c(0,0.24))
lines(density(mpg),lwd=2)
lines(density(mpg[cyl==4]),col="tomato3",lwd=2)
lines(density(mpg[cyl==6]),col="seagreen3",lwd=2)
lines(density(mpg[cyl==8]),col="steelblue3",lwd=2)
legend(25,0.20, c("Tot", "4-Cyl", "6-Cyl","8-Cyl"),
fill = c("black","tomato3","seagreen3","steelblue3"),cex=1,bty='n')

quartz()
truehist(cyl,col='gray80',main="Number of Cylinders",xlab="Number of Cylinders", ylab="Fraction",xlim=c(3,9))

truehist(hp,col='gray80',main="Horsepower",xlab="Horsepower", ylab="Fraction",xlim=c(0,450),ylim=c(0,0.034))
lines(density(hp),lwd=2)
lines(density(hp[cyl==4]),col="tomato3",lwd=2)
lines(density(hp[cyl==6]),col="seagreen3",lwd=2)
lines(density(hp[cyl==8]),col="steelblue3",lwd=2)
legend(250,0.032, c("Tot", "4-Cyl", "6-Cyl","8-Cyl"),
fill = c("black","tomato3","seagreen3","steelblue3"),cex=1,bty='n')

truehist(wt,col='gray80',main="Weight in Thousands of Pounds",xlab="Weight (1000lbs)", ylab="Fraction",xlim=c(0,6),ylim=c(0,1.2))
lines(density(wt),lwd=2)
lines(density(wt[cyl==4]),col="tomato3",lwd=2)
lines(density(wt[cyl==6]),col="seagreen3",lwd=2)
lines(density(wt[cyl==8]),col="steelblue3",lwd=2)
legend(4,1.1, c("Tot", "4-Cyl", "6-Cyl","8-Cyl"),
fill = c("black","tomato3","seagreen3","steelblue3"),cex=1,bty='n')

#-------------[3] BOXPLOTS by NUMBER of CYLINDERS--------------
quartz()
boxplot(mpg~cyl,main="Fuel Efficiency (Miles per Gallon) \n by Number of Cylinders ",xaxt="n",col="skyblue3",ylab="Miles per Gallon")
axis(1, 1:3,labels=c("4-Cylinder", "6-Cylinder", "8-Cylinder"))
#dev.print(device=postscript,"box_mpg.eps",width=7,height=7, horizontal=FALSE)
#dev.off()

par(mfrow=c(2,1))
boxplot(hp~cyl,main="Horsepower \n by Number of Cylinders ",xaxt="n",col="skyblue3",ylab="Horsepower")
axis(1, 1:3,labels=c("4-Cylinder", "6-Cylinder", "8-Cylinder"))

boxplot(wt~cyl,main="Weight (in thousands of pounds) \n by Number of Cylinders ",xaxt="n",col="skyblue3",ylab="Weight")
axis(1, 1:3,labels=c("4-Cylinder", "6-Cylinder", "8-Cylinder"))

#-------------[4] COPLOT MPG vs WEIGHT by NUMBER of CYLINDERS--
quartz()
coplot(mpg~wt|as.factor(cyl),overlap=0.6,pch=20,col="steelblue3")

#-------------[5] SCATTERPLOT MATRIX---------------------------
formatrix=subset(z,select=c(mpg,cyl,wt,hp))
palette(gray(0:4/4))
quartz()
scatterplotMatrix(formatrix,diagonal=c("histogram"),pch=20,cex=1.0, col=palette()[2:3], lwd=2,cex.axis=1.0,font.axis=2,main="Matrix Scatter Plot",plot.points=T,ellipse=T,smooth=T,levels=c(.5,.9),font.axis=2.0)

#----------------[6] EXPLORE MODELS-----------------------------
fcyl <- factor(cyl, levels=1:3)
levels(fcyl) <- c("4-Cyl", "6-Cyl", "8-Cyl")

### MODEL 1: FULL MODEL W/O INTERACTIONS------------------------
#y1=glm(mpg~hp+wt+cyl)
y1=lm(mpg~hp+wt+cyl)

vif(y1)
summary(y1)
anova(y1)
step(y1)
ss=regsubsets(mpg~hp+wt+cyl,method=c("exhaustive"),nbest=3,data=z)
subsets(ss,statistic="cp",legend=F,main="Mallows CP",col="steelblue4")
legend(1.5,35,bty="n",legend=c('h=Horsepower','w=Weight','c=Number of Cylinders'),col="steelblue4",cex=1.5)

#----------------REGRESSION PLOTS: MODEL 1------------------------
quartz()
par(mfcol=c(2,2))
plot(y1$fit,y1$res,col="skyblue3",pch=20,xlab="Predicted Response",ylab="Residuals",main="Residuals vs. Predicted Response",cex.axis=0.8,cex.main=0.9)
abline(h=0,lwd=2,col="red")
lines(lowess(y1$fit,y1$res),lwd=1.5) 
abline(v=6e5,col="red",lty=2,lwd=1.5)
legend(15,5.5,c(expression(y[obs]==y[pred]), "Lowess Smoother"), fill =c("red", "black"),cex=0.6)

#----------------COOKS DISTANCE PLOT------------------------------
quartz()
y1_cook=cooks.distance(y1)
plot(y1_cook,ylab="Cook's distance",type='o',main="Cook's Distance Plot",col="skyblue4", pch=20,lwd=.25)
text(7,0.28,"wt=5345lbs \n 8-Cyl \n  hp=230 \n mpg=14.7")
text(14,0.33, expression(""%->%""))

#---------------HISTOGRAM OF RESIDUALS-----------------------------
quartz()
truehist(y1$res,col="skyblue3",xlab="Residuals",ylab="Fraction",main="Histogram of Residuals",xlim=c(-6.5,6.5))
xr=rnorm(800000,mean(y1$res),sd(y1$res))
lines(density(xr),col="black",lwd=2)
lines(density(y1$res),col="red",lwd=2)
legend(0.3,0.2,c("Density","Normal Distr."),fill=c("red","black"),bty='n')

#----------------LOOK AT THE QQ PLOT------------------------------
quartz()
qqnorm(y1$res,col="skyblue4", pch=20,lwd=1,main="QQ Normal Plot")

#----------------PLOT OBSERVED vs. PREDICTED----------------------
quartz()
plot(y1$fit,mpg,pch=20,col="skyblue4",cex=1,xlab="Predicted Response",ylab="Observed Response",main="Observed vs. Predicted Response \n Full Model",cex.axis=0.8,cex.main=1.0)
lines(lowess(y1$fit,mpg),lwd=2)
abline(0,1,col="red",lwd=2,lty=2) 
text(25,18,expression(R^2==0.826))
legend(15,31, c(expression(y[obs]==y[pred]), "Lowess Smoother"), fill =c("red", "black"),cex=1,bty="y")
#----------------------------------------------------------

## vif+cp--> get rid of hp!
### MODEL 2: 

y2=lm(mpg[id!=17 & id!=20& id!=18]~wt[id!=17 & id!=20 & id!=18]+cyl[id!=17& id!=20& id!=18])
vif(y2)
summary(y2)

#----------------REGRESSION PLOTS: MODEL 2------------------------
quartz()
par(mfcol=c(2,2))
plot(y2$fit,y2$res,col="skyblue3",pch=20,xlab="Predicted Response",ylab="Residuals",main="Residuals vs. Predicted Response",cex.axis=0.8,cex.main=0.9)
abline(h=0,lwd=2,col="red")
lines(lowess(y2$fit,y2$res),lwd=1.5) 
abline(v=6e5,col="red",lty=2,lwd=1.5)
legend(16,5.5,c(expression(y[obs]==y[pred]), "Lowess Smoother"), fill =c("red", "black"),cex=0.6)

#----------------COOKS DISTANCE PLOT------------------------------
quartz()
y2_cook=cooks.distance(y2)
plot(y2_cook,ylab="Cook's distance",type='o',main="Cook's Distance Plot",col="skyblue4", pch=20,lwd=.25)
#text(7,0.28,"wt=5345lbs \n 8-Cyl \n  hp=230 \n mpg=14.7")
#text(14,0.33, expression(""%->%""))

#---------------HISTOGRAM OF RESIDUALS-----------------------------
quartz()
truehist(y2$res,col="skyblue3",xlab="Residuals",ylab="Fraction",main="Histogram of Residuals",xlim=c(-6.5,6.5),ylim=c(0,0.24))
xr=rnorm(800000,mean(y2$res),sd(y2$res))
lines(density(xr),col="black",lwd=2)
lines(density(y2$res),col="red",lwd=2)
legend(0.3,0.235,c("Density","Normal Distr."),fill=c("red","black"),bty='n')

#----------------LOOK AT THE QQ PLOT------------------------------
qqnorm(y2$res,col="skyblue4", pch=20,lwd=1,main="QQ Normal Plot")

quartz()
plot(y2$fit,mpg[id!=17 & id!=20 & id!=18],pch=20,col="skyblue4",cex=1,xlab="Predicted Response",ylab="Observed Response",main="Observed vs. Predicted Response \n Simple Model",cex.axis=0.8,cex.main=1.0)
lines(lowess(y2$fit,mpg[id!=17 & id!=20 & id!=18]),lwd=2)
abline(0,1,col="red",lwd=2,lty=2) 
text(25,18,expression(R^2==0.869))
legend(15,30, c(expression(y[obs]==y[pred]), "Lowess Smoother"), fill =c("red", "black"),cex=1,bty="y")
#dev.print(device=postscript,"yy2.eps",width=7,height=7, horizontal=FALSE)
#dev.off()
#----------------------------------------------------------


#-----------PLOT FUEL EFFICIENCY vs. WEIGHT by CYL--------------
quartz()
plot(mpg[cyl==4]~wt[cyl==4],pch=20,col="red",cex=2,xlim=c(0,6),ylim=c(10,40),ylab="Fuel Efficiency (miles per gallon)",xlab="Weight (in thousands of pounds)", main="Fuel Efficiency vs. Weight \n by Number of Cylinders")
lines(mpg[cyl==6]~wt[cyl==6],pch=20,col="blue",cex=2,type="p")
lines(mpg[cyl==8]~wt[cyl==8],pch=20,col="green",cex=2,type="p")
legend(4,35, c("4-Cylinder","6-Cylinder","8-Cylinder"), fill =c("red", "blue","green"),cex=1,bty="y")


#-----------LOOK AT CART-----------------------------------
cart1 <- rpart(mpg~wt+hp+cyl, method='poisson', data=z)
quartz()
plot(cart1,lwd=2,main="       Regression Tree \n Full Model",cex.main=1.5)
text(cart1, use.n=TRUE, cex=.6)
printcp(cart1)
print(cart1)
plotcp(cart1)
summary(cart1)



