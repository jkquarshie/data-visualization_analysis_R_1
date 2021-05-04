head(cars)

#create a scatterplot
scatter.smooth(x=cars$speed, y=cars$dist, main = "Dist ~ Speed", ylab = "Distance", xlab = "Speed")

par(mfrow=c(1,2))
boxplot(cars$speed, main= "Speed", sub=paste("Outliers:", boxplot.stats(cars$speed)$out))
boxplot(cars$dist, main= "Distance", sub=paste("Outliers:", boxplot.stats(cars$dist)$out))

library(e1071) #package for skewness

library(ggplot2)
library(ggpubr)
library(foreign)
library(lattice)
library(latticeExtra)
library(graphics)

par(mfrow=c(1,2)) #divide graph area into two rows

densityplot(cars$speed, main= "Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))

polygon(density(cars$speed), col="green")

densityplot(cars$dist, main= "Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))
