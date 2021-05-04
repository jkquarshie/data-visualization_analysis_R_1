
##Loading packages to be used
library(foreign)
library(ggplot2)
library(lattice)
library(latticeExtra)
library(pastecs)
library(psych)
library(ggpubr)
library(imputeTS)
library(zoo)
library(Amelia)

#R Codes
##Load the Dataset##
wbdata <- read.csv(file.choose(), header = TRUE)

#creating scatter plots for GDP per capita and HDI
plot(wbdata$GDP_percapita, wbdata$hdi, main = "GDP per capita V. HDI", xlab = "GDP Per Capita", ylab = "Human Development Index")

#viewing scatter plot with bestfit line
ggscatter(wbdata, x = "GDP_percapita", y = "hdi", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "GDP Per Capita", ylab = "Human Development Index")

#creating plots for GDP Growth and Oil Rents
plot(wbdata$GDP_growth, wbdata$oil_rents, main = "GDP Growth V. Oil Rents", xlab = "GDP Growth", ylab = "Oil Rents (% of GDP)")

#viewing plot with bestfit line
ggscatter(wbdata, x = "GDP_growth", y = "oil_rents", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "GDP Growth", ylab = "Oil Rents (% of GDP)")