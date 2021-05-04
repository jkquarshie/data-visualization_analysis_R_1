
##Installing new packages
install.packages("ggpubr")
install.packages("imputeTS")
install.packages("zoo")
install.packages("Amelia")

##Load Necessary Packages##
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

##Load the Dataset##
wbdata <- read.csv(file.choose(), header = TRUE)

##Interpolation
#Zoo Package #Linear
wbdata$GDP_percapitainterpolation <- na.approx(wbdata$GDP_percapita)

##Creating a Mean Index of GDP growth and Oil rents##
names(wbdata) #code to view and select variable column numbers

wbdata$economicgrowthindex <- rowMeans(wbdata [, c(3,5)], na.rm = TRUE) #Take means/create index for GDP growth and oil rents

##Correlations and Correlation Tests for GDP growth and GDP per capita
cor(wbdata$GDP_growth, wbdata$GDP_percapita, method = "pearson", use = "complete.obs") #Pearson correlation coefficient

cor.test(wbdata$GDP_growth, wbdata$GDP_percapita, method = "pearson", use = "complete.obs") #Pearson correlation test

##Correlations and Correlation Tests for ARC and HIV
cor(wbdata$antiretroviral_coverage, wbdata$newly_HIV, method = "pearson", use = "complete.obs") #Pearson correlation coefficient

cor.test(wbdata$antiretroviral_coverage, wbdata$newly_HIV, method = "pearson", use = "complete.obs") #Pearson correlation test

##Correlations and Correlation Tests for GDP per capita and oil_rents
cor(wbdata$GDP_percapita, wbdata$oil_rents, method = "pearson", use = "complete.obs") #Pearson correlation coefficient

cor.test(wbdata$GDP_percapita, wbdata$oil_rents, method = "pearson", use = "complete.obs") #Pearson correlation test
