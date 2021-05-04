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

###Beginning of the R Code###

##Load the Dataset##

wbdata <- read.csv(file.choose(), header = TRUE)
agdata <- read.csv(file.choose(), header = TRUE)

##Creating a Mean Index of Factors##

names(wbdata) #View and select variable column numbers
wbdata$economicindex <- rowMeans(wbdata [, c(3:4,6)], na.rm = TRUE) #Take means across rows

##Export CSV Data##

##Creating a Mean Index of Factors##

#Pre-Determined Directory
setwd(#addpath here#)
write.csv(wbdata, file = "New Data.csv", row.names = FALSE)

#Path-Based
write.csv(wbdata, 'C:\\ResearchImages\\New Data.csv', row.names = FALSE)

##Linear Regression##

mod1 <- lm(wbdata$GDP_growth ~ wbdata$oil_rents)
summary(mod1)