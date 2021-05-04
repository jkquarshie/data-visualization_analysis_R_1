##Install new packages
install.packages("MASS")
install.packages("coefplot")

##Load Necessary Packages
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
library(stargazer)
library(MASS)
library(coefplot)

###Beginning of the R Code###

##Load the Dataset##

wbdata <- read.csv(file.choose(), header = TRUE)

##Visual Linear Regression Results
mod <- lm(hdi ~ oil_rents + subsaharan + GDP_percapita + education_expenditure, data = wbdata)

summary(mod)

coefplot(mod, intercept=FALSE, sort=c("natural"), decreasing=TRUE, title=NULL
         , newNames = c(oil_rents = "Oil Rents", subsaharan = "Sub-Saharan Africa", GDP_percapita = "GDP Per Capita", education_expenditure = "Education Expenditure")
         , predictors = c("oil_rents", "subsaharan", "GDP_percapita", "education_expenditure")
         , pointSize=4, color="blue", lwdInner=1, lwdOuter=1, zeroColor="red")


##Logistics Regression##
mod1 <- glm(wbdata$subsaharan ~ wbdata$oil_rents + wbdata$GDP_percapita + wbdata$education_expenditure, family = "binomial")
summary(mod1)

stargazer(mod1) #export to latex

