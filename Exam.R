#load packages
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

wbdata <- read.csv(file.choose(), header = TRUE) #LOADING DATASET

#Identifying relationships ##Scatterplot
#plot ONE
plot(wbdata$manufacturing_valueadded, wbdata$GDP_percapita, 
     main = "Manufacturing (% of GDP) V. GDP Per Capita", 
     xlab = "Manufacturing (% of GDP)", ylab = "GDP Per Capita")

text(wbdata$manufacturing_valueadded, wbdata$GDP_percapita, labels = wbdata$country)

#regress to test relationship between manufacturing & GDP per capita
regress1 <- lm(wbdata$manufacturing_valueadded ~ wbdata$GDP_percapita)
summary(regress1)

stargazer(regress1) #for latex

#plot TWO
plot(wbdata$unemployment, wbdata$gdp_growth, main = "Unemployment V. GDP Growth",
     xlab = "Unemployment", ylab = "GDP Growth")
text(wbdata$unemployment, wbdata$gdp_growth, labels = wbdata$country)

#regress to test the relationship between ##unemployment rate and #GDP Growth
regress2 <- lm(wbdata$unemployment ~ wbdata$gdp_growth)
summary(regress2)

stargazer(regress2) #for latex

#Descriptive statistics
describe(wbdata)

##descriptive for FDI
mean(wbdata$fdi_netflows, na.rm = TRUE) #Mean
var(wbdata$fdi_netflows, na.rm = TRUE) #Variance
sd(wbdata$fdi_netflows, na.rm = TRUE) #Standard deviation
min(wbdata$fdi_netflows, na.rm = TRUE) #Minimum
max(wbdata$fdi_netflows, na.rm = TRUE) #Maximum
range(wbdata$fdi_netflows, na.rm = TRUE) #Range
median(wbdata$fdi_netflows, na.rm = TRUE) #Median
describe(wbdata$fdi_netflows) #skew and kurtosis

##descriptive for life expectancy
mean(wbdata$life_expentancy_birth, na.rm = TRUE) #Mean
var(wbdata$life_expentancy_birth, na.rm = TRUE) #Variance
sd(wbdata$life_expentancy_birth, na.rm = TRUE) #Standard deviation
min(wbdata$life_expentancy_birth, na.rm = TRUE) #Minimum
max(wbdata$life_expentancy_birth, na.rm = TRUE) #Maximum
range(wbdata$life_expentancy_birth, na.rm = TRUE) #Range
median(wbdata$life_expentancy_birth, na.rm = TRUE) #Median
describe(wbdata$life_expentancy_birth) #skew and kurtosis

##descriptive stats for employment in agric
var(wbdata$employment_agric, na.rm = TRUE) #variance
describe(wbdata$employment_agric) ## mean, sd, min, max, range, median, skew & kurtosis

##descriptive stats for employment in industry
var(wbdata$employment_industry, na.rm = TRUE) #variance
describe(wbdata$employment_industry) ##mean, sd, min, max, range, median, skew & kurtosis

##histo
hist(wbdata$life_expentancy_birth, breaks = 10)
hist(wbdata$life_expentancy_birth, breaks = 10, main = "Life Expectancy", 
     xlab = "Life Expectancy", ylab = "Frequency", col = "blue", border = FALSE)

##kernel density
densityplot(wbdata$life_expentancy_birth, na.rm = TRUE,
            main = "Life Expectancy", xlab = "Life Expectancy (Births)")

##boxplot
boxplot(wbdata$employment_agric, wbdata$employment_industry, ylab = "Participation (Employment)",
        xlab = "Employment by Sector", names = c("Agric Sector", "Industrial Sector"))

##model explaining access to electricity
mod1 <- lm(wbdata$access_electricity ~ wbdata$unemployment + wbdata$GDP_percapita + 
             wbdata$gdp_growth + wbdata$subsaharan)
summary(mod1) #summary of model
stargazer(mod1) #for latex


mod2 <- lm(wbdata$access_electricity ~ wbdata$employment_agric + wbdata$employment_industry + 
             wbdata$employment_services +wbdata$employment_pop_ratio +wbdata$population)
summary(mod2)

##logistic regression
mod3 <- glm(wbdata$partial_freedom ~ wbdata$employment_industry +wbdata$GDP_percapita + 
              wbdata$employment_services, family = "binomial")
summary(mod3)
stargazer(mod3) #for latex

##life expectancy model
mod4 <- lm(wbdata$life_expentancy_birth ~ wbdata$GDP_percapita + wbdata$subsaharan)
mod5 <- lm(wbdata$life_expentancy_birth ~ wbdata$GDP_percapita + wbdata$subsaharan + 
             wbdata$access_electricity)
mod6 <- lm(wbdata$life_expentancy_birth ~ wbdata$GDP_percapita + wbdata$subsaharan + 
             wbdata$access_electricity + wbdata$island)
summary(mod4)
stargazer(mod4, mod6) #for latex
