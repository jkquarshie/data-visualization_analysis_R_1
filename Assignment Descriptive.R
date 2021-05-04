
##Installing new packages##

install.packages("pastecs") 
install.packages("psych") 

#loading needed packages
library(foreign)
library(ggplot2)
library(lattice)
library(latticeExtra)
library(pastecs)
library(psych)

#Beginning of R codes#

#loading dataset for assignment#

wbdata <- read.csv(file.choose(), header = TRUE)

###Descriptive Statistics### GDP Growth

##Using R Functions##

mean(wbdata$GDP_growth, na.rm = TRUE) #Mean

var(wbdata$GDP_growth, na.rm = TRUE) #Variance

sd(wbdata$GDP_growth, na.rm = TRUE) #Standard deviation

min(wbdata$GDP_growth, na.rm = TRUE) #Minimum

max(wbdata$GDP_growth, na.rm = TRUE) #Maximum

range(wbdata$GDP_growth, na.rm = TRUE) #Range

median(wbdata$GDP_growth, na.rm = TRUE) #Median

summary(wbdata$GDP_growth, na.rm = TRUE) #Mean, median, quantiles, range together

hist(wbdata$GDP_growth, breaks = 70)

##Pastecs Package##

stat.desc(wbdata$GDP_growth)

stats_table1 <- cbind(wbdata$GDP_growth) #Table of variables for descriptive stats

stat.desc(stats_table1)


##Psych Descriptive Statistics + Skew and Kurtosis##

describe(wbdata$GDP_growth) #One variable

describe(wbdata) #Whole dataset


##Education

###Descriptive Statistics###

##Using R Functions##

mean(wbdata$education_expenditure, na.rm = TRUE) #Mean

var(wbdata$education_expenditure, na.rm = TRUE) #Variance

sd(wbdata$education_expenditure, na.rm = TRUE) #Standard deviation

min(wbdata$education_expenditure, na.rm = TRUE) #Minimum

max(wbdata$education_expenditure, na.rm = TRUE) #Maximum

range(wbdata$education_expenditure, na.rm = TRUE) #Range

median(wbdata$education_expenditure, na.rm = TRUE) #Median

summary(wbdata$education_expenditure, na.rm = TRUE) #Mean, median, quantiles, range together

hist(wbdata$education_expenditure, breaks = 60)

##Pastecs Package##

stat.desc(wbdata$education_expenditure)

stats_table1 <- cbind(wbdata$education_expenditure) #Table of variables for descriptive stats

stat.desc(stats_table1)


##Psych Descriptive Statistics + Skew and Kurtosis##

describe(wbdata$education_expenditure) #One variable

describe(wbdata) #Whole dataset


#proportion of population female#

###Descriptive Statistics###

##Using R Functions##

mean(wbdata$proportion_female, na.rm = TRUE) #Mean

var(wbdata$proportion_female, na.rm = TRUE) #Variance

sd(wbdata$proportion_female, na.rm = TRUE) #Standard deviation

min(wbdata$proportion_female, na.rm = TRUE) #Minimum

max(wbdata$proportion_female, na.rm = TRUE) #Maximum

range(wbdata$proportion_female, na.rm = TRUE) #Range

median(wbdata$proportion_female, na.rm = TRUE) #Median

summary(wbdata$proportion_female, na.rm = TRUE) #Mean, median, quantiles, range together

hist(wbdata$proportion_female, breaks = 70)

##Pastecs Package##

stat.desc(wbdata$proportion_female)

stats_table1 <- cbind(wbdata$proportion_female) #Table of variables for descriptive stats

stat.desc(stats_table1)


##Psych Descriptive Statistics + Skew and Kurtosis##

describe(wbdata$proportion_female) #One variable

describe(wbdata) #Whole dataset

