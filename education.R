###Descriptive Statistics###

##Using R Functions##

mean(wbdata$, na.rm = TRUE) #Mean

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

