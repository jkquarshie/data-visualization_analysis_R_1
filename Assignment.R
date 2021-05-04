
##loading packages to be used##

library(foreign)
library(ggplot2)
library(lattice)
library(latticeExtra)

##loading data set (world bank data)##

wbdata <- read.csv(file.choose(), header = TRUE)

##visualization## #Histograms

hist(wbdata$GDP_growth, main = "GDP Growth Frequency", xlab = "GDP Growth", ylab = "Number of Countries", col = "blue", border = FALSE) ##histogram (gdp_growth)

hist(wbdata$GDP_growth, breaks = 50, main = "GDP Growth Frequency", xlab = "GDP Growth", ylab = "Number of Countries") ##adding bins for better visualization

hist(wbdata$GDP_growth, breaks = 50, main = "GDP Growth Frequency", xlab = "GDP Growth", ylab = "Number of Countries", col = "blue", border = FALSE)

##codes for kernel density plots#
densityplot <- density(wbdata$GDP_growth, na.rm = TRUE)

plot(densityplot, main = "GDP Growth Kernel Density Plot", xlab = "GDP Growth")

##coding for bar chart Visualization##

par(mar = c(12,4,4,2) + 0.1) #creating margins to prevent 'bottom', 'left', 'top', 'right' chart from cutting off
barplot(wbdata$GDP_growth, names.arg = wbdata$country, las = 2, ylab = "GDP Growth", col = "blue") #creating barcharts with labels

par(mar = c(8,4,4,2) + 0.1) #create margins to prevent from cutting off (horizontal)
barplot(wbdata$GDP_growth, names.arg = wbdata$country, horiz = TRUE, las = 2)

wbdata <- wbdata[order(wbdata$GDP_growth), ] #Ordering the data by GDP growth
barplot(wbdata$GDP_growth, names.arg = wbdata$country, las = 2, col = "blue")

#Coding for scatterplot visualization

plot(wbdata$GDP_growth, wbdata$oda, main = "GDP Growth v. ODA", xlab = "GDP Growth", ylab = "ODA (US$)")

text(wbdata$GDP_growth, wbdata$oda, labels=wbdata$country, las = 2) #adding country labels to plot

plot <- ggplot(wbdata, aes(GDP_growth,oda)) + geom_point() + geom_text(aes(label=country)) #Ggplot
plot

