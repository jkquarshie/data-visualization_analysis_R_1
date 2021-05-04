##loading packages to be used##

library(foreign)
library(ggplot2)
library(lattice)
library(latticeExtra)

##loading data set (world bank data)##

wbdataclass <- read.csv(file.choose(), header = TRUE)

#code for viewing data#

View(wbdataclass)

##visualization## #Histigrams

hist(wbdataclass$oil_rents) ##histogram 

hist(wbdataclass$oil_rents, breaks = 30) ##adding bins for better visualization

##adding lables to figures in histogram##

hist(wbdataclass$oil_rents, main = "Oil Rent Frequency", xlab = "Oil Rents (% of GDP", ylab = "Frequency (Number of Countries)")

##adding colors to histogram##
hist(wbdataclass$oil_rents, main = "Oil Rent Frequency", xlab = "Oil Rents (% of GDP", ylab = "Frequency (Number of Countries", col = "blue", border = FALSE)

##codes for kernel density plots#

densityplot <- density(wbdataclass$oil_rents, na.rm = TRUE)
plot(densityplot, main = "Oil Rents Kernel Density Plot", xlab = "Oil Rents (% of GDP")

#codes for barcharts#

par(mar = c(12,4,4,2) + 0.1) #set margins to prevent cutting off
par(mar = c(12,4,4,2) + 0.1)
barplot(wbdataclass$oil_rents, names.arg = wbdataclass$country, las = 2, ylab = "Oil Rent (% of GDP)") #adding shape labels

par(mar = c(2,10,2,2) + 0.1) #Set plot margins to prevent from cutting off

barplot(wbdataclass$oil_rents, names.arg = wbdataclass$country, horiz = TRUE, las = 2, xlab =  "Oil Rent (% of GDP)")

#ordering the data on barchart
wbdataclass <- wbdataclass[order(wbdataclass$oil_rents),] #ordering by oilrent#
barplot(wbdataclass$oil_rents, names.arg = wbdataclass$country, horiz = TRUE, las = 2)

#order by country
wbdataclass <- wbdataclass[order(wbdataclass$country),] #ordering by country#
barplot(wbdataclass$oil_rents, names.arg = wbdataclass$country, horiz = TRUE, las = 2)

#ploting scatter diagrams#

plot(wbdataclass$oil_rents, wbdataclass$oda, main = "Oil Rents Vs ODA", xlab = "Oil Rents (% of GDP)", ylab = "ODA (US$)")
text(wbdataclass$oil_rents, wbdataclass$oda, labels=wbdataclass$country) #Add marker labels


