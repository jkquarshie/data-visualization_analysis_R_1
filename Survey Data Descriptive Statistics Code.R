##Install Package to Load Different Types of Datasets##

install.packages("questionr") #Must be connected to the internet

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
library(stargazer)
library(MASS)
library(coefplot)
library(gmodels)
library(questionr)

###Beginning of the R Code###

##Load the Dataset##

abdata <- read.csv(file.choose(), header = TRUE)

##Preparing the Data##

#Rename variables

abdata$futureecon <- abdata$Q7 #Good to do if you modify variables

#Inspect
table(abdata$futureecon)

#Recoding variables
abdata$futureecon[abdata$futureecon==-1]<-NA
abdata$futureecon[abdata$futureecon==98]<-NA

#Country names
abdata$countryname <- abdata$COUNTRY
abdata$countryname[abdata$countryname==1]<-"Algeria"
abdata$countryname[abdata$countryname==2]<-"Benin"
abdata$countryname[abdata$countryname==3]<-"Botswana"
abdata$countryname[abdata$countryname==4]<-"Burkina Faso"
abdata$countryname[abdata$countryname==5]<-"Burundi"
abdata$countryname[abdata$countryname==6]<-"Cameroon"
abdata$countryname[abdata$countryname==7]<-"Cape Verde"
abdata$countryname[abdata$countryname==8]<-"Cote d'Ivoire"
abdata$countryname[abdata$countryname==9]<-"Egypt"
abdata$countryname[abdata$countryname==10]<-"Gabon"
abdata$countryname[abdata$countryname==11]<-"Ghana"
abdata$countryname[abdata$countryname==12]<-"Guinea"
abdata$countryname[abdata$countryname==13]<-"Kenya"
abdata$countryname[abdata$countryname==14]<-"Lesotho"
abdata$countryname[abdata$countryname==15]<-"Liberia"
abdata$countryname[abdata$countryname==16]<-"Madagascar"
abdata$countryname[abdata$countryname==17]<-"Malawi"
abdata$countryname[abdata$countryname==18]<-"Mali"
abdata$countryname[abdata$countryname==19]<-"Mauritius"
abdata$countryname[abdata$countryname==20]<-"Morocco"
abdata$countryname[abdata$countryname==21]<-"Mozambique"
abdata$countryname[abdata$countryname==22]<-"Namibia"
abdata$countryname[abdata$countryname==23]<-"Niger"
abdata$countryname[abdata$countryname==24]<-"Nigeria"
abdata$countryname[abdata$countryname==25]<-"S???o Tom??? and Principe"
abdata$countryname[abdata$countryname==26]<-"Senegal"
abdata$countryname[abdata$countryname==27]<-"Sierra Leone"
abdata$countryname[abdata$countryname==28]<-"South Africa"
abdata$countryname[abdata$countryname==29]<-"Sudan"
abdata$countryname[abdata$countryname==30]<-"Swaziland"
abdata$countryname[abdata$countryname==31]<-"Tanzania"
abdata$countryname[abdata$countryname==32]<-"Togo"
abdata$countryname[abdata$countryname==33]<-"Tunisia"
abdata$countryname[abdata$countryname==34]<-"Uganda"
abdata$countryname[abdata$countryname==35]<-"Zambia"
abdata$countryname[abdata$countryname==36]<-"Zimbabwe"

##Descriptive Statistics Table##

#Average statistics
totaltable <- wtd.table(abdata$futureecon, weights = abdata$Combinwt, na.rm = TRUE) #Weighting variable added
totaltable <- t(totaltable)
totaltableprop <- prop.table(totaltable, 1)
rownames(totaltableprop) <- c("Average")
colnames(totaltableprop) <- c("Much Worse", "Worse", "Same", "Better", "Much Better", "Don't Know")

totaltableprop #Show the table


##By country statistics
proptab <- wtd.table(abdata$countryname, abdata$futureecon, weights = abdata$Combinwt, na.rm = TRUE)
proptab <- prop.table(proptab, 1)
colnames(proptab) <- c("Much Worse", "Worse", "Same", "Better", "Much Better", "Don't Know")
proptabfin <- as.data.frame.array(proptab)
proptabfin <- cbind(Country = rownames(proptabfin), proptabfin)
rownames(proptabfin) <- 1:nrow(proptabfin)

proptabfin$`Much Worse` <- paste(round(100*proptabfin$`Much Worse`, 0), "%", sep="")
proptabfin$`Worse` <- paste(round(100*proptabfin$`Worse`, 0), "%", sep="")
proptabfin$`Same` <- paste(round(100*proptabfin$`Same`, 0), "%", sep="")
proptabfin$`Better` <- paste(round(100*proptabfin$`Better`, 0), "%", sep="")
proptabfin$`Much Better` <- paste(round(100*proptabfin$`Much Better`, 0), "%", sep="")
proptabfin$`Don't Know` <- paste(round(100*proptabfin$`Don't Know`, 0), "%", sep="")

proptabfin

#Export options:

#Data/ Excel file
write.csv(proptabfin, "FutureEcon.csv")

#LaTeX format
stargazer(proptabfin, rowname = TRUE, colnames= TRUE, summary = FALSE)


##By gender statistics
proptab <- wtd.table(abdata$gender, abdata$futureecon, weights = abdata$Combinwt, na.rm = TRUE)
proptab <- prop.table(proptab, 1)
colnames(proptab) <- c("Much Worse", "Worse", "Same", "Better", "Much Better", "Don't Know")
proptabfin <- as.data.frame.array(proptab)
proptabfin <- cbind(Country = rownames(proptabfin), proptabfin)
rownames(proptabfin) <- 1:nrow(proptabfin)

proptabfin$`Much Worse` <- paste(round(100*proptabfin$`Much Worse`, 0), "%", sep="")
proptabfin$`Worse` <- paste(round(100*proptabfin$`Worse`, 0), "%", sep="")
proptabfin$`Same` <- paste(round(100*proptabfin$`Same`, 0), "%", sep="")
proptabfin$`Better` <- paste(round(100*proptabfin$`Better`, 0), "%", sep="")
proptabfin$`Much Better` <- paste(round(100*proptabfin$`Much Better`, 0), "%", sep="")
proptabfin$`Don't Know` <- paste(round(100*proptabfin$`Don't Know`, 0), "%", sep="")

proptabfin
