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

afrodata <- read.csv(file.choose(), header = TRUE)

##Preparing/cleaning Data##

#Rename variables/creating new variables in data

afrodata$accesstomedicare <- afrodata$Q8C #Good to do if you modify variables
colnames(afrodata)[colnames(afrodata)=="Q8C"] <- "accesstomedicare" #Simply replacing the name

afrodata$gender <- afrodata$Q101 #Good to do if you modify variables
colnames(afrodata)[colnames(afrodata)=="Q101"] <- "gender" #Simply replacing the name

table(afrodata$accesstomedicare) #check

#Recoding variables/cleaning data
afrodata$accesstomedicare[afrodata$accesstomedicare == -1] <- NA
afrodata$accesstomedicare[afrodata$accesstomedicare == 98] <- NA
afrodata$accesstomedicare[afrodata$accesstomedicare == 9] <- NA #without creatingn new variable


#check results
table(afrodata$gender, afrodata$accesstomedicare)
table(afrodata$countryname, afrodata$accesstomedicare)

##relabelling country# 
afrodata$countryname <- afrodata$COUNTRY
afrodata$countryname[afrodata$countryname==1]<-"Algeria"
afrodata$countryname[afrodata$countryname==2]<-"Benin"
afrodata$countryname[afrodata$countryname==3]<-"Botswana"
afrodata$countryname[afrodata$countryname==4]<-"Burkina Faso"
afrodata$countryname[afrodata$countryname==5]<-"Burundi"
afrodata$countryname[afrodata$countryname==6]<-"Cameroon"
afrodata$countryname[afrodata$countryname==7]<-"Cape Verde"
afrodata$countryname[afrodata$countryname==8]<-"Cote d'Ivoire"
afrodata$countryname[afrodata$countryname==9]<-"Egypt"
afrodata$countryname[afrodata$countryname==10]<-"Gabon"
afrodata$countryname[afrodata$countryname==11]<-"Ghana"
afrodata$countryname[afrodata$countryname==12]<-"Guinea"
afrodata$countryname[afrodata$countryname==13]<-"Kenya"
afrodata$countryname[afrodata$countryname==14]<-"Lesotho"
afrodata$countryname[afrodata$countryname==15]<-"Liberia"
afrodata$countryname[afrodata$countryname==16]<-"Madagascar"
afrodata$countryname[afrodata$countryname==17]<-"Malawi"
afrodata$countryname[afrodata$countryname==18]<-"Mali"
afrodata$countryname[afrodata$countryname==19]<-"Mauritius"
afrodata$countryname[afrodata$countryname==20]<-"Morocco"
afrodata$countryname[afrodata$countryname==21]<-"Mozambique"
afrodata$countryname[afrodata$countryname==22]<-"Namibia"
afrodata$countryname[afrodata$countryname==23]<-"Niger"
afrodata$countryname[afrodata$countryname==24]<-"Nigeria"
afrodata$countryname[afrodata$countryname==25]<-"S???o Tom??? and Principe"
afrodata$countryname[afrodata$countryname==26]<-"Senegal"
afrodata$countryname[afrodata$countryname==27]<-"Sierra Leone"
afrodata$countryname[afrodata$countryname==28]<-"South Africa"
afrodata$countryname[afrodata$countryname==29]<-"Sudan"
afrodata$countryname[afrodata$countryname==30]<-"Swaziland"
afrodata$countryname[afrodata$countryname==31]<-"Tanzania"
afrodata$countryname[afrodata$countryname==32]<-"Togo"
afrodata$countryname[afrodata$countryname==33]<-"Tunisia"
afrodata$countryname[afrodata$countryname==34]<-"Uganda"
afrodata$countryname[afrodata$countryname==35]<-"Zambia"
afrodata$countryname[afrodata$countryname==36]<-"Zimbabwe"


##Descriptive Statistics Table##

#Average statistics
totaltable <- wtd.table(afrodata$accesstomedicare, 
                        weights = afrodata$Combinwt, 
                        na.rm = TRUE) #Weighting variable added
totaltable <- t(totaltable)
totaltableprop <- prop.table(totaltable, 1)
rownames(totaltableprop) <- c("Average")
colnames(totaltableprop) <- c("Never", "Once or Twice", 
                              "Several Times", "Many Times", "Always")

totaltableprop #Show the table

##By country statistics
proptab <- wtd.table(afrodata$countryname, afrodata$accesstomedicare, 
                     weights = afrodata$Combinwt, na.rm = TRUE)
proptab <- prop.table(proptab, 1)
colnames(proptab) <- c("Never", "Once or Twice", "Several Times", "Many Times", "Always")
proptabfin <- as.data.frame.array(proptab)
proptabfin <- cbind(Country = rownames(proptabfin), proptabfin)
rownames(proptabfin) <- 1:nrow(proptabfin)

proptabfin$`Never` <- paste(round(100*proptabfin$`Never`, 0), "%", sep="")
proptabfin$`Once or Twice` <- paste(round(100*proptabfin$`Once or Twice`, 0), "%", sep="")
proptabfin$`Several Times` <- paste(round(100*proptabfin$`Several Times`, 0), "%", sep="")
proptabfin$`Many Times` <- paste(round(100*proptabfin$`Many Times`, 0), "%", sep="")
proptabfin$`Always` <- paste(round(100*proptabfin$`Always`, 0), "%", sep="")

proptabfin

#Export options: #Data/ Excel file
write.csv(proptabfin, "accesstomedicare.csv")

#LaTeX format
stargazer(proptabfin, rowname = TRUE, colnames= TRUE, summary = FALSE)

##By gender statistics
proptab <- wtd.table(afrodata$gender, afrodata$accesstomedicare, 
                     weights = afrodata$Combinwt, na.rm = TRUE)
proptab <- prop.table(proptab, 1)
colnames(proptab) <- c("Never", "Once or Twice", "Several Times", "Many Times", "Always")
proptabfin <- as.data.frame.array(proptab)
proptabfin <- cbind(Gender = rownames(proptabfin), proptabfin)
rownames(proptabfin) <- 1:nrow(proptabfin)

proptabfin$`Never` <- paste(round(100*proptabfin$`Never`, 0), "%", sep="")
proptabfin$`Once or Twice` <- paste(round(100*proptabfin$`Once or Twice`, 0), "%", sep="")
proptabfin$`Several Times` <- paste(round(100*proptabfin$`Several Times`, 0), "%", sep="")
proptabfin$`Many Times` <- paste(round(100*proptabfin$`Many Times`, 0), "%", sep="")
proptabfin$`Always` <- paste(round(100*proptabfin$`Always`, 0), "%", sep="")

proptabfin

write.csv(proptabfin, "accesstomedicaregender.csv") #export to excel

#LaTeX format
stargazer(proptabfin, rowname = TRUE, colnames= TRUE, summary = FALSE)
