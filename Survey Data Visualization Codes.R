##Install Package to Load Different Types of Datasets##

install.packages("gmodels") #Must be connected to the internet

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

###Beginning of the R Code###

##Load the Dataset##

afrodata <- read.csv(file.choose(), header = TRUE)

##Preparing/cleaning Data##

#Rename variables/creating new variables in data

afrodata$foodsecurity <- afrodata$Q8A #Good to do if you modify variables
colnames(afrodata)[colnames(afrodata)=="Q8A"] <- "foodsecurity" #Simply replacing the name

afrodata$gender <- afrodata$Q101 #Good to do if you modify variables
colnames(afrodata)[colnames(afrodata)=="Q101"] <- "gender" #Simply replacing the name

table(afrodata$foodsecurity) #check

#Recoding variables/cleaning data
afrodata$foodsecurity[afrodata$foodsecurity == -1] <- NA
afrodata$foodsecurity[afrodata$foodsecurity == 98] <- NA

afrodata$foodsecurity[afrodata$foodsecurity == 9] <- NA #without creating #foodsecknow variable

afrodata$foodsecknow <- afrodata$foodsecurity #creating foodsecknow variable first
afrodata$foodsecknow[afrodata$foodsecurity == 9] <- NA

#check results
table(afrodata$gender, afrodata$foodsecurity)

##creating crosstabs
CrossTable(afrodata$foodsecurity, afrodata$gender, weight = NULL) #using foodsecurity variable
CrossTable(afrodata$foodsecknow, afrodata$gender, weight = NULL) #using foodsecknow variable

###Combine Categories###
afrodata$foodsecurity1 <- afrodata$foodsecurity
afrodata$foodsecurity1[afrodata$foodsecurity1==0]<-1
afrodata$foodsecurity1[afrodata$foodsecurity1==1]<-1
afrodata$foodsecurity1[afrodata$foodsecurity1==2]<-2
afrodata$foodsecurity1[afrodata$foodsecurity1==3]<-2
afrodata$foodsecurity1[afrodata$foodsecurity1==4]<-2

CrossTable(afrodata$foodsecurity1, afrodata$gender, weight = NULL)


###Descriptive Statistics###
describe(afrodata$foodsecurity)

###Visualizations
##Histogram
hist(afrodata$foodsecurity, main = "Hunger and Food Insecurity in Africa", 
     xlab = "Hunger and Food Insecurity (0=Never without food 4=Always without food", 
     ylab = "Frequency", col = "cyan") #Add figure labels

##Frequency/Histogram plot by country# 
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

histogram(~afrodata$foodsecurity | factor(afrodata$countryname), 
          as.table=TRUE, layout=c(6,6), col="cyan", 
          xlab="Hunger and Food Insecurity in Africa By Country (0=Never, 4=always)")

##Kernel Density Plot for age and foodsecurity

densityplot <- density(afrodata$foodsecurity, na.rm = TRUE)
plot(densityplot, main = "Hunger and Food Insecurity", 
     xlab = "Responses (0=Never 4=Always)")

afrodata$age <- afrodata$Q113 #viewing age distribution of all respondents
densityplotage <- density(afrodata$age, na.rm = TRUE)
plot(densityplotage, main = "Age Distribution", xlab = "Age")

##Box plot
boxplot(afrodata$foodsecurity, ylab = "Hunger and Food Insecurity in Africa (0=Never 4=Always)")
boxplot(afrodata$foodsecurity ~ afrodata$gender, 
        ylab = "Hunger and Food Insecurity in Africa (0=Never 4= Always)", 
        xlab = "Gender", names = c("Men","Women"))
