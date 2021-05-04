
install.packages("effects") #Must be connected to the internet

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
library(effects)

###Beginning of the R Code###

##Load the Dataset##

afrodata <- read.csv(file.choose(), header = TRUE)

##Preparing the Data##

afrodata$managingeconomy <- afrodata$Q66A #create categorical variable
afrodata$managingeconomydi <- afrodata$Q66A #create dichotomous variable


#Inspect
table(afrodata$managingeconomy)
table(afrodata$managingeconomydi)


#Recoding dependent variables
afrodata$managingeconomy[afrodata$managingeconomy==-1]<-NA #dropping missing responses
afrodata$managingeconomy[afrodata$managingeconomy==99]<-NA #dropping refused to answers
afrodata$managingeconomy[afrodata$managingeconomy==9]<- NA #dropping don't knows
afrodata$managingeconomy[afrodata$managingeconomy==0]<- NA #dropping 0 score (not in code book)
table(afrodata$managingeconomy) #inspect

afrodata$managingeconomydi[afrodata$managingeconomydi==-1]<-NA #drop missing values
afrodata$managingeconomydi[afrodata$managingeconomydi==99]<-NA #drop refused to answers
afrodata$managingeconomydi[afrodata$managingeconomydi==9]<-NA #dropping don't knows
afrodata$managingeconomydi[afrodata$managingeconomydi==0]<- NA #dropping 0 score (not in code book)
afrodata$managingeconomydi[afrodata$managingeconomydi==1]<-0
afrodata$managingeconomydi[afrodata$managingeconomydi==2]<-0
afrodata$managingeconomydi[afrodata$managingeconomydi==3]<-1
afrodata$managingeconomydi[afrodata$managingeconomydi==4]<-1 #combining into dichotomous variable
table(afrodata$managingeconomydi) #inspect

#Recoding independent variables

afrodata$livingconditions <- afrodata$Q4B #recoding living conditions variable
afrodata$livingconditions[afrodata$livingconditions==9] <-NA #dropping don't know values
afrodata$livingconditions[afrodata$livingconditions==-1]<-NA #dropping missing values
afrodata$livingconditions[afrodata$livingconditions==98]<-NA #dropping refused to answer responses
table(afrodata$livingconditions)


afrodata$economicconditions <- afrodata$Q4A #recoding economic conditions variable
table(afrodata$economicconditions) #inspect
afrodata$economicconditions[afrodata$economicconditions==-1]<-NA #dropping missing responses
afrodata$economicconditions[afrodata$economicconditions==9]<-3 #code at neither good nor bad responses


afrodata$sophistication <- afrodata$Q14 #recoding sophistication variable
table(afrodata$sophistication) #inspect
afrodata$sophistication[afrodata$sophistication==-1]<-NA #dropping missing responses value
afrodata$sophistication[afrodata$sophistication==9]<-3 #Coded at the middle value as (sometimes)
afrodata$sophistication[afrodata$sophistication==98]<-NA #drop don't know values

afrodata$rightdirection <- afrodata$Q3 #recoding direction of economy variable
table(afrodata$rightdirection) #inspect
afrodata$rightdirection[afrodata$rightdirection==-1]<-NA #drop missing
afrodata$rightdirection[afrodata$rightdirection==3]<-NA #drop miscellaneous value
afrodata$rightdirection[afrodata$rightdirection==98]<-NA #drop refused to answer
afrodata$rightdirection[afrodata$rightdirection==2]<-3 #shift wrong direction to scale 3
afrodata$rightdirection[afrodata$rightdirection==9]<-2 #recode don't know to 2

afrodata$education <- afrodata$Q97 #recoding education variable
table(afrodata$education) #inspect
afrodata$education[afrodata$education==-1]<-NA #drop missing responses
afrodata$education[afrodata$education==99]<-NA #drop don't knows
afrodata$education[afrodata$education==98]<-NA #drop refused to answer responses


###Descriptive Statistics###


descriptivetab <- stat.desc(afrodata[,c("managingeconomy", "livingconditions", "economicconditions",
                                      "sophistication", "rightdirection", "education")])

descriptivetabt <- t(descriptivetab)
colnames(descriptivetabt) <- c("N", "Null", "Missing/ No Value", "Min", "Max", "Range", "Sum", "Median", 
                               "Mean", "SE Mean", "CI Mean 95%", "Variance", "Std. Deviation", 
                               "Coef. Variance")
rownames(descriptivetabt) <- c("Managing The Economy", "Living Conditions", "Economic Conditions", 
                               "Sophistication", "Right or Wrong Direction", "Education")

stargazer(descriptivetabt, summary = FALSE, omit = c("Null", "Range", "Sum", "Coef. Variance"))


###Regression Analysis###

#model 1
mod1 <- lm(managingeconomy ~ economicconditions + livingconditions + sophistication + rightdirection
           + education, data = afrodata)
summary(mod1)
stargazer(mod1)

coefplot(mod1, intercept=TRUE, sort=c("natural"), decreasing=TRUE, title="Managing The Economy"
         , newNames = c(livingconditions = "Living Conditions", economicconditions = "Economic Conditions",
                        sophistication = "Sophistication", rightdirection = "Right or Wrong Direction", 
                        education = "Education")
         , predictors = c("livingconditions", "economicconditions", "sophistication", "rightdirection", "education")
         , pointSize=4, color="blue", lwdInner=1, lwdOuter=1, zeroColor="red")

#model 2
mod2 <- lm(managingeconomy ~ livingconditions + economicconditions + education + rightdirection, data = afrodata)
summary(mod2)
stargazer(mod2)

stargazer(mod1, mod2)

coefplot(mod2, intercept=TRUE, sort=c("natural"), decreasing=TRUE, title="Managing The Economy"
         , newNames = c(livingconditions = "Living Conditions", economicconditions = "Economic Conditions",
                        rightdirection = "Right or Wrong Direction", education = "Education")
         , predictors = c("livingconditions", "economicconditions", "rightdirection", "education")
         , pointSize=4, color="blue", lwdInner=1, lwdOuter=1, zeroColor="red")


modlogit1 <- glm(managingeconomydi ~ livingconditions + economicconditions + 
                   sophistication + education + rightdirection, data = afrodata, family = "binomial" (link="logit"))
summary(modlogit1)
stargazer(modlogit1)


###Marginal Effects Plots###
modlogit1effects <- allEffects(modlogit1)

effect("livingconditions", modlogit1)
plot(effect("livingconditions", modlogit1), main="Partial Effect of Living Conditions", 
     ylab="Probability of Giving Better Assessment of How Economy Is Managed"
     , xlab="Living Conditions", rug = FALSE)

effect("economicconditions", modlogit1)
plot(effect("economicconditions", modlogit1), main="Partial Effect of Economic Conditions", 
     ylab="Probability of Giving Better Assessment of How Economy Is Managed"
     , xlab="Economic Conditions", rug = FALSE)

effect("sophistication", modlogit1)
plot(effect("sophistication", modlogit1), main="Partial Effect of Sophistication", 
     ylab="Probability of Giving Better Assessment of How Economy Is Managed"
     , xlab="Sophistication", rug = FALSE)

effect("education", modlogit1)
plot(effect("education", modlogit1), main="Partial Effect of Education", 
     ylab="Probability of Giving Better Assessment of How Economy Is Managed"
     , xlab="Education", rug = FALSE)

effect("rightdirection", modlogit1)
plot(effect("rightdirection", modlogit1), main="Partial Effect of Right or Wrong Direction", 
     ylab="Probability of Giving Better Assessment of How Economy Is Managed"
     , xlab="Right or Wrong Direction", rug = FALSE)
