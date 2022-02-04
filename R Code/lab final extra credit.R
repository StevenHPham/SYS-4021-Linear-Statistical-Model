sourcedir <- "~/Documents/4021/R Code"
datadir <- "~/Documents/4021/Data/Spam"


library(forecast)
library(tidyverse)
library(lubridate)
library(ggplot2)

setwd(datadir)
ttnic<-read.table('titanic.csv',header=T,sep=',')
setwd(sourcedir)
#Q1 : How many people survived? A:342
sum(ttnic$Survived == 1)

ttnic$Survived <- as.factor(ttnic$Survived)
ttnic$Pclass <- as.factor(ttnic$Pclass)
ttnic$Sex <- as.factor(ttnic$Sex)
ttnic$Cabin <- as.factor(ttnic$Cabin)
ttnic$Embarked <- as.factor(ttnic$Embarked)
ttnic$title <- as.factor(ttnic$title)


#Q2 : Build a model using the 4 most predictive variables of survival.  
#Which of the following variables are in your model? 
#Use uva.pairs with "Survival" and the variables below.


#Call SPM_Panel to use uva.pairs

source("SPM_Panel.R")

uva.pairs(ttnic)

ttnic.lm <- lm(Survived~(Pclass + Sex + Fare + as.factor(Cabin)) , data=ttnic)
summary(ttnic.lm)


#Q5 
ttnic.lm2 <- lm(Survived~(Age+Fare+as.factor(Sex)+as.factor(Pclass)), data=ttnic)
summary(ttnic.lm2)

#interaction
ttnic.lm2.int <- lm(Survived~(Age+Fare+Sex+Pclass)^2, data=ttnic)
summary(ttnic.lm2.int)

#stepwise 
ttnic.lm2.step <- step(ttnic.lm2)
summary(ttnic.lm2.step)

setwd(sourcedir)
source("pc.glm.R")
source("PCAplots.R")
source("ROC.R")
source("TestSet.R")

#Q8 : total errors: (false positive + false negative) 107 + 82 = 189
ttnic.pred <- predict(ttnic.lm2.step, type="response")
score.table(ttnic.pred, ttnic$Survived , .5)

#Q9 : ROC Curve

plot.roc(ttnic.pred , ttnic$Survived, main = "ROC Curve - SPAM Filter", col = "blue")
lines.roc(ttnic.pred, ttnic$Survived , col="orange")



##PART 2: AUTO and mpg
#auto csv
setwd(datadir)
auto <- read.table('auto.csv',header=T,sep=',')
auto$car <- as.factor(auto$car)
auto$horsepower <- as.factor(auto$horsepower)

setwd(sourcedir)
boxplot(auto$mpg)

max(auto$mpg)

setwd(sourcedir)
source("SPM_Panel.R")

#scatter plot matrix for linear relationship to mpg
uva.pairs(auto)

#Model w base case as 3 cylinders
mpg.main <- lm(mpg~(as.factor(cylinders) + weight + displacement), data=auto)
summary(mpg.main)

auto$cylinders <- as.factor(auto$cylinders)
contrasts(auto$cylinders)


#Changed base case to 4 cylinders
contrasts(auto$cylinders)<-matrix(c(1,0,0,0,0, 0,0,1,0,0, 0,0,0,1,0,  0,0,0,0,1),nrow=5)
colnames(contrasts(auto$cylinders)) <-matrix(c("3","5","6","8"),ncol=4)
contrasts(auto$cylinders)
#Rerun model w base case as 4 cylinders
mpg.main.rec <- lm(mpg~(cylinders + weight + displacement) , data=auto)
summary(mpg.main.rec)

#Q16 Diagnostic plots
plot(mpg.main.rec)
plot(mpg.main.rec, which=4) #Cook's Distance




#PART 3: Time series analysis
setwd(datadir)
gnp96 <- read.table("gnp96.dat")
gnp <- ts(gnp96[,2])

plot(log(gnp))

#q17
#Build a new model, gmp.trend, which predicts gnp based on the time variable, time.gnp

time.gnp <- c(1:length(gnp))
gnp.trend <- lm(log(gnp)~time.gnp)
summary(gnp.trend)

pg.spam <- spec.pgram(spam.ts , spans = 9 , demean = T , log="no")
spam.spec <- data.frame(freq=pg.spam$freq , spec=pg.spam$spec)
ggplot(spam.spec) + geom_line(aes(x=freq,y=spec))

#q18 periodogram
pg.gnp <- spec.pgram(log(gnp) , spans = 9 , demean = T,log='yes')

gnp.spec <- data.frame(freq=pg.gnp$freq , spec = pg.gnp$spec)

ggplot(gnp.spec) + geom_line(aes(x=freq,y=spec)) + 
  ggtitle("Smooth Periodogram of GNP")




#q19
pacf(log(gnp))

#Q20
acf(diff(log(gnp)))

#q21
pacf(diff(log(gnp)))

#q22

gnp.auto <- auto.arima(diff(log(gnp)), approximation = FALSE)
summary(gnp.auto)

#q24
gnp.forecast <- forecast(gnp.auto,h=4)
autoplot(gnp.forecast)
summary(gnp.forecast)





