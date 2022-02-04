  sourcedir <- "~/Documents/4021/R Code"
  datadir <- "~/Documents/4021/Data/"
  
  
  library(forecast)
  library(tidyverse)
  library(lubridate)
  library(ggplot2)
  
  setwd(datadir)
  app <-read.table('app.csv',header=T,sep=',')
  setwd(sourcedir)
?source("SPM_Panel.R")

boxplot(app$Price)

uva.pairs(app)

app.int <- lm(Price~(Users + Platform)^2 , data = app)
summary(app.int)

AIC(app.int)

app.main <- lm(Price ~. , data=app)
summary(app.main)
AIC(app.main)
plot(app.main,which = 4)

boxcox(app.main)

app.pca <- princomp(app[,c("Develop" ,"X5Star" , "CompSites" , "Date" , "Users", "Price" )],cor = T)
biplot(app.pca)
barplot(app.pca$loadings[,1], main='PC1 Loadings')

#END OF PART 1


#PART 2
setwd(datadir)
heart <- read.csv("heart.csv", sep=",", header=T)
setwd(sourcedir)

source("pc.glm.R")
source("PCAplots.R")
source("ROC.R")
source("TestSet.R")

spam.glm.main <- glm(V58~., data = spam, family = binomial)
spam.null <- glm(V58~1, data = spam, family = binomial)

#log transform of diag
log(Spam$train[,-58] + .1)
heart$diag <- log(heart$diag + .1)

heart.glm.null <- glm(diag~1, data=heart, family=binomial)
heart.glm.main <- glm(diag~(age + as.factor(cp) + as.factor(sex)) , data=heart, family=binomial)

anova(heart.glm.null,heart.glm.main,test = "Chi")

heart.glm2 <- glm(diag~(age + as.factor(cp) + as.factor(sex)) + restbps + chol + fbs, data=heart, family=binomial)

anova(heart.glm.main , heart.glm2 , test="Chi")

heart.glm3 <- glm(diag~(age + as.factor(cp) + as.factor(sex)) + restbps + chol + fbs + as.factor(restecg), data=heart, family=binomial)
heart.glm.step <- step(heart.glm3)
summary(heart.glm.step)


heart.glm.age <- glm(diag~age , data=heart, family = binomial)
anova(heart.glm.age,heart.glm.step , test = "Chi")


heart.pred <- predict(heart.glm.step , type = "response")
score.table(heart.pred , heart$diag , 0.5)

plot.roc(heart.pred , heart$diag , main = "ROC Curve - SPAM Filter", col = "blue")

heart.pca <- princomp(heart[,c("age" , "restbps" , "chol")])
cumplot(heart.pca)


heartpca.glm75 <- pc.glm(heart.pca, 75 , heart$diag)

heartpca.null <- pc.null(heart.pca , 75, heart$diag)

anova(heartpca.null, heartpca.glm75 , test="Chi")

AIC(heartpca.glm75)
AIC(heart.glm.step)

heart.pred2 <- predict(heartpca.glm75 , type = "response")
score.table(heart.pred2 , heart$diag , 0.5)

#END OF PART 2


#PART 3

setwd(datadir)
sunspot <- read.table("sunspot.csv", sep = ",", header = T)
setwd(sourcedir)


sun <- ts(sunspot[,2])

pg.sun <- spec.pgram(sun, spans=12, demean = T , log="no")
sun.spec <- data.frame(freq=pg.sun$freq , spec=pg.sun$spec)
ggplot(sun.spec) + geom_line(aes(x=freq,y=spec))


time.sun <- c(1:length(sun))
sun.trend <- lm(sun~time.sun + sin(2* 3.1415* time.sun  /12) + cos(2*3.1415*time.sun/12))

summary(sun.trend)


sun.ts.temp <- ts(sun.trend$residuals)
sun.acf <- ggAcf(sun.ts.temp)
sun.pacf <- ggPacf(sun.ts.temp)
ggarrange(sun.acf , sun.pacf,nrow=2,ncol=1)


sun.auto <- auto.arima(sun, approximation = FALSE)
summary(sun.auto)

tsdiag(sun.auto,lag=30)
