sourcedir <- "~/Documents/4021/R Code"
datadir <- "~/Documents/4021/Data/Spam"
setwd(sourcedir)
spam <- read.table(paste(datadir,"/Spam.txt", sep=""), sep = " ", header = F)

setwd(paste(datadir,"/figures", sep="")) 

source("pc.glm.R")
source("PCAplots.R")
source("ROC.R")
source("TestSet.R")


#Q16 boxplots of V20-28
png('Image6.png',width = 1920,height = 1080)
uva.pairs(log(spam[,c(20:28,58)]+.1))
dev.off()

#Q15 boxplots of V1-9
png('Image7-1to9.png',width = 1920,height = 1080)
uva.pairs(log(spam[,c(1:9,58)]+.1))
dev.off()

#Q16 boxplots of 49-57
png('Image8-49to57.png',width = 1920,height = 1080)
uva.pairs(log(spam[,c(49:57,58)]+.1))
dev.off()

#Q18 Log transform variables 1-57 with a 0.1 offset then obtain their principal components using the correlation matrix.  
#Produce the cumplot for the log transformed variables.
spam.pca.lab5 <- princomp(log(spam[,c(1:57)]+0.1), cor = T)
cumplot(spam.pca.lab5)

#q19 biplot
biplot.fact(spam.pca.lab5, spam[,58])

#q20: 1:10 & 48:57 (first & last 10)
spam10 <- log(spam[,c(1:10 , 48:57)]+.1)

spam10$V58 <- spam[,58]
table(spam$V58)
table(spam10$V58) #verify that v58 is the same 

spam10.glm.main <- glm(V58~., data = spam10, family = binomial)
summary(spam10.glm.main)



#null model for deviance test 
spam10.null <- glm(V58~1, data = spam10, family = binomial)
anova(spam10.null, spam10.glm.main, test = "Chi")

#Created a main effect & null model. Ran an anova test to see if the main model is significant. Yes, is significant

#Q21
library(MASS)
drop1(spam10.glm.main, response~., test = "Chi", data = spam10)
#if we drop variable 1, will the deviance be significantly worse? if *** yes
#we can drop the variables that are not significant to the model BUT THIS IS NOT A GOOD WAY TO BUILD A MODEL
#instead, we should do the opposite, start w/ a small model with good variables and add more 

#Q22
(exp(spam10.glm.main$coefficients[5])-1)*100
#if we change variable 4 by 1 unit, odds of being spam increase by 132%
#Double check w Prof. Quinn if this is already log(v4) or just v4

#23 prediction on main model
spam10.pred <- predict(spam10.glm.main, type = "response")
score.table(spam10.pred, spam10[,21], .5)
#double check w prof quinn false positive 148

#24 stepwise on the main effects model
step.spam10 <- step(spam10.glm.main, data = spam10, family = binomial)

#25 partial likelihood test: stepwise vs main effect model
anova(step.spam10,spam10.glm.main,test = "Chi")
#Takeaway: main model is not significant


#26 aic values for main vs stepwise model
#smaller aic is better & stepwise is smaller (2436) than main (2444)

#27 prediction on stepwise model
spam10.step.pred <- predict(step.spam10, type = "response") 
score.table(spam10.step.pred, spam10[,21], .5)



#28 ROC curve

plot.roc(spam10.pred, spam10[,21], main = "ROC Curve - SPAM Filter", col = "blue")
lines.roc(spam10.step.pred, spam10[,21], col = "orange")


#30 pca for spam10
spam10.pca <- princomp(spam10[,-21], cor = T)
cumplot(spam10.pca)

# pca model
spam10pca.glm98 <- pc.glm(spam10.pca, 98, spam10[,21])
spam10pca.null <- pc.null(spam10.pca, 98, spam10[,21])
anova(spam10pca.null, spam10pca.glm98, test = "Chi")

#31 confusion matrix for false neg
spam10pca.pred <- predict(spam10pca.glm98, type = "response")
score.table(spam10pca.pred, spam10[,21], .5)
