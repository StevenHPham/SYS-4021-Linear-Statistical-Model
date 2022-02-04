#load libraries
library(ggplot2)
library(GGally)
library(devtools) # for ggbiplot
library(ggbiplot)
library(MASS)
library(lindia)
library(ggbiplot)


dataFolder <- "~/Documents/4021/Data/TrainData"
setwd(dataFolder)

housing <- read.csv("housing-prices.csv")

country <- read.csv("Country-data.csv")

housing

#Q1 find outlier in boxplot
boxplot(housing$Price)
boxplot.stats(housing$Price)$out #Number of outliers : 0
ggplot(housing, aes(y=Price)) + geom_boxplot()



boxplot(country$child_mort)
boxplot.stats(country$child_mort)$out
which(country$child_mort == 160)
country[which(country$child_mort == 160),]


#Q30 Lowest child mortality rate
countryBoxplot <- ggplot(country, aes(y=child_mort)) + geom_boxplot() #create a boxplot
ggplot_build(countryBoxplot)$data[[1]]$ymin #find min of boxplot
country[which(country$child_mort == 2.6),] #find row that is ymin


#Q2 boxplot position 
names(ggplot_build(dmgbox)$data[[1]]) #return name of boxplot position 
housingBox <- ggplot(housing, aes(y=price)) + geom_boxplot()
upper <- ggplot_build(housingBox)$data[[1]]$ymax
upper


#Q3 scatter plot matrix
#df <- data.frame(year=2001:2019,damages=tapply(totacts$ACCDMG, as.factor(totacts$YEAR), sum))
#ggplot(data=df, aes(x=year, y=damages)) + geom_line() + geom_point()

ggpairs(housing[,c("Price", "Size", "Age", "Baths","Rooms")])
ggpairs(housing[,c("price", "City")],cardinality_threshold = 48)

ggpairs(country[,c("child_mort","exports","health","imports","income","inflation","life_expec","total_fer","gdpp")])

#Q5: main effects model  linear regression lm
houses.lm1 <-lm(Price~Baths ,data=housing)
summary(houses.lm1)

#Q7 main effect and interaction terms
houses.lm2 <-lm(Price~Baths+Rooms+Age+Size ,data=housing)
summary(houses.lm2)

#Q8: partial F test
anova(houses.lm1,houses.lm2)
#Beta are x:y = 0

#Q10:  AIC 
AIC(house.main)
AIC(house.inter)

#Q11 Based on the AIC and adjusted-R2 values of models house.main and house.inter,  
#    which of the following statements are true?
#   
#         We choose the smaller model based on AIC and adjusted-R2.

#Q12 Cook's distance **double check distance on y-axis 
plot(house.main,labels.id = NULL, which=4) #which=4 is Cook's distance
plot(house.main,labels.id = NULL) #this is to cycle through all diagnostic plots


#Q13
#Bad residual vs fitted == lack of fit aka bad model


#Q14 Box cox- log transformation raised to lambda
gg_boxcox(house.main)



#Q15 set base case , levels ,  and change to a factor
contrasts(housing$City)
#
#Lots of cities leads to big matrix (greater than 40 levels of City), 
#First city that shows up is the default base case


#Q16 correlation matrix, principal components pca

library(ggbiplot)
library(devtools) 
library(data.table)

#Example for PCA.R
xdmgnd.pca <- princomp(xdmgnd[,c("CARSDMG","EQPDMG", "TRKDMG","ACCDMG", "TOTKLD", "TOTINJ")])

#biplot 
housing.pca <- princomp(housing[,c("price","sqft","bedrooms","baths")] , cor = T)
biplot(housing.pca)

houses.pca <- princomp(housing[,c("Price","Rooms","Baths","Size")],cor = T)
biplot(houses.pca)
#closest = most correlated
#furthest away = most independent

# SPM
uva.pairs(housing)


#Q16 which 2 variables have the largest absolute loadings in the first principal component
screeplot(housing.pca)
housing.pca$loadings

houses.pca$loadings
screeplot(houses.pca)

# number of variable == number of components
#component 1 have highest variability, meaning theres correlation between the variables

#Q17 explain 90% of variance in data cumsum plot
cumplot(housing.pca, col = "blue")
aa <- cumplot(houses.pca, col = "blue")

country.pca <- princomp(country[,c("child_mort","exports","health","imports","income","inflation","life_expec","total_fer","gdpp")],cor = T)
ab <- cumplot(country.pca, col = "blue")
biplot(country.pca)

install.packages("olsrr")
library(olsrr)

ols_test_breusch_pagan(house.main)
#Hypothesize if the variance is constant

country[134,]


#q13 on exam - stepwise model from main effects + interaction model for all predictor variables
houses.lm3 <-lm(Price~(Baths+Rooms+as.factor(Age)+Size)^2 ,data=housing)
houses.lm3.step <- step(houses.lm3)

summary(houses.lm3.step)

plot(houses.lm3.step,labels.id = NULL,which = 4)
gg_boxcox(houses.lm3.step)



ols_test_breusch_pagan(houses.lm3.step)

barplot(houses.pca$loadings[,1], main='PC1 Loadings')
barplot(houses.pca$loadings[,2], main='PC2 Loadings')

barplot(country.pca$loadings[,2], main='PC1 Loadings')
country.pca$loadings


