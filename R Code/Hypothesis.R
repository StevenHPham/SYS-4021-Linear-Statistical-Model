#Assignment 1
source("/Users/emirsahin/Documents/R STuff/Source/AccidentInput.R")

acts <- file.inputl("/Users/emirsahin/Documents/R STuff/Data/TrainData")

totacts <- combine.data(acts)

dim(totacts)

totacts$TOTCAS <- totacts$TOTKLD + totacts$TOTINJ

library(ggplot2)
library(ggpubr)
library(devtools) # for ggbiplot
library(ggbiplot)
library(GGally)

#------ EXTREME DAMAGE
dmgbox <- ggplot(totacts, aes(y=ACCDMG)) + geom_boxplot()

upper <- ggplot_build(dmgbox)$data[[1]]$ymax

xdmg <- totacts[totacts$ACCDMG > upper,]

which(xdmg$ACCDMG > 15e6)

xdmg <- xdmg[-186,]

xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

#Reset rownames (observation #s) for sequential numbering- otherwise they will remain the #s from totacts

rownames(xdmgnd) <- NULL

#--
Derail <- rep(0, nrow(totacts))
Derail[which(totacts$TYPE == 1)] <- 1 
Derail <- as.factor(Derail)
totacts$Derail <- Derail


hwyRailCrossing <-rep(0, nrow(totacts))
hwyRailCrossing[which(totacts$TYPE == 7)] <- 1
hwyRailCrossing <- as.factor(hwyRailCrossing)
totacts$hwyRailCrossing <- hwyRailCrossing



ggpairs(xdmgnd[,c( "TOTCAS", "Derail", "hwyRailCrossing" )])

totcas.lm1 <- lm(TOTCAS~Derail+hwyRailCrossing , data=xdmgnd)
summary(totcas.lm1)

totcas.lm2 <- lm(TOTCAS~as.factor(TYPE) +TRNSPD +Derail, data=xdmgnd)
totcas.lm2.step <- step(totcas.lm2)
summary(totcas.lm2)


#total acts
ggpairs(totacts[,c("TOTCAS","Derail","hwyRailCrossing")])
totcas.lm3.totacts <- lm(TOTCAS~Derail+hwyRailCrossing , data=totacts)
summary(totcas.lm3.totacts)

totcas.lm4.totacts <- lm(TOTCAS~as.factor(TYPE) +TRNSPD +Derail, data=totacts)
summary(totcas.lm4.totacts)




#-------
fdcas <-xdmgnd[,c("TRNSPD","CARSHZD","TONS","LOADED1", "TYPTRK", "ACCDMG", "TOTCAS")]
fdcas$TYPTRK <- as.factor(fdcas$TYPTRK)
ggpairs(fdcas)
#TRNSPD for sure - maybe both are correlated

ppcas <-xdmgnd[,c("DRUG", "ALCOHOL", "PASSTRN", "CAUSE", "TYPE", "ACCDMG", "TOTCAS", "METHOD")]
ppcas$CAUSE <- substr(ppcas$CAUSE,1,1)
ppcas$TYPE <- as.factor(ppcas$TYPE)
ggpairs(ppcas)

hzcas <- xdmgnd[,c("CARS", "CARSHZD", "CARSDMG", "ACCDMG", "CONDUCTR","IMO", "TOTCAS")]
ggpairs(hzcas)



ggplot(ppcas, aes(PASSTRN, TOTCAS)) + geom_boxplot()
ggplot(ppcas, aes(PASSTRN, ACCDMG)) + geom_boxplot()
ggplot(fdcas, aes(LOADED1, ACCDMG)) +geom_boxplot()
xdmgnd$METHOD
#---- No casualties
incas <- totacts[(xdmgnd$TOTCAS >= 1),]
ifdcas <-totacts[,c("TRNSPD","CARS","TONS","LOADED1", "TYPTRK", "ACCDMG", "TOTCAS")]
ippcas <-totacts[,c("DRUG", "ALCOHOL", "PASSTRN", "CAUSE", "ACCDMG", "TOTCAS")]
ippcas$CAUSE <- substr(ppcas$CAUSE,1,1)
ggpairs(ifdcas)
ggpairs(ippcas)

ggplot(ippcas, aes(PASSTRN, TOTCAS)) + geom_boxplot()
#PC
fdcorr <- princomp(fdcas, cor=T)
biplot(fdcorr)

#--- PICKED CARSHZD and PASSTRN
#Checking TYPEQ
xdmgnd$TYPEQ <- as.numeric(xdmgnd$TYPEQ)
xdmgnd$TYPEQ <- factor(xdmgnd$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint"))
table(xdmgnd$TYPEQ)
xdmgnd$PEEP <- with(xdmgnd, TYPEQ == 2 | TYPEQ == 3)

table(xdmgnd$PEEP)
  
