
#Q9
#create a new column called casualty
totacts$casualty <- totacts$TOTINJ + totacts$TOTKLD
graph <- ggplot(totacts, aes(y=casualty)) + geom_boxplot()



#Boxplot  parts
ggplot_build(graph)$data[[1]]$middle



#Q10-11 
#return number of accidents with 1 or more casualty
sum(totacts$casualty > 0)



#Q12
# Create a new data frame that have atleast 1 casualty
casualty_df <- totacts[totacts$casualty > 0,]



#Create a new dataframe with no duplicates
casualty_dfnd <- casualty_df[!(duplicated(casualty_df[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]



#Q13:
#Create boxplots by year
bwplot(as.factor(IYR)~ casualty, main = "Box plots" , data = casualty_dfnd  )



#Q14 Sum of casualties by year
aggregate(casualty_dfnd['casualty'], by=casualty_dfnd['IYR'], sum)



#Q16
ggplot(as.data.frame(table(xdmg$TYPE)), aes(x = Var1, y= Freq)) + geom_bar(stat="identity")


#Q17
table(casuality_dfnd$Cause)

#Q18 Find accident with largest  casualty (2002)
which(casualty_dfnd$casualty == max(casualty_dfnd$casualty))
casualty_dfnd[205,"Cause"]

#Q19
ggplot(as.data.frame(table(casualty_dfnd$TYPEQ)), aes(x = Var1, y= Freq)) + geom_bar(stat="identity")

#Q20 Sum of casualties
sum(casualty_dfnd$casualty)

#Q21 
casualty_dfnd[205,]

#Q22
source("SPM_Panel.R")
casualty_dfne <- casualty_dfnd[casualty_dfnd$casualty < 999,]
uva.pairs(casualty_dfne[,c("CONDUCTR","CDTRHR","HIGHSPD","CARS","DRUG","ALCOHOL","casualty", "ACCDMG")])
casualty_dfne$CONDUCTR

#Q23
cas.pca<-princomp(casualty_dfne[,c("CARS","TIMEHR","TEMP","casualty","TRNSPD")])
biplot(cas.pca)


barplot(cas.pca$loadings[,1], main='PC1 Loadings')
barplot(cas.pca$loadings[,2], main='PC2 Loadings')

#Q24 Cumulative Plot
cumplot(cas.pca, col = "blue")



