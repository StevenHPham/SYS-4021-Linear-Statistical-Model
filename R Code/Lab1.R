library(ggplot2)
library(ggpubr)
library(GGally)

#Question 9-12  & 14

ggplot(data = totacts, aes(x = as.factor(YEAR), y = CARSDMG)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Total Injured") +
  labs(x = "Year", y = "$")

#Question 13

which(totacts$TOTINJ == max(totacts$TOTINJ))
totacts$INCDTNO[4048]

#Question 15

hist(totacts$TEMP)
ggplot(as.data.frame(totacts$TEMP), aes(x=totacts$TEMP)) + geom_histogram(fill= NA,color= "steelblue", bins = nclass.Sturges(totacts$TEMP))  + ggtitle("Total Accident Damage in 2011") + labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))

#Q 16: Correlation
# SPM
source("SPM_Panel.R")
uva.pairs(xdmgnd[,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])
ggpairs(xdmgnd[,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])


#Q 19
which(totacts$ACCDMG == max(totacts$ACCDMG))
which(totacts$TOTINJ== max(totacts$TOTINJ))
totacts$IYR[47965]
totacts[47908,]
totacts[4048,]


#Q 25
sum(totacts$ACCDMG > 1500000)
nrow(totacts[totacts$ACCDMG > 1500000,])

#Q 26
sum(totacts$TOTKLD > 0)

#Q27




