#******************************************************
#
#					    
#			 		Multivariate Viz
#
#******************************************************

##############
# Get the data
##############

# Source AccidentInput

source("AccidentInput.R")

# you should have two data structures in working memory
# First - a list of data frames for each year of accident data

acts <- file.inputl(path)

# Next a data frame with all accidents from all years from 2001 - 2019
# with columns that are consistent for all of these years

# Get a common set the variables

comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))

# the combined data frame

totacts <- combine.data(acts)

dim(totacts)


#*************************************************
#
#		More Data Cleaning
#
#*************************************************

# Variable names

names(totacts)


# View the data types

str(totacts)

# Look at the type for TYPE using summary

summary(totacts$TYPE)

#Now, let's put more meaningful labels on TYPE variable
totacts$TYPE <- factor(totacts$TYPE, labels = c("Derailment", "HeadOn", "Rearend", "Side", "Raking", "BrokenTrain", "Hwy-Rail", "GradeX", "Obstruction", "Explosive", "Fire","Other","SeeNarrative" ))

# Use table() to see the frequencies

table(totacts$TYPE)

# Use barplot() to graph this

ggplot(as.data.frame(table(totacts$TYPE)), aes(x = Var1, y= Freq)) + geom_bar(stat="identity")

# Add color, a title, and change the text size


# Looks at TYPEQ

# First convert to numeric, using as.numeric()
totacts$TYPEQ <- as.numeric(totacts$TYPEQ)

# Now convert to factor- use actual categories from data dictionary to be more informative

totacts$TYPEQ <- factor(totacts$TYPEQ, labels = c("Freight", "Passenger", "Commuter", "Work",  "Single", "CutofCars", "Yard", "Light", "Maint"))

ggplot(as.data.frame(table(totacts$TYPEQ)), aes(x = Var1, y= Freq)) + geom_bar(stat="identity")

# Look at CAUSE with summary
summary(totacts$CAUSE)

# Create a new variable called Cause
# that uses labels for cause.
# Add it to totacts.

totacts$Cause <- rep(NA, nrow(totacts))

totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"

# This new variable, Cause, has to be a factor

totacts$Cause <- factor(totacts$Cause)

# use table() and barplot() to see it.
table(totacts$Cause)


# Look at histograms of TEMP with different breaks


# Change the color and title


# Now do the summary for totacts, but remove all narratives
# and any other variables you won't use for this project
cbind(1:ncol(totacts), names(totacts))
new.df <- totacts[,-c(122:136)]



#**************************************************
#
#			Scatter Plot Matrices
#
#**************************************************

# Scatter plots

df <- data.frame(year=2001:2019,damages=tapply(totacts$ACCDMG, as.factor(totacts$YEAR), sum))
ggplot(data=df, aes(x=year, y=damages)) + geom_line() + geom_point()

setwd(sourcedir)

source("SPM_Panel.R")
library(GGally)

# without panel functions for 2019

pairs(~  TRKDMG + EQPDMG + ACCDMG + TOTINJ + TOTKLD, data = acts[[19]])

# with panel function- a little more detail

ggpairs(acts[[19]][,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])

  

# Do this for all accidents

ggpairs(totacts[,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])

# Save as png to avoid problems in the document- make sure not to save in directory with data files

png("metrics.png")
ggpairs(totacts[,c("TRKDMG", "EQPDMG", "ACCDMG", "TOTINJ", "TOTKLD")])
dev.off()



#**************************************************
#
#		Trellis Categorical Plots
#
#**************************************************


# load the lattice library

library(lattice)

# Plotting damage per year

ggplot(data = totacts, aes(x = as.factor(YEAR), y = ACCDMG)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Accident Damage") +
  labs(x = "Damage ($)", y = "Year")

# put this in png format

# Which accident has the most damage?

which(totacts$ACCDMG == max(totacts$ACCDMG))

totacts$ACCDMG[which(totacts$ACCDMG == max(totacts$ACCDMG))]


# what type of accident had the most damage?

totacts$TYPE[which(totacts$ACCDMG == max(totacts$ACCDMG))]

# Find out the worst accident for total killed and injured

which(totacts$TOTINJ == max(totacts$TOTINJ))

totacts$TOTINJ[which(totacts$TOTINJ == max(totacts$TOTINJ))]

totacts$TOTINJ[4048]
totacts[4048,]
totacts[4048,122:136]
totacts$ACCDMG[4048]


# Find the worst accidents in 2018.  What happened?
totacts$ACCDMG[which(totacts$ACCDMG == max(totacts$ACCDMG[which(totacts$IYR==18)]))]

totacts$TOTINJ[which(totacts$TOTINJ == max(totacts$TOTINJ[which(totacts$IYR==18)]))]

totacts$TOTKLD[which(totacts$TOTKLD == max(totacts$TOTKLD[which(totacts$IYR==18)]))]

#Find the accidents with the most injuries in 2018.  What happened in these accidents?  
#Explore the news to understand underying causes and what could have been done to presvent these accidents.

which(totacts$TOTINJ > 50 & totacts$IYR==18)

#Find the accident with the most damage in 2018.  What do you notice about the accidents with the most damage and those with the most injuries?

which(totacts$ACCDMG == max(totacts$ACCDMG[which(totacts$IYR==18)]))
which(totacts$ACCDMG > 5e6 & totacts$IYR==18)

# Plotting accident cause vs. damage

ggplot(data = totacts, aes(x = Cause, y = ACCDMG)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Accident Damage") +
  labs(x = "Damage ($)", y = "Accident Cause")


bwplot(Cause~ log(ACCDMG+1), main = "Box Plots of Log(Accident Damage)", xlab = "log(Damage ($))", ylab = "Accident Cause", data = totacts)

ggplot(data = totacts, aes(x = Cause, y = log(ACCDMG+1))) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Log(Accident Damage)") +
  labs(x = "log(Damage ($))", y = "Accident Cause")



# Plot cause vs. no. killed or injured

ggplot(data = totacts, aes(x = Cause, y = TOTKLD)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Box Plots of Total Killed")+
  labs(x = "Total Killed", y = "Accident Cause")

ggplot(data = totacts, aes(x = as.factor(YEAR), y = EQPDMG)) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.8) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Box Plots of Total Injured") +
  labs(x = "Year", y = "$")


# X-Y plots conditioned on cause

qplot(ACCDMG, TOTKLD, data = totacts) + facet_wrap(~ Cause, scales = "free") +
  ggtitle("Damage vs. Killed Conditioned on Cause") + 
  labs(x = "Total Killed", y = "Total Accident Damage") +
  theme(plot.title = element_text(hjust = 0.5))


