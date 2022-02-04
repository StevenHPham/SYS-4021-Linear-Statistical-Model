#********************************************************************************
#             
#					Univariate Graphics
#
#********************************************************************************


#************************************
# Reading in data
#************************************

# Set working directory

# For example, an Mac user

traindir <- "~/Documents/4021/Data/TrainData"
sourcedir <-"~/Documents/4021/R Code"


setwd(traindir)


#***********************************************

# Read in the accident files one at at time
# for the first questions in the in-class assignment we will 
# only use data from 2019

acts19 <- read.csv("RailAccidents19.csv")

acts17 <- read.csv("RailAccidents17.csv")
which.max(acts17$ACCDMG)
acts17[91]
#**************************************************

# To get a summary of all of the variables use

summary(acts19)

# To get a summary of a subset of the variables (e.g., "ACCDMG", "TOTKLD", "CARS" ) 
# you can use

summary(acts19[,c("ACCDMG", "TOTKLD", "CARS")])


# To get individual statistics (e.g. mean, var) you can use

mean(acts19$TOTKLD)

var(acts19$TOTKLD)

# You can round your answer using the round() function

round(mean(acts19$TOTKLD))

#**************************************************

# You will need to read in all 18 years of the data 
# You will put the data into a data structure called a list

# To do this you will use code I have written, AccidentInput.R 
# Put that file in your working directory and then source it:

setwd(sourcedir)
source("AccidentInput.R")

# Now use it to read in all the data. You must have ALL and ONLY the rail accident data
# files in one directory. Call that directory and its path, path.
# You can give your data a name
# In my examples below I use acts as the name for data sets
# Then use the command

setwd(traindir)

my.files <- list.files(traindir)

acts <- lapply(my.files, read.csv)

acts <- file.inputl(traindir) 

# path is the specification of the path to your file.

# Now acts[[1]] is the data set from year 2001, 
# acts[[2]] is the data set from year 2002, etc.

# Before we put all the data into one data frame
# we must clean the data

##################################################
#
#	Data Cleaning
#
##################################################

# Are the number of columns different from year to year?

ncol(acts[[1]])
ncol(acts[[8]])
ncol(acts[[7]])


# Get a common set the variables
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[8]]))


# Now combine the data frames for all 17 years
# Use combine.data()

totacts <- combine.data(acts, comvar)

dim(totacts)

boxplot(totacts$EQPDMG)


#***********************************
#
# 	histograms of ACCDMG
#
#***********************************

library(ggplot2)

df<-as.data.frame(acts[[11]]$ACCDMG)
# These examples are for 2011 
# Histograms are both in default way and also with ggplot2 package


# for 2011
ggplot(as.data.frame(acts[[11]]$ACCDMG), 
       aes(x=acts[[11]]$ACCDMG)) + 
  geom_histogram(fill= "steelblue") + 
  ggtitle("Total Accident Damage in 2011") + 
  labs(x = "Dollars ($)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))



# Different bin widths

# Bin FD
ggplot(as.data.frame(acts[[11]]$ACCDMG), 
       aes(x=acts[[11]]$ACCDMG)) + geom_histogram(fill= "steelblue", bins = nclass.FD(acts[[11]]$ACCDMG)) + ggtitle("Total Accident Damage in 2011") + labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))

# Bin Scott
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + geom_histogram(fill= "steelblue", bins = nclass.scott(acts[[11]]$ACCDMG)) + ggtitle("Total Accident Damage in 2011") + labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))

# Bin 20
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + geom_histogram(fill= "steelblue", bins = 20) + ggtitle("Total Accident Damage in 2011") + labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))

# Bin 100
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + geom_histogram(fill= "steelblue", bins = 100) + ggtitle("Total Accident Damage in 2011") + labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))


# other years

library(ggpubr)

a <- ggplot(as.data.frame(acts[[1]]), aes(x=acts[[1]]$ACCDMG)) + geom_histogram(fill= "steelblue") + ggtitle("Total Accident Damage in 2001") + labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))
b <- ggplot(as.data.frame(acts[[4]]), aes(x=acts[[4]]$ACCDMG)) + geom_histogram(fill= "steelblue") + ggtitle("Total Accident Damage in 2004") + labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))
c <- ggplot(as.data.frame(acts[[8]]), aes(x=acts[[8]]$ACCDMG)) + geom_histogram(fill= "steelblue") + ggtitle("Total Accident Damage in 2008") + labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))
d <- ggplot(as.data.frame(acts[[11]]), aes(x=acts[[11]]$ACCDMG)) + geom_histogram(fill= "steelblue") + ggtitle("Total Accident Damage in 2011") + labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))

ggarrange(a,b,c,d, ncol=2, nrow = 2)



# Accident damage 2001-2019

# First Solution: Loop with assign function
for(i in 1:14)
{
  j <- which(colnames(acts[[11]]) == "EQPDMG")
  if(i < 10)
  {
    assign(paste0("Acc", i), geom_boxplot(as.data.frame(acts[[i]][,j]), aes_string(x=acts[[i]]$EQPDMG)) + geom_histogram(fill= "steelblue") + ggtitle(paste("Total Accident Damage in 200", i, sep = "")) + labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5)))
  }
  else
  {
    assign(paste0("Acc", i), geom_boxplot(as.data.frame(acts[[i]][,j]), aes_string(x=acts[[i]]$EQPDMG)) + geom_histogram(fill= "steelblue") + ggtitle(paste("Total Accident Damage in 20", i, sep = "")) + labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5)))
  }
}

ggarrange(Acc1,Acc2,Acc3,Acc4,Acc5,Acc6,Acc7,Acc8,Acc9,Acc10,Acc11,Acc12,Acc13, ncol = 3, nrow = 5)


# Second Solution: Loop with creating a List
List <- list()

for(i in 1:14)
{
  j <- which(colnames(acts[[11]]) == "ACCDMG")
  Namelist <- paste( 'Acc', i, sep = '' )
  
  if(i < 10)
  {
    List[[ Namelist ]] <- ggplot(as.data.frame(acts[[i]][,j]), aes_string(x=acts[[i]]$ACCDMG)) + geom_histogram(fill= "steelblue") + ggtitle(paste("Total Accident Damage in 200", i, sep = "")) + labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))
  }
  else
  {
    List[[ Namelist ]] <- ggplot(as.data.frame(acts[[i]][,j]), aes_string(x=acts[[i]]$ACCDMG)) + geom_histogram(fill= "steelblue") + ggtitle(paste("Total Accident Damage in 20", i, sep = "")) + labs(x = "Dollars ($)", y = "Frequency") + theme(plot.title = element_text(hjust = 0.5))
  }
  
}

ggarrange(List$Acc1, List$Acc2, List$Acc3, List$Acc4, List$Acc5, List$Acc6, List$Acc7, List$Acc8, List$Acc9, List$Acc10, List$Acc11, List$Acc12, List$Acc13, List$Acc14, ncol = 3, nrow = 5)

# Damage in constant scales
for(i in 1:11)
{
  j <- which(colnames(acts[[11]]) == "ACCDMG")
  if(i < 10)
  {
    assign(paste0("AccS", i), ggplot(as.data.frame(acts[[i]][,j]), aes_string(x=acts[[i]]$ACCDMG)) + geom_histogram(fill= "steelblue") + ggtitle(paste("Total Accident Damage in 200", i, sep = "")) + labs(x = "Dollars ($)", y = "Frequency") + xlim(c(0,1.7e7)) + ylim(c(0,4000)) + theme(plot.title = element_text(hjust = 0.5)))
  }
  else
  {
    assign(paste0("AccS", i), ggplot(as.data.frame(acts[[i]][,j]), aes_string(x=acts[[i]]$ACCDMG)) + geom_histogram(fill= "steelblue") + ggtitle(paste("Total Accident Damage in 20", i, sep = "")) + labs(x = "Dollars ($)", y = "Frequency") + xlim(c(0,1.7e7)) + ylim(c(0,4000)) + theme(plot.title = element_text(hjust = 0.5)))
  }
}

ggarrange(AccS1,AccS2,AccS3,AccS4,AccS5,AccS6,AccS7,AccS8,AccS9,AccS10,AccS11, ncol = 3, nrow = 4)




#*********************************************************************
#
# 				Boxplots of ACCDMG
#
#*********************************************************************
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage in 2001") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue", outlier.shape = "*", outlier.size = 5) + ggtitle("Total Accident Damage in 2001") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip() 

ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue", outlier.shape = 4, outlier.size = 2) + ggtitle("Total Accident Damage in 2001") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

ggplot(as.data.frame(acts[[6]]$ACCDMG), aes(x=acts[[6]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage in 2001") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

#4 plots on a single graph

e <- ggplot(as.data.frame(acts[[1]]$ACCDMG), aes(x=acts[[1]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage in 2001") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()
f <- ggplot(as.data.frame(acts[[4]]$ACCDMG), aes(x=acts[[4]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage in 2001") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()
g <- ggplot(as.data.frame(acts[[8]]$ACCDMG), aes(x=acts[[8]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage in 2001") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()
h <- ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage in 2001") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()

ggarrange(e, f, g, h, ncol = 2, nrow = 2)


#With a constant scale
i <- ggplot(as.data.frame(acts[[1]]$ACCDMG), aes(x=acts[[1]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage in 2001") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip() + xlim(c(0,1.7e7))
j <- ggplot(as.data.frame(acts[[4]]$ACCDMG), aes(x=acts[[4]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage in 2001") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()+ xlim(c(0,1.7e7))
k <- ggplot(as.data.frame(acts[[8]]$ACCDMG), aes(x=acts[[8]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage in 2001") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()+ xlim(c(0,1.7e7))
l <- ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(x=acts[[11]]$ACCDMG)) + 
  geom_boxplot(col= "steelblue") + ggtitle("Total Accident Damage in 2001") +
  labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()+ xlim(c(0,1.7e7))

ggarrange(i, j, k, l, ncol = 2, nrow = 2)


#Another way of getting multiple plots on a single graph
for(i in 1:11)
{
  j <- which(colnames(acts[[11]]) == "ACCDMG")
  if(i < 10)
  {
    assign(paste0("AccB", i), ggplot(as.data.frame(acts[[i]][,j]), aes_string(x=acts[[i]]$ACCDMG)) + 
             geom_boxplot(col= "steelblue") + ggtitle(paste("Total Accident Damage in 200", i, sep = "")) +
             labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip())
  }
  else
  {
    assign(paste0("AccB", i), ggplot(as.data.frame(acts[[i]][,j]), aes_string(x=acts[[i]]$ACCDMG)) + 
             geom_boxplot(col= "steelblue") + ggtitle(paste("Total Accident Damage in 20", i, sep = "")) +
             labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip())  }
}

ggarrange(AccB1,AccB2,AccB3,AccB4,AccB5,AccB6,AccB7,AccB8,AccB9,AccB10,AccB11, ncol = 3, nrow = 4)

#With a constant scale
for(i in 1:11)
{
  j <- which(colnames(acts[[11]]) == "ACCDMG")
  if(i < 10)
  {
    assign(paste0("AccBC", i), ggplot(as.data.frame(acts[[i]][,j]), aes_string(x=acts[[i]]$ACCDMG)) + 
             geom_boxplot(col= "steelblue") + ggtitle(paste("Total Accident Damage in 200", i, sep = "")) +
             labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip())+ xlim(c(0,1.7e7)) 
  }
  else
  {
    assign(paste0("AccBC", i), ggplot(as.data.frame(acts[[i]][,j]), aes_string(x=acts[[i]]$ACCDMG)) + 
             geom_boxplot(col= "steelblue") + ggtitle(paste("Total Accident Damage in 20", i, sep = "")) +
             labs(x = "Dollars ($)") + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()) + xlim(c(0,1.7e7)) 
    }
  
}

ggarrange(AccBC1,AccBC2,AccBC3,AccBC4,AccBC5,AccBC6,AccBC7,AccBC8,AccBC9,AccBC10,AccBC11, ncol = 3, nrow = 4)


#*****************************
#
#		QQ Plots
#
#*****************************

#Does our data come from a Guassian distribution
ggplot(as.data.frame(acts[[11]]$ACCDMG), aes(sample=acts[[11]]$ACCDMG)) + stat_qq() + stat_qq_line() + ggtitle("Total Accident Damage") + theme(plot.title = element_text(hjust = 0.5))

ggplot(as.data.frame(acts[[11]]$TEMP), aes(sample=acts[[11]]$TEMP)) + stat_qq() + stat_qq_line() + ggtitle("Accident Temperature") + theme(plot.title = element_text(hjust = 0.5))


