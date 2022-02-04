#***************************************************************
#
#      In-Class Grid Stability Classification Contest
#   		   
#***************************************************************

#***************************************************************
#
#  Read in the data
#
#***************************************************************

# Set working directory
sourcedir <- "~/Documents/4021/R Code"
datadir <- "~/Documents/4021/Data/GridStability"
setwd(sourcedir)

# Read in the data
GS.train <- read.csv(paste(datadir,"/GridStability_train.csv", sep=""), sep = ",", header = T)
GS.test <- read.csv(paste(datadir,"/GridStability_test.csv", sep=""), sep = ",", header = T)

# Source potentially useful functions
source("pc.glm.R")
source("PCAplots.R")
source("FactorPlots.R")
source("ROC.R")
source("TestSet.R")


#*************************************************************************************
#
# Build the best GLM you can to classify tumors as malignant
# (1) or benign (0) and write classifications to file
#
#*************************************************************************************

# Build model to training set:
GS.glm <- glm(stabf~., GS.train, family = binomial)

# Predict probabilities on testing set:
GS.pred <- predict(GS.glm, type = "response", newdata = GS.test)

# Convert probabilities to 0/1 classification based on threshold T
T <- 0.5
GS.classifications <- matrix(0,nrow=length(GS.pred),ncol=2)
GS.classifications[,1] <- c(1:length(GS.pred))
GS.classifications[which(GS.pred > T),2] <- 1

# Write predictions to file
colnames(GS.classifications) = c("Id","Predicted")
write.csv(GS.classifications, paste(datadir,"/sampleSubmission.csv", sep=""), row.names=FALSE)




#Submission

setwd(datadir)
dir.create(paste(datadir,"/figures",sep="")) #Create a folder "figures"
gridSt.data1 <- read.csv("GridStability_train.csv") #Create a dataframe

#Linear uva pairs
setwd(paste(datadir,"/figures", sep="")) 
png('Image1.png',width = 1920,height = 1080)
uva.pairs(gridSt.data1)
dev.off()
#Takeaway: g1-g4 is linear, the rest is log

#Log uva pairs
png('Image2.png',width = 1920,height = 1080)
uva.pairs((gridSt.data1[,c(1:12,13)])^2) #squared transformation instead of log trans because of negative values
dev.off()


#Takeaway: tau1-4 and g1-4 have high correlation coeff >0.19

png('Image3.png',width = 1920,height = 1080)
for(i in 1:4)
{
  assign(paste0("tau", i), ggplot(data = gridSt.data1, aes_string(x=as.factor(gridSt.data1$stabf),y=gridSt.data1[,i])) + 
           geom_boxplot(fill= "steelblue") +
           ggtitle(paste("V", i, sep = "")))
}

ggarrange(tau1,tau2,tau3,tau4,ncol=2,nrow=2)

dev.off()
