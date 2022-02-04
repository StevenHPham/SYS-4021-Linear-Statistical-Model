sourcedir <- "~/Documents/4021/R Code"
datadir <- "~/Documents/4021/Data/Spam"


library(forecast)
library(tidyverse)
library(lubridate)
library(ggplot2)
#install.packages("forecast")

setwd(datadir)
ham<-read.table('ham_ts.csv',header=T,sep=',')
spam<-read.table('spam_ts.csv',header=T,sep=',')
setwd(sourcedir)

ham.ts<-ts(ham$count)
spam.ts<-ts(spam$count)

sum(spam$count) / 364
sum(ham$count) / 506

date <- spam %>% select(year, month, day) %>% mutate(date = make_datetime(year,month,day))
spam$date <- date$date
ggplot(spam, aes(x=date,y=count)) + geom_line() + ylab("Spam Count") + xlab("")

acf(spam.ts)
ggAcf(spam.ts)


ham.season<-lm(ham.ts~Day)


#Is ham.trendseason significant?
summary(ham.season) 
#partial f test (small model , large model)
anova(ham.season,ham.trendseason)
