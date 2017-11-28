library(plyr)
library(dplyr)

rawdata<-read.csv("CRDC2013_14_SCH.csv")

enrolldata<-select(rawdata,1259:1294,1315:1354)
discdata<-select(rawdata,1259:1294,1315:1354)
discdata$OverallSusp<-colSums(discdata[])
discdata$BlackSusp
discdata$HispanicSusp<-apply(discdata,1:2,19:20,37:38,58:59,sum)
discdata$WhiteSusp
discdata$IEPSusp
discdata$ELLSusp