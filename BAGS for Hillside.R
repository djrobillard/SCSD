library(plyr)
library(dplyr)
library(scales)
library(xlsx)
library(lubridate)


############################################
#EHOUT
ehout<-read.csv("ehout2017.csv")

#create a counter for every incident (will use to sum by ID)
ehout$inccount = 1

#Subset by MP
ehoutref<-subset(ehout,IncidentType=="classroom")
ehoutclass<-subset(ehout,IncidentType=="referral")


#sum referrals,OSS,OSS Days by MP

RefOSS<-aggregate(cbind(OSS,OSS.Days,inccount)~OffenderID,data=ehoutref,FUN=sum)
RefISS<-aggregate(cbind(ISS,ISS.Days)~OffenderID,data=ehoutref,FUN=sum)

#subset classroom incidents by MP
Class<-aggregate(cbind(inccount)~OffenderID,data=ehoutclass,FUN=sum)


#change all column names
colnames(RefOSS)[which(names(RefOSS)=="inccount")]<-"Referrals"
colnames(RefOSS)[which(names(RefOSS)=="OSS")]<-"OSS"
colnames(RefOSS)[which(names(RefOSS)=="OSS.Days")]<-"OSS Days"
colnames(Class)[which(names(Class)=="inccount")]<-"Classroom Incidents"
colnames(RefISS)[which(names(RefISS)=="ISS")]<-"ISS"
colnames(RefISS)[which(names(RefISS)=="ISS.Days")]<-"ISS Days"

#merge all together
ehoutfinal<-merge(RefOSS,Class,"OffenderID",all=TRUE)
ehoutfinal<-merge(ehoutfinal,RefISS,"OffenderID",all=TRUE)


colnames(ehoutfinal)[which(names(ehoutfinal)=="OffenderID")]<-"Student ID"



############################################################
#combining files
hillside<-read.csv("Hillside.csv")
colnames(hillside)[which(names(hillside)=="Student.ID")]<-"Student ID"
hillsideBAGS<-merge(hillside,ehoutfinal,"Student ID")



write.csv(hillsideBAGS, "C:/Users/drobil66/Desktop/RFiles/HillsideBAGS.csv") 