ehout<-read.csv("ehout2017.csv")
library(lubridate)

###parse dates and create time intervals
ehout$date <- mdy(ehout$IncidentDate) # puts into date format
ehout$year<-year(ehout$date)
ehout$month<-month(ehout$date)
ehout$month<-sprintf("%02d",ehout$month)
ehout$day<-day(ehout$date)
ehout$day<-sprintf("%02d",ehout$day)

ehout$date.num <- as.numeric(paste0(ehout$year, ehout$month, ehout$day)) 

ehout$MP<-ifelse(ehout$date.num>20160815 & ehout$date.num<20161127, 1, 
                  ifelse(ehout$date.num>20161128 & ehout$date.num<20170127, 2, 
                         ifelse(ehout$date.num>20170128 & ehout$date.num<20170407, 3, 
                                ifelse(ehout$date.num>20170408 & ehout$date.num<20170622, 4, 0)
                         )
                  )
)

# cleaning up the rest
#Cleaning Up EHOUT for Usage

#only columns from ehout that are necessary
ehout<-ehout[c("OffenderID","OSS","OSS.Days","IncidentType","MP")]
#only referrals from ehout
ehoutref<-subset(ehout,IncidentType=="referral")
ehoutclass<-subset(ehout,IncidentType=="classroom")
#change OSS Days Name to one word
colnames(ehoutref)[which(names(ehoutref)=="OSS.Days")]<-"OSSDays"

#create a counter for every incident (will use to sum by ID)
ehoutref$inccount = 1
ehoutclass$classcount=1

#Subset by MP
ehoutrefMP1<-subset(ehoutref,MP=="1")
ehoutrefMP2<-subset(ehoutref,MP=="2")
ehoutrefMP3<-subset(ehoutref,MP=="3")
ehoutrefMP4<-subset(ehoutref,MP=="4")
ehoutclassMP1<-subset(ehoutclass,MP=="1")
ehoutclassMP2<-subset(ehoutclass,MP=="2")
ehoutclassMP3<-subset(ehoutclass,MP=="3")
ehoutclassMP4<-subset(ehoutclass,MP=="4")

#sum referrals,OSS,OSS Days by MP

MP1RefOSS<-aggregate(cbind(OSS,OSSDays,inccount)~OffenderID,data=ehoutrefMP1,FUN=sum)
MP2RefOSS<-aggregate(cbind(OSS,OSSDays,inccount)~OffenderID,data=ehoutrefMP2,FUN=sum)
MP3RefOSS<-aggregate(cbind(OSS,OSSDays,inccount)~OffenderID,data=ehoutrefMP3,FUN=sum)
MP4RefOSS<-aggregate(cbind(OSS,OSSDays,inccount)~OffenderID,data=ehoutrefMP4,FUN=sum)

#subset classroom incidents by MP
MP1Class<-aggregate(cbind(classcount)~OffenderID,data=ehoutclassMP1,FUN=sum)
MP2Class<-aggregate(cbind(classcount)~OffenderID,data=ehoutclassMP2,FUN=sum)
MP3Class<-aggregate(cbind(classcount)~OffenderID,data=ehoutclassMP3,FUN=sum)
MP4Class<-aggregate(cbind(classcount)~OffenderID,data=ehoutclassMP4,FUN=sum)

#change all column names
colnames(MP1RefOSS)[which(names(MP1RefOSS)=="inccount")]<-"MP1 Referrals"
colnames(MP1RefOSS)[which(names(MP1RefOSS)=="OSS")]<-"MP1 OSS"
colnames(MP1RefOSS)[which(names(MP1RefOSS)=="OSSDays")]<-"MP1 OSS Days"
colnames(MP1Class)[which(names(MP1Class)=="classcount")]<-"MP1 Classroom Incidents"

colnames(MP2RefOSS)[which(names(MP2RefOSS)=="inccount")]<-"MP2 Referrals"
colnames(MP2RefOSS)[which(names(MP2RefOSS)=="OSS")]<-"MP2 OSS"
colnames(MP2RefOSS)[which(names(MP2RefOSS)=="OSSDays")]<-"MP2 OSS Days"
colnames(MP2Class)[which(names(MP2Class)=="classcount")]<-"MP2 Classroom Incidents"

colnames(MP3RefOSS)[which(names(MP3RefOSS)=="inccount")]<-"MP3 Referrals"
colnames(MP3RefOSS)[which(names(MP3RefOSS)=="OSS")]<-"MP3 OSS"
colnames(MP3RefOSS)[which(names(MP3RefOSS)=="OSSDays")]<-"MP3 OSS Days"
colnames(MP3Class)[which(names(MP3Class)=="classcount")]<-"MP3 Classroom Incidents"

colnames(MP4RefOSS)[which(names(MP4RefOSS)=="inccount")]<-"MP4 Referrals"
colnames(MP4RefOSS)[which(names(MP4RefOSS)=="OSS")]<-"MP4 OSS"
colnames(MP4RefOSS)[which(names(MP4RefOSS)=="OSSDays")]<-"MP4 OSS Days"
colnames(MP4Class)[which(names(MP4Class)=="classcount")]<-"MP4 Classroom Incidents"

#merge all together
ehoutbyMP<-merge(MP1RefOSS,MP1Class,"OffenderID",all=TRUE)
ehoutbyMP<-merge(ehoutbyMP,MP2RefOSS,"OffenderID",all=TRUE)
ehoutbyMP<-merge(ehoutbyMP,MP2Class,"OffenderID",all=TRUE)
ehoutbyMP<-merge(ehoutbyMP,MP3RefOSS,"OffenderID",all=TRUE)
ehoutbyMP<-merge(ehoutbyMP,MP3Class,"OffenderID",all=TRUE)
ehoutbyMP<-merge(ehoutbyMP,MP4RefOSS,"OffenderID",all=TRUE)
ehoutbyMP<-merge(ehoutbyMP,MP4Class,"OffenderID",all=TRUE)

colnames(ehoutbyMP)[which(names(ehoutbyMP)=="OffenderID")]<-"Student ID"
