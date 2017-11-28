library(plyr)
library(dplyr)


#split ehoutallyears into files by year
ehoutall<-read.csv("ehout all years.csv")

ehout2014<-subset(ehoutall,SchoolYear=="2014")
ehout2015<-subset(ehoutall,SchoolYear=="2015")
ehout2016<-subset(ehoutall,SchoolYear=="2016")
ehout2017<-subset(ehoutall,SchoolYear=="2017")


ehout2014<-ehout2014[!(is.na(ehout2014$IncidentID)),]
ehout2015<-ehout2015[!(is.na(ehout2015$IncidentID)),]
ehout2016<-ehout2016[!(is.na(ehout2016$IncidentID)),]

ehout2014[,26][is.na(ehout2014[,26])]<-0
ehout2015[,26][is.na(ehout2015[,26])]<-0
ehout2016[,26][is.na(ehout2016[,26])]<-0

#### cleaning up new ehout2017

ehout2017<-read.csv("ehout2017.csv") ##updated ehoutfile
ehout2017$SchoolYear<-2017
ehout2017<-filter(ehout2017,IncidentType=="referral")
ehout2017<-rename(ehout2017,IncidentCode=EH.Incident.Code)
ehout2017<-select(ehout2017,SchoolYear,OffenderID,IncidentID,IncidentDate,SchoolCode,SchoolName,IncidentCode,Incident.Description,OffenderName,IEP,ELL,RaceEthnicity,FRPL,Gender,Grade,OSS,OSS.Days,ISS,ISS.Days,Total.Suspension,Total.Susp.Duration,Written.By.ID,Written.By,Location)


##combining all files together
ehoutallyears<-rbind(ehout2014,ehout2015)
ehoutallyears<-rbind(ehoutallyears,ehout2016)
ehoutallyears<-rbind(ehoutallyears,ehout2017)

write.csv(ehoutallyears,"C:/Users/drobil66/Desktop/RFiles/R Extracts/ehout all years.csv")