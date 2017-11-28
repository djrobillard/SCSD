#this file automatically updates ehout all years using the current files in the data repository

#it will temporarily set your working directory to the accountability share so that you can quickly pull in the necessary files

#you need packages plyr, dplyr and readxl (because the current ehout file is an xlsx file)

library(readxl)
library(plyr)
library(dplyr)
library(triebeard)
library(rio)

#pull in files that you need
setwd('Y:/Data_Repository/Discipline')

ehout2017<-import("ehout2017.xlsx")
convert("ehout2017.xlsx","ehout2017.csv")
ehoutallyears<-read.csv("./Discipline Archive/ehout all years.csv")

#breaking up old version of ehoutallyears
ehout2014<-subset(ehoutallyears,SchoolYear=="2014")
ehout2015<-subset(ehoutallyears,SchoolYear=="2015")
ehout2016<-subset(ehoutallyears,SchoolYear=="2016")

#cleaning up ehout2017 to match
ehout2017$SchoolYear<-2017
#cleaning up column names
colnames(ehout2017)[which(names(ehout2017)=="EH Incident Code")]<-"EH.Incident.Code...Final"
colnames(ehout2017)[which(names(ehout2017)=="Incident Description")]<-"Incident.Description"
colnames(ehout2017)[which(names(ehout2017)=="OSS Days")]<-"OSS.Days"
colnames(ehout2017)[which(names(ehout2017)=="ISS Days")]<-"ISS.Days"
colnames(ehout2017)[which(names(ehout2017)=="Total Suspension")]<-"Total.Suspension"
colnames(ehout2017)[which(names(ehout2017)=="Total Susp Duration")]<-"Total.Susp.Duration"
colnames(ehout2017)[which(names(ehout2017)=="Written By ID")]<-"Written.By.ID"
colnames(ehout2017)[which(names(ehout2017)=="Written By")]<-"Written.By"
colnames(ehout2017)[which(names(ehout2017)=="504Status")]<-"x504Status"



ehout2017<-select(ehout2017,SchoolYear,OffenderID,IncidentID,IncidentType,IncidentDate,IncidentTime,SchoolCode,SchoolName,EH.Incident.Code...Final,Incident.Description,OffenderName,IEP,ELL,RaceEthnicity,FRPL,Gender,Grade,OSS,OSS.Days,ISS,ISS.Days,Total.Suspension,Total.Susp.Duration,Written.By.ID,Written.By,x504Status)

##combining all files together
ehoutallyears<-rbind(ehout2014,ehout2015)
ehoutallyears<-rbind(ehoutallyears,ehout2016)
ehoutallyears<-rbind(ehoutallyears,ehout2017)
