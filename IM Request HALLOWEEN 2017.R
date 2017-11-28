
###################################################
#########K-12 SUSPENSIONS YTD AND OVERALL#####
####################################################
###parse dates and create time intervals
library(lubridate)
library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)
library(xlsx)

ehout<-read.csv("ehout all years.csv",strip.white = TRUE)
ehout$date <- mdy(ehout$IncidentDate) # puts into date format
ehout$year<-year(ehout$date) # parse year
ehout$month<-month(ehout$date) # parse month
ehout$month<-sprintf("%02d",ehout$month) # create 2 digit month
ehout$day<-day(ehout$date) # parse day
ehout$day<-sprintf("%02d",ehout$day) # create 2 digit day

ehout$date.num <- as.numeric(paste0(ehout$year, ehout$month, ehout$day)) # concatenate

#ytd calculations
ehout$ytd<-ifelse(ehout$date.num>20130901 & ehout$date.num<20131015, 1, 
                  ifelse(ehout$date.num>20140901 & ehout$date.num<20141015, 1, 
                         ifelse(ehout$date.num>20150901 & ehout$date.num<20151015, 1, 
                                ifelse(ehout$date.num>20160821 & ehout$date.num<20161015, 1,
                                       ifelse(ehout$date.num>20170821 & ehout$date.num<20171015,1,0)
                                )
                         )
                  ))

ehout$RaceEthnicity<-revalue(ehout$RaceEthnicity, c("P"="AO", "A"="AO","I" = "AO", "M" = "AO"))

#####filter to proper data set
discipline<-ehout %>%
  filter(ytd==1)%>%
  mutate(Ref=1)%>%
  select(SchoolYear,OffenderID,OSS,ISS,OSS.Days,ISS.Days,Total.Susp.Duration,Ref,RaceEthnicity,IEP,ytd)%>%
  group_by(SchoolYear,OffenderID,RaceEthnicity,IEP) %>%
  summarise(OSSCount=sum(OSS),OSSDays=sum(OSS.Days),TotalDays=sum(Total.Susp.Duration),RefCount=sum(Ref),
            ISSCount=sum(ISS))%>%
  mutate(OSSOne=ifelse(OSSCount>0,1,0))%>%
  mutate(ISSOne=ifelse(ISSCount>0,1,0))%>%
  group_by(SchoolYear,RaceEthnicity,IEP)%>%
  summarise(OSSIncidents=sum(OSSCount),OSSDays=sum(OSSDays),OSSUnique=sum(OSSOne),ISSUnique=sum(ISSOne),
            TotalDays=sum(TotalDays),ISSIncidents=sum(ISSCount),Referrals=sum(RefCount))

YTDdiscipline<-discipline %>%
  filter(ytd==1)%>%
  ungroup() %>%
  select(SchoolYear,RaceEthnicity,IEP,OSSIncidents,OSSDays,OSSUnique,Referrals,ISSIncidents,ISSUnique,TotalDays)

#######all year elementary suspensions#################
Allyearelementarysuspensions<-ehout %>%
  select(SchoolYear,OffenderID,OSS,ISS,Total.Susp.Duration,RaceEthnicity,IEP)%>%
  mutate(Ref=1)%>%
  group_by(SchoolYear,OffenderID,RaceEthnicity,IEP) %>%
  summarise(OSSCount=sum(OSS),TotalDays=sum(Total.Susp.Duration),RefCount=sum(Ref),
            ISSCount=sum(ISS))%>%
  mutate(OSS=1)%>%
  mutate(ISS=1)%>%
  mutate(Ref=1)%>%
  group_by(SchoolYear,RaceEthnicity,IEP)%>%
  summarise(OSSIncidents=sum(OSSCount),OSSUnique=sum(OSS),TotalDays=sum(TotalDays),
            ISSIncidents=sum(ISSCount),Referrals=sum(RefCount),ISSUnique=sum(ISS))%>%
  select(SchoolYear,RaceEthnicity,IEP,OSSIncidents,OSSUnique,Referrals,ISSIncidents,ISSUnique,TotalDays)


###########################enrollment##################################
allbeds<-read.csv("combinedBEDS2014-2017.csv")
allbeds$CHALLENGE_TYPE<-revalue(allbeds$CHALLENGE_TYPE,c("Learning Disability"="1", "Other Health Impairment"="1", 
                                                         "Intellectual Disability" = "1", "Autism" = "1","Multiple Disabilities" = "1", "Emotional Disturbance" = "1",
                                                         "Speech or Language Impairment"="1", "Traumatic Brain Injury"="1","Orthopedic Impairment" = "1",
                                                         "Pre-School Student with a Disability" = "1","Visual Impairment (includes Blindness)" = "1",
                                                         "Hearing Impairment" = "1","Deaf-Blindness"="1"))

allbeds$CHALLENGE_TYPE<- sub("^$", "0", allbeds$CHALLENGE_TYPE)
allbeds$ETHNIC_DESC<-revalue(allbeds$ETHNIC_DESC, c("Black or African American"="B", "White"="W", 
                                                    "American Indian or Alaska Native" = "AO", "Hispanic or Latino" = "H",
                                                    "Multiracial" = "AO", "Asian or Pacific Islander" = "AO"))

beds<-allbeds %>%
  select(SY,STUDENT_ID,CHALLENGE_TYPE,ETHNIC_DESC)%>%
  rename(SchoolYear=SY)%>%
  rename(IEP=CHALLENGE_TYPE)%>%
  rename(RaceEthnicity=ETHNIC_DESC)%>%
  mutate(Count=1)%>%
  group_by(SchoolYear,RaceEthnicity,IEP)%>%
  summarise(TotalStudents=sum(Count))

enroll18<-read.csv("CurrentEnrollment.csv")
enroll18$Rptg.Race.Ethnicity.Desc<-revalue(enroll18$Rptg.Race.Ethnicity.Desc, c("Black or African American"="B", "White"="W", 
                                          "American Indian or Alaska native" = "AO", "Hispanic" = "H",
                                          "Multiracial" = "AO", "Asian" = "AO",
                                          "Native Hawaiian / Other Pacific Islander" = "AO"))

enroll2018<-enroll18 %>%
  mutate(SchoolYear=2018)%>%
  mutate(Count=1)%>%
  rename(STUDENT_ID=Student.Id)%>%
  rename(RaceEthnicity=Rptg.Race.Ethnicity.Desc)%>%
  select(SchoolYear,STUDENT_ID,IEP,RaceEthnicity,Count)%>%
  group_by(SchoolYear,RaceEthnicity,IEP)%>%
  summarise(TotalStudents=sum(Count))%>%
  mutate(IEP=as.character(IEP))

allenroll<-as.data.frame(rbind(enroll2018,beds))


YTD<-merge(allenroll,YTDelementarysuspensions,by=c("RaceEthnicity","SchoolYear","IEP"),all=TRUE)
allyear<-merge(allenroll,Allyearelementarysuspensions,by=c("RaceEthnicity","SchoolYear","IEP"),all=TRUE)

YTD[is.na(YTD)]<-0
allyear[is.na(allyear)]<-0

YTDRacetable<-YTD %>%
  select(SchoolYear,TotalStudents,OSSIncidents,ISSIncidents,Referrals,TotalDays,RaceEthnicity,IEP)%>%
  group_by(SchoolYear,RaceEthnicity)%>%
  summarise("Total Students"=sum(TotalStudents),"OSS Incidents"=sum(OSSIncidents),"ISS Incidents"=sum(ISSIncidents),
            "Referrals"=sum(Referrals),"Instructional Days Lost"=sum(TotalDays))%>%
  melt(id.vars=c("SchoolYear","RaceEthnicity"))%>%
  dcast(variable~RaceEthnicity+SchoolYear)%>%
  rename("Other 2014"= AO_2014, "Other 2015" =AO_2015, "Other 2016"=AO_2016,"Other 2017"= AO_2017,
         "Other 2018"=AO_2018,"Black 2014"=B_2014,"Black 2015"=B_2015,"Black 2016"=B_2016,"Black 2017"=B_2017,
         "Black 2018"="B_2018","Hispanic 2014"=H_2014,"Hispanic 2015"=H_2015,"Hispanic 2016"=H_2016,"Hispanic 2017"=H_2017,
         "Hispanic 2018"=H_2018,"White 2014"=W_2014,"White 2015"=W_2015,"White 2016"=W_2016,"White 2017"=W_2017,
         "White 2018"=W_2018)%>%
  select("variable","Black 2014","Black 2017","Black 2018","Hispanic 2014","Hispanic 2017","Hispanic 2018",
         "White 2014","White 2017","White 2018","Other 2014","Other 2017","Other 2018")

YTDIEPtable<-YTD %>%
  select(SchoolYear,TotalStudents,OSSIncidents,ISSIncidents,Referrals,TotalDays,RaceEthnicity,IEP)%>%
  group_by(SchoolYear,IEP)%>%
  summarise("Total Students"=sum(TotalStudents),"OSS Incidents"=sum(OSSIncidents),"ISS Incidents"=sum(ISSIncidents),
            "Referrals"=sum(Referrals),"Instructional Days Lost"=sum(TotalDays))%>%
  melt(id.vars=c("SchoolYear","IEP"))%>%
  dcast(variable~IEP+SchoolYear)%>%
  rename("Gen. Ed 2014"= "0_2014", "Gen. Ed 2015" ="0_2015", "Gen. Ed 2016"="0_2016","Gen. Ed 2017"= "0_2017",
         "Gen. Ed 2018"="0_2018","IEP 2014"="1_2014","IEP 2015"="1_2015","IEP 2016"="1_2016","IEP 2017"="1_2017",
         "IEP 2018"="1_2018")%>%
  mutate("Total 2014"=`Gen. Ed 2014`+`IEP 2014`)%>%
  mutate("Total 2017"=`Gen. Ed 2017` + `IEP 2017`)%>%
  mutate("Total 2018"=`Gen. Ed 2018` + `IEP 2018`)%>%
  select("variable","Total 2014","Total 2017","Total 2018","IEP 2014","IEP 2017","IEP 2018")

AllYearRacetable<-allyear %>%
  select(SchoolYear,TotalStudents,OSSIncidents,ISSIncidents,Referrals,TotalDays,RaceEthnicity,IEP)%>%
  group_by(SchoolYear,RaceEthnicity)%>%
  summarise("Total Students"=sum(TotalStudents),"OSS Incidents"=sum(OSSIncidents),"ISS Incidents"=sum(ISSIncidents),
            "Referrals"=sum(Referrals),"Instructional Days Lost"=sum(TotalDays))%>%
  melt(id.vars=c("SchoolYear","RaceEthnicity"))%>%
  dcast(variable~RaceEthnicity+SchoolYear)%>%
  rename("Other 2014"= AO_2014, "Other 2015" =AO_2015, "Other 2016"=AO_2016,"Other 2017"= AO_2017,
         "Other 2018"=AO_2018,"Black 2014"=B_2014,"Black 2015"=B_2015,"Black 2016"=B_2016,"Black 2017"=B_2017,
         "Black 2018"=B_2018,"Hispanic 2014"=H_2014,"Hispanic 2015"=H_2015,"Hispanic 2016"=H_2016,"Hispanic 2017"=H_2017,
         "Hispanic 2018"=H_2018,"White 2014"=W_2014,"White 2015"=W_2015,"White 2016"=W_2016,"White 2017"=W_2017,
         "White 2018"=W_2018)%>%
  select("variable","Black 2014","Black 2016","Black 2017","Hispanic 2014","Hispanic 2016","Hispanic 2017",
         "White 2014","White 2016","White 2017","Other 2014","Other 2016","Other 2017")

AllYearIEPtable<-allyear %>%
  select(SchoolYear,TotalStudents,OSSIncidents,ISSIncidents,Referrals,TotalDays,RaceEthnicity,IEP)%>%
  group_by(SchoolYear,IEP)%>%
  summarise("Total Students"=sum(TotalStudents),"OSS Incidents"=sum(OSSIncidents),"ISS Incidents"=sum(ISSIncidents),
            "Referrals"=sum(Referrals),"Instructional Days Lost"=sum(TotalDays))%>%
  melt(id.vars=c("SchoolYear","IEP"))%>%
  dcast(variable~IEP+SchoolYear)%>%
  rename("Gen. Ed 2014"= "0_2014", "Gen. Ed 2015" ="0_2015", "Gen. Ed 2016"="0_2016","Gen. Ed 2017"= "0_2017",
         "Gen. Ed 2018"="0_2018","IEP 2014"="1_2014","IEP 2015"="1_2015","IEP 2016"="1_2016","IEP 2017"="1_2017",
         "IEP 2018"="1_2018")%>%
  mutate("Total 2014"=`Gen. Ed 2014`+`IEP 2014`)%>%
  mutate("Total 2016"=`Gen. Ed 2016` + `IEP 2016`)%>%
  mutate("Total 2017"=`Gen. Ed 2017` + `IEP 2017`)%>%
  select("variable","Total 2014","Total 2016","Total 2017","IEP 2014","IEP 2016","IEP 2017")

Allyeartable<-merge(AllYearIEPtable,AllYearRacetable,by="variable")
Allyeartable<-select(Allyeartable,"variable","Total 2014","Total 2016","Total 2017","Black 2014","Black 2016","Black 2017",
                     "Hispanic 2014","Hispanic 2016","Hispanic 2017","White 2014","White 2016",
                     "White 2017","Other 2014","Other 2016","Other 2017","IEP 2014","IEP 2016","IEP 2017")

YTDtable<-merge(YTDIEPtable,YTDRacetable,by="variable")
YTDtable<-select(YTDtable,"variable","Total 2014","Total 2017","Total 2018","Black 2014","Black 2017",
                 "Black 2018","Hispanic 2014","Hispanic 2017","Hispanic 2018","White 2014","White 2017",
                 "White 2018","Other 2014","Other 2017","Other 2018","IEP 2014","IEP 2017","IEP 2018")

  

write.csv(discipline, "C:/Users/drobil66/Desktop/disciplineYTD.csv") 
write.xlsx(Allyeartable, "C:/Users/drobil66/Desktop/RFiles/R Reports/Allyeartable.xlsx"


write.xlsx(YTD, "C:/Users/drobil66/Desktop/RFiles/R Reports/YTD.xlsx") 
write.xlsx(allyear, "C:/Users/drobil66/Desktop/RFiles/R Reports/Allyear.xlsx")