
###################################################
#########ELEMENTARY SUSPENSIONS YTD AND OVERALL#####
####################################################
###parse dates and create time intervals
library(lubridate)
library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)
library(xlsx)

ehout<-read.csv("ehout all years.csv")
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

#####filter to proper data set
library(plyr)
library(dplyr)
elementarysuspensions<-ehout %>%
  filter(Grade %in% c("1","2","3","4","5","KF","U4","U3","U1","U2","U5"))%>%
  mutate(Ref=1)%>%
  select(SchoolYear,OffenderID,OSS,ISS,Total.Susp.Duration,Ref,RaceEthnicity,IEP,Grade,ytd)%>%
  group_by(SchoolYear,OffenderID,RaceEthnicity,IEP,Grade,ytd) %>%
  summarise(OSSCount=sum(OSS),TotalDays=sum(Total.Susp.Duration),RefCount=sum(Ref),
            ISSCount=sum(ISS))%>%
  mutate(OSS=1)%>%
  mutate(ISS=1)%>%
  mutate(Ref=1)%>%
  group_by(SchoolYear,ytd,RaceEthnicity,IEP,Grade)%>%
  summarise(OSSIncidents=sum(OSSCount),OSSUnique=sum(OSS),TotalDays=sum(TotalDays),
            ISSIncidents=sum(ISSCount),Referrals=sum(RefCount),ISSUnique=sum(ISS))

elementarysuspensions$Grade<-revalue(elementarysuspensions$Grade,c("U2"="2","U1"="1",
                                                                   "U3"="3","U4"="4","U5"="5"))
elementarysuspensions$RaceEthnicity<-revalue(elementarysuspensions$RaceEthnicity, c("P"="AO", "A"="AO", 
                                                    "I" = "AO", "M" = "H"))


YTDelementarysuspensions<-elementarysuspensions %>%
  filter(ytd==1)%>%
  ungroup() %>%
  select(SchoolYear,RaceEthnicity,IEP,Grade,OSSIncidents,OSSUnique,Referrals,ISSIncidents,ISSUnique,TotalDays)

Allelementarysuspensions<-elementarysuspensions %>%
  select(SchoolYear,RaceEthnicity,IEP,Grade,OSSIncidents,OSSUnique,Referrals,ISSIncidents,ISSUnique,TotalDays)

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
  filter(CURR_GRADE_LVL %in% c("1","2","3","4","5","KF"))%>%
  select(SY,STUDENT_ID,CURR_GRADE_LVL,CHALLENGE_TYPE,ETHNIC_DESC)%>%
  rename(SchoolYear=SY)%>%
  rename(Grade=CURR_GRADE_LVL)%>%
  rename(IEP=CHALLENGE_TYPE)%>%
  rename(RaceEthnicity=ETHNIC_DESC)%>%
  mutate(Count=1)%>%
  group_by(SchoolYear,RaceEthnicity,IEP,Grade)%>%
  summarise(TotalStudents=sum(Count))

enroll18<-read.csv("2018eschoolenroll1023.csv")
enroll18$Fed.Race.Ethnic<-revalue(as.character(enroll18$Fed.Race.Ethnic), c("4"="B", "6"="W","2" = "AO", "7" = "AO",
                                                              "3" = "AO","1"="H","5"="AO"))
enroll18$Iep.Status<-revalue(enroll18$Iep.Status,c("None"="0","NONE"="0","CT"="1","SC"="1","Rsrc"="1",
                                                   "DCLS"="0","Dcls"="0"))
enroll18$Grade<-revalue(enroll18$Grade,c("Pre-school"="PS","Pre-Kindergarten (Half day)"="PKH","Pre-Kindergarten (Full day)"="PKF",
                                         "Kindergarten"="KF","First grade"="1","Second grade"="2","Third grade"="3",
                                         "Fourth grade"="4","Fifth grade"="5","Sixth grade"="6","Seventh grade"="7",
                                         "Eighth grade"="8","Ninth grade"="9","Tenth grade"="10","Eleventh grade"="11",
                                         "Twelfth grade"="12","Twelve Plus"="12+","Adult Ed"="A","Ungraded (Kdg)"="KF",
                                         "Ungraded (1st)"="1","Ungraded (2nd)"="2","Ungraded (3rd)"="3","Ungraded (4th)"="4","Ungraded (5th)"="5",
                                       "Ungraded (6th)"="6","Ungraded (7th)"="7","Ungraded (8th)"="8","Ungraded secondary"="12"))

enroll18$Iep.Status<-sub("^$", "0", enroll18$Iep.Status)
                                         
enroll2018<-enroll18 %>%
  filter(Grade %in% c("1","2","3","4","5","KF"))%>%
  mutate(SchoolYear=2018)%>%
  mutate(Count=1)%>%
  rename(STUDENT_ID=Student.ID)%>%
  rename(IEP=Iep.Status)%>%
  rename(RaceEthnicity=Fed.Race.Ethnic)%>%
  select(SchoolYear,STUDENT_ID,Grade,IEP,RaceEthnicity,Count)%>%
  group_by(SchoolYear,RaceEthnicity,IEP,Grade)%>%
  summarise(TotalStudents=sum(Count))

allenroll<-as.data.frame(rbind(enroll2018,beds))


YTD<-merge(allenroll,YTDelementarysuspensions,by=c("RaceEthnicity","SchoolYear","IEP","Grade"),all=TRUE)
allyear<-merge(allenroll,Allelementarysuspensions,by=c("RaceEthnicity","SchoolYear","IEP","Grade"),all=TRUE)
  
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
         "White 2018"=W_2018)

YTDIEPtable<-YTD %>%
  select(SchoolYear,TotalStudents,OSSIncidents,ISSIncidents,Referrals,TotalDays,RaceEthnicity,IEP)%>%
  group_by(SchoolYear,IEP)%>%
  summarise("Total Students"=sum(TotalStudents),"OSS Incidents"=sum(OSSIncidents),"ISS Incidents"=sum(ISSIncidents),
            "Referrals"=sum(Referrals),"Instructional Days Lost"=sum(TotalDays))%>%
  melt(id.vars=c("SchoolYear","IEP"))%>%
  dcast(variable~IEP+SchoolYear)%>%
  rename("Gen. Ed 2014"= "0_2014", "Gen. Ed 2015" ="0_2015", "Gen. Ed 2016"="0_2016","Gen. Ed 2017"= "0_2017",
         "Gen. Ed 2018"="0_2018","IEP 2014"="1_2014","IEP 2015"="1_2015","IEP 2016"="1_2016","IEP 2017"="1_2017",
         "IEP 2018"="1_2018")

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
         "White 2018"=W_2018)

AllYearIEPtable<-allyear %>%
  select(SchoolYear,TotalStudents,OSSIncidents,ISSIncidents,Referrals,TotalDays,RaceEthnicity,IEP)%>%
  group_by(SchoolYear,IEP)%>%
  summarise("Total Students"=sum(TotalStudents),"OSS Incidents"=sum(OSSIncidents),"ISS Incidents"=sum(ISSIncidents),
            "Referrals"=sum(Referrals),"Instructional Days Lost"=sum(TotalDays))%>%
  melt(id.vars=c("SchoolYear","IEP"))%>%
  dcast(variable~IEP+SchoolYear)%>%
  rename("Gen. Ed 2014"= "0_2014", "Gen. Ed 2015" ="0_2015", "Gen. Ed 2016"="0_2016","Gen. Ed 2017"= "0_2017",
         "Gen. Ed 2018"="0_2018","IEP 2014"="1_2014","IEP 2015"="1_2015","IEP 2016"="1_2016","IEP 2017"="1_2017",
         "IEP 2018"="1_2018")

write.xlsx(AllYearIEPtable, "C:/Users/drobil66/Desktop/RFiles/R Reports/AllYearIEPtable.xlsx") 
write.xlsx(AllYearRacetable, "C:/Users/drobil66/Desktop/RFiles/R Reports/AllYearRacetable.xlsx")
write.xlsx(YTDIEPtable, "C:/Users/drobil66/Desktop/RFiles/R Reports/YTDIEPtable.xlsx") 
write.xlsx(YTDRacetable, "C:/Users/drobil66/Desktop/RFiles/R Reports/YTDRacetable.xlsx")