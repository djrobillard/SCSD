
###################################################
#########PSP Data 2013-2014 SUSPENSIONS YTD AND OVERALL#####
####################################################
###parse dates and create time intervals
library(lubridate)
library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)
library(xlsx)

ehout<-read.csv("ehout all years.csv",strip.white = TRUE)


#####filter to proper data set

disciplinecounts<-ehout %>%
  mutate(Ref=1)%>%
  mutate(SchoolYear=as.numeric(SchoolYear))%>%
  select(SchoolYear,OffenderID,OSS,ISS,OSS.Days, ISS.Days,Total.Suspension,Total.Susp.Duration,
         Ref,RaceEthnicity,IEP)%>%
  group_by(SchoolYear,OffenderID,RaceEthnicity,IEP) %>%
  summarise(OSSCount=sum(OSS),OSSDays=sum(OSS.Days),ISSDays=sum(ISS.Days),TotalSusp=sum(Total.Suspension),
            TotalDays=sum(Total.Susp.Duration),RefCount=sum(Ref),ISSCount=sum(ISS))%>%
  mutate(OSSPlus=ifelse(OSSCount>1,1,0))%>%
  mutate(ISSPlus=ifelse(ISSCount>1,1,0))%>%
  mutate(RefPlus=ifelse(RefCount>1,1,0))%>%
  mutate(OSSOne=ifelse(OSSCount>0,1,0))%>%
  mutate(ISSOne=ifelse(ISSCount>0,1,0))%>%
  mutate(RefOne=ifelse(RefCount>01,1,0))%>%
  group_by(SchoolYear,RaceEthnicity,IEP)%>%
  summarise(OSSIncidents=sum(OSSCount),OSSUnique=sum(OSSOne),TotalDays=sum(TotalDays),OSSDays=sum(OSSDays),
            ISSDays=sum(ISSDays),TotalSusp=sum(TotalSusp),ISSIncidents=sum(ISSCount),Referrals=sum(RefCount),
            ISSUnique=sum(ISSOne),MultOSS=sum(OSSPlus),MultISS=sum(ISSPlus),MultRef=sum(RefPlus),RefUnique=sum(RefOne))%>%
  filter(SchoolYear!=2008)%>%
  filter(SchoolYear!=2009)%>%
  filter(SchoolYear!=2010)%>%
  filter(SchoolYear!=2011)%>%
  mutate(IEP=as.character(IEP))
  
disciplinecounts$RaceEthnicity<-revalue(disciplinecounts$RaceEthnicity,c("P"="A"))


###########################enrollment##################################
allbeds<-read.csv("combinedBEDS2012-2017.csv",strip.white = TRUE)
allbeds$CHALLENGE_TYPE<-revalue(allbeds$CHALLENGE_TYPE,c("Learning Disability"="1", "Other Health Impairment"="1", 
                                                         "Intellectual Disability" = "1", "Autism" = "1","Multiple Disabilities" = "1", "Emotional Disturbance" = "1",
                                                         "Speech or Language Impairment"="1", "Traumatic Brain Injury"="1","Orthopedic Impairment" = "1",
                                                         "Pre-School Student with a Disability" = "1","Visual Impairment (includes Blindness)" = "1",
                                                         "Hearing Impairment" = "1","Deaf-Blindness"="1"))

allbeds$CHALLENGE_TYPE<- sub("^$", "0", allbeds$CHALLENGE_TYPE)
allbeds$ETHNIC_DESC<-revalue(allbeds$ETHNIC_DESC, c("Black or African American"="B", "White"="W", 
                                                    "American Indian or Alaska Native" = "I", "Hispanic or Latino" = "H",
                                                    "Multiracial" = "M", "Asian or Pacific Islander" = "A"))

beds<-allbeds %>%
  select(SY,STUDENT_ID,CURR_GRADE_LVL,CHALLENGE_TYPE,ETHNIC_DESC)%>%
  rename(SchoolYear=SY)%>%
  rename(Grade=CURR_GRADE_LVL)%>%
  rename(IEP=CHALLENGE_TYPE)%>%
  rename(RaceEthnicity=ETHNIC_DESC)%>%
  mutate(Count=1)%>%
  group_by(SchoolYear,RaceEthnicity,IEP)%>%
  summarise(TotalStudents=sum(Count))

enroll18<-read.csv("CurrentEnrollment.csv",strip.white = TRUE)
enroll18$Rptg.Race.Ethnicity.Desc<-revalue(enroll18$Rptg.Race.Ethnicity.Desc, c("Black or African American"="B", "White"="W", 
                                                    "American Indian or Alaska native" = "I", "Hispanic" = "H",
                                                    "Multiracial" = "M", "Asian" = "A","Native Hawaiian / Other Pacific Islander"="A"))

enroll18$Iep.Status<-revalue(enroll18$Iep.Status,c("None"="0","NONE"="0","CT"="1","SC"="1","Rsrc"="1",
                                                   "DCLS"="0","Dcls"="0"))

enroll2018<-enroll18 %>%
  mutate(SchoolYear=2018)%>%
  mutate(Count=1)%>%
  rename(STUDENT_ID=Student.Id)%>%
  rename(RaceEthnicity=Rptg.Race.Ethnicity.Desc)%>%
  select(SchoolYear,STUDENT_ID,IEP,RaceEthnicity,Count)%>%
  group_by(SchoolYear,RaceEthnicity,IEP)%>%
    summarise(TotalStudents=sum(Count)) %>%
    mutate(IEP=as.character(IEP))

allenroll<-as.data.frame(rbind(enroll2018,beds))

Alldata<-merge(allenroll,disciplinecounts,by=c("SchoolYear","RaceEthnicity","IEP"),all=TRUE)


write.csv(Alldata, "C:/Users/drobil66/Desktop/RFiles/R Reports/AllData.csv") 
