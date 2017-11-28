
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

#####filter to proper data set
library(plyr)
library(dplyr)
elementarysuspensions<-ehout %>%
  mutate(Ref=1)%>%
  select(SchoolYear,OffenderID,OSS,ISS,Total.Susp.Duration,Ref,RaceEthnicity,IEP,SchoolName)%>%
  group_by(SchoolYear,OffenderID,RaceEthnicity,IEP,SchoolName) %>%
  summarise(OSSCount=sum(OSS),TotalDays=sum(Total.Susp.Duration),RefCount=sum(Ref),
            ISSCount=sum(ISS))%>%
  mutate(OSS=1)%>%
  mutate(ISS=1)%>%
  mutate(Ref=1)%>%
  group_by(SchoolYear,RaceEthnicity,IEP,SchoolName)%>%
  summarise(OSSIncidents=sum(OSSCount),OSSUnique=sum(OSS),TotalDays=sum(TotalDays),
            ISSIncidents=sum(ISSCount),Referrals=sum(RefCount),ISSUnique=sum(ISS),RefUnique=sum(Ref))

elementarysuspensions$RaceEthnicity<-revalue(elementarysuspensions$RaceEthnicity, c("P"="A"))

Allsuspensions<-elementarysuspensions %>%
  ungroup() %>%
  select(SchoolYear,SchoolName,RaceEthnicity,IEP,OSSIncidents,OSSUnique,Referrals,ISSIncidents,ISSUnique,RefUnique,TotalDays)

###########################enrollment##################################
allbeds<-read.csv("combinedBEDS2014-2017.csv")
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
  select(SY,STUDENT_ID,CURR_GRADE_LVL,CHALLENGE_TYPE,ETHNIC_DESC,LOCATION_NAME)%>%
  rename(SchoolYear=SY)%>%
  rename(SchoolName=LOCATION_NAME)%>%
  rename(IEP=CHALLENGE_TYPE)%>%
  rename(RaceEthnicity=ETHNIC_DESC)%>%
  mutate(Count=1)%>%
  group_by(SchoolYear,RaceEthnicity,IEP,SchoolName)%>%
  summarise(TotalStudents=sum(Count))

beds$SchoolName<-revalue(beds$SchoolName, c(
  "BELLEVUE ELEMENTARY SCHOOL"="Bellevue Elementary School",
  "CLARY MIDDLE SCHOOL"="Clary Middle School",
  "CORCORAN HIGH SCHOOL" = "Corcoran High School",
  "DANFORTH MIDDLE SCHOOL" = "Danforth Middle School",
  "DELAWARE ACADEMY" = "Delaware Academy",
  "DELAWARE PRIMARY SCHOOL" = "Delaware Primary School",
  "DR WEEKS ELEMENTARY SCHOOL" ="Dr. Weeks Elementary School", 
  "DR KING ELEMENTARY SCHOOL" = "Dr. King Elementary School",
  "EDWARD SMITH K-8 SCHOOL" = "Edward Smith K-8",
  "EXPEDITIONARY LEARNING MIDDLE SCH"="Expeditionary Learning Middle School",
  "FOWLER HIGH SCHOOL"="Fowler High School", 
  "FRANKLIN ELEMENTARY SCHOOL" = "Franklin Elementary School",
  "FRAZER K-8 SCHOOL" = "Frazer PK-8",
  "GRANT MIDDLE SCHOOL" = "Grant Middle School",
  "HENNINGER HIGH SCHOOL" = "Henninger High School",
  "HUGHES ELEMENTARY SCHOOL"="Hughes Elementary School",
  "HUNTINGTON K-8 SCHOOL"="Huntington PK-8",
  "HURLBUT W SMITH K-8 SCHOOL" = "H.W. Smith PK-8",
  "INSTITUTE OF TECH AT SYRACUSE CENTRA" = "Institute of Technology at Syracuse Central",
  "LEMOYNE ELEMENTARY SCHOOL" = "Lemoyne Elementary School",
  "LINCOLN MIDDLE SCHOOL" = "Lincoln Middle School",
  "MCKINLEY-BRIGHTON ELEMENTARY"="McKinley-Brighton Elementary School",
  "MEACHEM ELEMENTARY SCHOOL"="Meachem Elementary School", 
  "NOTTINGHAM HIGH SCHOOL" = "Nottingham High School",
  "PORTER ELEMENTARY SCHOOL" = "Porter Elementary School",
  "PUBLIC SERVICE LEADERSHIP ACA-FOWLER" = "Public Service Leadership Academy at Fowler",
  "ROBERTS K-8 SCHOOL" = "Roberts PK-8",
  "SALEM HYDE ELEMENTARY SCHOOL"="Salem Hyde Elementary School",
  "SEYMOUR DUAL LANGUAGE ACADEMY"="Seymour Dual Language Academy",
  "SYRACUSE LATIN SCHOOL" = "Syracuse Latin School",
  "VAN DUYN ELEMENTARY SCHOOL" = "Van Duyn Elementary School",
  "WEBSTER ELEMENTARY SCHOOL" = "Webster Elementary School",
  "WESTSIDE ACADEMY AT BLODGETT" = "Westside Academy at Blodgett",
  "PK - SYRACUSE CITY SD"="UPK"
))

beds$SchoolName<-trimws(beds$SchoolName)




allyear<-merge(beds,Allsuspensions,by=c("RaceEthnicity","SchoolYear","IEP","SchoolName"),all=TRUE)


write.xlsx(allyear, "C:/Users/drobil66/Desktop/RFiles/R Reports/allyear.xlsx") 
