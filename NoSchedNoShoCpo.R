library(tidyverse)
enrollment<-read.csv("Currentenrollment.csv")
Nosched<-read.csv("NoSched.csv")
enrollment1<-enrollment %>%
  mutate(CPO=ifelse(Student.Building=="Building 555",1,0))%>%
  mutate(NoSho=ifelse(Home.Room=="NOSHO",1,0))%>%
  select(Student.Id,Student.Name,Student.Gender,Rptg.Race.Ethnicity.Desc,IEP,Student.Building,ï..Accountability.Building,CPO,NoSho)

Nosched<-Nosched %>%
  select(Student.Id)%>%
  mutate(NoSched=1)
  
  

Combined<-merge(enrollment1,Nosched,by="Student.Id",all=TRUE)
Combined<-Combined %>%
  replace(.,is.na(.),0)%>%
  mutate(Keep=ifelse(NoSched+NoSho+CPO>0,1,0))%>%
  filter(Keep==1)%>%
  select(Student.Id,Student.Name,Student.Gender,Rptg.Race.Ethnicity.Desc,IEP,
         Student.Building,ï..Accountability.Building,CPO,NoSho,NoSched)%>%
  rename(Race=Rptg.Race.Ethnicity.Desc,AccountabilityBuilding=ï..Accountability.Building,
         EnrollmentBuilding=Student.Building)