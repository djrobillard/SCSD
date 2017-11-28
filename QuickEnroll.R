library(tidyverse)

enroll<-read.csv("CurrentEnrollment.csv")
enroll1<- enroll %>%
  select(Student.Id, Student.Name,Curr.Grade.Lvl,IEP,Rptg.Race.Ethnicity.Desc,Student.Building.Num,Student.Building)%>%
  filter(Student.Building.Num %in% c("602","604","605","606","607"))

  filter( %in% c(2017))%>%
  filter(EnrollmentSchoolID %in% c(29,24))%>%
  mutate(Name=paste(LastName, FirstName,sep=', ')) %>%
  select(SCHOOL_YEAR,STUDENT_ID,Name,EnrollmentSchoolName,ELL)%>%
  filter(ELL==1)
  
  
  write.csv(enroll1, "C:/Users/drobil66/Desktop/RFiles/R Reports/Enroll1.csv")



altenroll<-filter(enroll,EnrollmentSchoolID==c(
  "47","48","52","54","602","603","604","605","606","607","608","646",
  "647","670","777","780"))