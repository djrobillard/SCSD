old<-read.csv("disciplinethrough2018.csv")
ehout<-read.csv("ehout all years.csv")
library(plyr)
library(dplyr)

cleanold<-old %>%
  mutate(IncidentType=0) %>%
  mutate(IncidentTime=0) %>%
  mutate(TotalSuspension=ISS+OSS)%>%
  mutate(TotalSuspensionDuration=iss_duration+oss_duration)%>%
  mutate(StaffID=0)%>%
  mutate(StaffName=0)%>%
  select(SCHOOL_YEAR,Student_ID,incident_id,IncidentType,incident_date,IncidentTime,
         EnrollmentSchoolID,EnrollmentSchoolName,Inc_Code,IncidentDescription,Student.Name,
         IEP,ELL,Ethnicity,F.R.Lunch,Gender,Grade,OSS,oss_duration,ISS,iss_duration,TotalSuspension,
         TotalSuspensionDuration,)

write.csv(cleanold, "C:/Users/drobil66/Desktop/RFiles/R Reports/cleanold.csv") 
