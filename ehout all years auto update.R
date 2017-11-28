library(dplyr)
library(readxl)
library(lubridate)
ehout2014<-read.csv(file = "Y:/Data_Repository/Discipline/Discipline Archive/Final 2014 Referral Data.csv")
ehout2015<-read.csv(file = "Y:/Data_Repository/Discipline/Discipline Archive/Final 2015 Referral Data.csv")
ehout2016<-read.csv(file = "Y:/Data_Repository/Discipline/Discipline Archive/Final 2016 Referral Data.csv")

#ehout.all<-read.csv(file = "Z:/Data_Repository/Discipline/Discipline Archive/ehout all years.csv")
ehout2014$Incident_Date<-mdy(ehout2014$Incident_Date)
ehout2014b<- ehout2014 %>%
  mutate(Total.Susp.Duration = iss_duration + oss_duration, Total.Suspension = ISS + OSS) %>%
  rename(SchoolYear = SCHOOL_YEAR, OffenderID = Student_ID, IncidentID = incident_id, IncidentDate = Incident_Date,
         SchoolCode = IncidentSchoolID, SchoolName = IncidentSchoolName, IncidentCode = Inc_Code, Incident.Description = IncidentDescr1,
         OffenderName = Student.Name, FRPL = F.R.Lunch, ISS.Days = iss_duration, OSS.Days = oss_duration,
         Written.By.ID = ReferredByID, Written.By = ReferredBy, RaceEthnicity = Ethnicity) %>%
  filter(SummerSchool == "N") %>%
  select(SchoolYear, OffenderID, IncidentID, IncidentDate, SchoolCode, SchoolName,
         IncidentCode, Incident.Description, OffenderName, IEP,
         ELL, RaceEthnicity, FRPL, Gender,
         Grade, OSS, OSS.Days, ISS,
         ISS.Days, Total.Suspension, Total.Susp.Duration, Written.By.ID, Written.By, Location)


ehout2015$IncidentDate<-mdy(ehout2015$IncidentDate)
ehout2015b <- ehout2015 %>%
  mutate( SchoolYear = 2015) %>%
  rename(IncidentCode = EH.Incident.Code...Final) %>%
  select(SchoolYear, OffenderID, IncidentID, IncidentDate, SchoolCode, SchoolName,
         IncidentCode, Incident.Description, OffenderName, IEP,
         ELL, RaceEthnicity, FRPL, Gender,
         Grade, OSS, OSS.Days, ISS,
         ISS.Days, Total.Suspension, Total.Susp.Duration, Written.By.ID, Written.By, Location)

ehout2016$IncidentDate<-mdy(ehout2016$IncidentDate)
ehout2016b <- ehout2016 %>%
  mutate(SchoolYear = 2016) %>%
  rename(IncidentCode = EH.Incident.Code) %>%
  select(SchoolYear, OffenderID, IncidentID, IncidentDate, SchoolCode, SchoolName,
         IncidentCode, Incident.Description, OffenderName, IEP,
         ELL, RaceEthnicity, FRPL, Gender,
         Grade, OSS, OSS.Days, ISS,
         ISS.Days, Total.Suspension, Total.Susp.Duration, Written.By.ID, Written.By, Location)


ehout.all<-rbind(ehout2014b, ehout2015b, ehout2016b)

library(readxl)
ehout2017<-read_excel("Y:/Data_Repository/Discipline/ehout2017.xlsx", sheet = 1)
ehout2017$IncidentDate<-ymd(ehout2017$IncidentDate)

ehout2017b<- ehout2017 %>%
  mutate(SchoolYear = 2017) %>%
  filter(IncidentType == "referral")

colnames(ehout2017b)[colnames(ehout2017b)=="EH Incident Code"]<-"IncidentCode"
colnames(ehout2017b)[colnames(ehout2017b)=="Incident Description"]<-"Incident.Description"
colnames(ehout2017b)[colnames(ehout2017b)=="OSS Days"]<-"OSS.Days"
colnames(ehout2017b)[colnames(ehout2017b)=="ISS Days"]<-"ISS.Days"
colnames(ehout2017b)[colnames(ehout2017b)=="ISS Days"]<-"ISS.Days"
colnames(ehout2017b)[colnames(ehout2017b)=="Total Suspension"]<-"Total.Suspension"
colnames(ehout2017b)[colnames(ehout2017b)=="Total Susp Duration"]<-"Total.Susp.Duration"
colnames(ehout2017b)[colnames(ehout2017b)=="Written By ID"]<-"Written.By.ID"
colnames(ehout2017b)[colnames(ehout2017b)=="Written By"]<-"Written.By"

ehout2017b<- ehout2017b %>%
  select(SchoolYear, OffenderID, IncidentID, IncidentDate, SchoolCode, SchoolName,
         IncidentCode, Incident.Description, OffenderName, IEP,
         ELL, RaceEthnicity, FRPL, Gender,
         Grade, OSS, OSS.Days, ISS,
         ISS.Days, Total.Suspension, Total.Susp.Duration, Written.By.ID, Written.By, Location)


ehout.all<-rbind(ehout.all, ehout2017b)
write.csv(ehout.all, "C:/Users/drobil66/Desktop/RFiles/R Reports/ehout all years.csv")
