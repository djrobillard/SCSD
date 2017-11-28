library(plyr)
library(dplyr)
library(scales)
library(xlsx)
library(lubridate)
library(eeptools)

attendance<-read.csv("attendance2018.csv")
attendance<-attendance %>%
  mutate(date=as.character(Cal.Date))%>%
  mutate(date=format(as.POSIXct(date,format='%m/%d/%Y'),format='%m/%d/%Y'))%>%
  mutate(date=mdy(date))%>%
  mutate(year=year(date))%>%
  mutate(month=month(date))%>%
  mutate(month=sprintf("%02d",month))%>%
  mutate(day=day(date))%>%
  mutate(day=sprintf("%02d",day))%>%
  mutate(date.num=as.numeric(paste0(year,month,day)))%>%
  mutate(TwoWeeks=ifelse(date.num>20171106 & date.num<20171122, 1, 0))%>%
  mutate(YTD=1)%>%
  mutate(Day=1)%>%
  mutate(Pres=ifelse(Attendance.Desc=="Present",1,0))%>%
  mutate(TwoWeekDay=ifelse(TwoWeeks==1,1,0))%>%
  mutate(TwoWeekPres=ifelse(TwoWeeks==1 & Pres==1,1,0))%>%
  group_by(Student.Id,Attendance.Location.Name) %>%
  summarise(TotalEnrolled=sum(Day),DaysPresent=sum(Pres),TwoWeekDays=sum(TwoWeekDay),TwoWeekPres=sum(TwoWeekPres))%>%
  mutate(YTDAttendancePercentage=DaysPresent/TotalEnrolled)%>%
  mutate(TwoWeekAttendancePercentage=TwoWeekPres/TwoWeekDays)%>%
  select(Student.Id,YTDAttendancePercentage,TwoWeekAttendancePercentage)%>%
  rename("Student.ID"="Student.Id")%>%
  mutate(YTDAttendancePercentage=(paste(round((YTDAttendancePercentage)*100,digits=1),"%",sep="")))%>%
  mutate(TwoWeekAttendancePercentage=(paste(round((TwoWeekAttendancePercentage)*100,digits=1),"%",sep="")))
  
############################################
#EHOUT
ehout<-read.csv("ehout2018.csv")
ehout <-ehout %>%
  mutate(inccount=1) %>%
  filter(IncidentType=="referral")%>%
  group_by(OffenderID)%>%
  summarise(OSS=sum(OSS),Referrals=sum(inccount))%>%
  rename("Student.ID"="OffenderID")

########################################################
#upload file from eSchool
ADPinfo<-read.csv("ADPeschool.csv")

##############################################################
#upload current list
ADPList<-read.csv("ADPList.csv")
############################################################
#add credits
credits<-read.csv("Credits Etc Through 8.21.17.csv")
credits<-credits %>%
  select(student_id,TotalEarnCreditsToDate,Total.Pass)%>%
  rename("Student.ID"="student_id","TotalRegentsPassed"="Total.Pass","Credits"="TotalEarnCreditsToDate")
############################################################
#merge with everything
ADPFinal<-merge(ehout,attendance,"Student.ID",all=TRUE)
ADPFinal<-merge(ADPFinal,ADPinfo,"Student.ID",all=TRUE)
ADPFinal<-merge(ADPFinal,credits,"Student.ID",all=TRUE)
ADPFinal<-merge(ADPFinal,ADPList,"Student.ID",all.y=TRUE)

ADPFinal<-ADPFinal %>%
  mutate(birthdate=mdy(Birthdate))%>%
  mutate(today=today())%>%
  mutate(Age=floor((today-birthdate)/365.25))%>%
  select(Student.ID,Last,First,Building,Grade,Age,Counselor,Iep.Status,Credits,YTDAttendancePercentage,TwoWeekAttendancePercentage,TotalRegentsPassed,Referrals,OSS)


write.csv(ADPFinal, "C:/Users/drobil66/Desktop/RFiles/R Reports/ADP November 30 2017.csv") 