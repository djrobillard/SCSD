library(lubridate)
library(plyr)
library(dplyr)
library(scales)

hillside<-read.csv("Hillside.csv")
attendance<-read.csv("attendance.csv")
ehout<-read.csv("ehout.csv")

#column names for Hillside
colnames(hillside)[which(names(hillside)=="Student.ID")]<-"student_id"


#connect attendance for those students in 201617
colnames(attendance)[which(names(attendance)=="Attendance.Percentage")]<-"AttendancePercentage"


#Import Current YTD attendance
attendance<-attendance[,c("student_id","End", "grade", "IEP", "ELL", "EnrollmentSchoolName", "DaysEnrolled", "DailyAbsences", "AttendancePercentage")]

#subset for current students
attendance$End<-as.character(attendance$End)
CurrentStudents<-subset(attendance,End=="02/09/2017")

#ehout
#clean up names
colnames(ehout)[which(names(ehout)=="OSS.Days")]<-"OSSDays"
colnames(ehout)[which(names(ehout)=="OffenderID")]<-"student_id"
colnames(ehout)[which(names(ehout)=="OffenderName")]<-"Student Name"

ehout<-ehout[c("student_id","OSS","OSSDays","IncidentType","IncidentDate","Student Name")]

ehout$date<-mdy(ehout$IncidentDate)
ehout$year<-year(ehout$date)
ehout$month<-month(ehout$date)
ehout$month<-sprintf("%02d",ehout$month)
ehout$day<-day(ehout$date)
ehout$day<-sprintf("%02d",ehout$day)

ehout$edate.num <- as.numeric(paste0(ehout$year, ehout$month, ehout$day)) 

ehout$inccount<-1

EhoutRef<-subset(ehout,IncidentType=="referral")
EhoutClass<-subset(ehout,IncidentType=="classroom")

Agg1 <- aggregate(inccount ~ student_id, data = EhoutRef, FUN = sum)
colnames(Agg1)[which(names(Agg1)=="inccount")]<-"Referrals"
Agg2 <- aggregate(inccount ~ student_id, data = EhoutClass, FUN = sum)
colnames(Agg2)[which(names(Agg2)=="inccount")]<-"Classroom Incidents"
Agg3 <- aggregate(OSS ~ student_id,data=ehout,FUN=sum)
Agg4<-aggregate(OSSDays~student_id,data=ehout,FUN=sum)

AllDiscipline<-merge(Agg1,Agg2,"student_id",all=TRUE)
AllDiscipline<-merge(AllDiscipline,Agg3,"student_id",all=TRUE)
AllDiscipline<-merge(AllDiscipline,Agg4,"student_id",all=TRUE)

#combining
FinalHillside<-merge(CurrentStudents,AllDiscipline,by="student_id",all=TRUE)
FinalHillside<-merge(hillside,FinalHillside,by="student_id")
#getting columns in order
FinalHillside<-FinalHillside[,c("student_id","Name","School","Assigned.Staff", "EnrollmentSchoolName","grade","Cohort.Group","Race","IEP","ELL","Gender","HWSC.Enrollment.Date","MS.Tag","Referrals","Classroom Incidents","OSS","OSSDays","DaysEnrolled","DailyAbsences","AttendancePercentage")]
FinalHillside$AttendancePercentage<-as.numeric(as.character(FinalHillside$AttendancePercentage))
FinalHillside$AttendancePercentage <- paste(round((FinalHillside$AttendancePercentage)*100,digits=1),"%",sep="")
FinalHillside[is.na(FinalHillside)]<-0


#printtocsv
write.csv(FinalHillside, "C:/Users/drobil66/Desktop/RFiles/HillsideBags.csv") 