library(plyr)
library(dplyr)
library(scales)
library(xlsx)
library(lubridate)

#
attendance<-read.csv("attendance.csv")

#convert date to correct format

###parse dates and create time intervals
attendance$date <- as.Date(attendance$cal_date) # puts into date format
attendance$year<-year(attendance$date)
attendance$month<-month(attendance$date)
attendance$month<-sprintf("%02d",attendance$month)
attendance$day<-day(attendance$date)
attendance$day<-sprintf("%02d",attendance$day)

attendance$date.num <- as.numeric(paste0(attendance$year, attendance$month, attendance$day)) 

attendance$MP<-ifelse(attendance$date.num>20160815 & attendance$date.num<20161127, 1, 
                 ifelse(attendance$date.num>20161128 & attendance$date.num<20170127, 2, 
                        ifelse(attendance$date.num>20170128 & attendance$date.num<20170407, 3, 
                               ifelse(attendance$date.num>20170408 & attendance$date.num<20170622, 4, 0)
                        )
                 )
)


#only select columns
attendance<-attendance[c("STUDENT_ID","MissedPeriods","Period.Absence..","MP")]
#change colname
colnames(attendance)[which(names(attendance)=="Period.Absence..")]<-"PeriodAbsencePercent"


#add column for each day
attendance$inccount = 1
#identify each day as absent or not
attendance$Absent<-as.numeric(attendance$'PeriodAbsencePercent'>.5)

#subset by MP
attendanceMP1<-subset(attendance,MP=="1")
attendanceMP2<-subset(attendance,MP=="2")
attendanceMP3<-subset(attendance,MP=="3")
attendanceMP4<-subset(attendance,MP=="4")

#aggregate enrolled days and total absences
enrolledMP1<- aggregate(inccount ~ STUDENT_ID, data = attendanceMP1, FUN = sum)
absencesMP1<- aggregate(Absent ~ STUDENT_ID, data = attendanceMP1, FUN = sum)
enrolledMP2<- aggregate(inccount ~ STUDENT_ID, data = attendanceMP2, FUN = sum)
absencesMP2<- aggregate(Absent ~ STUDENT_ID, data = attendanceMP2, FUN = sum)
enrolledMP3<- aggregate(inccount ~ STUDENT_ID, data = attendanceMP3, FUN = sum)
absencesMP3<- aggregate(Absent ~ STUDENT_ID, data = attendanceMP3, FUN = sum)
enrolledMP4<- aggregate(inccount ~ STUDENT_ID, data = attendanceMP4, FUN = sum)
absencesMP4<- aggregate(Absent ~ STUDENT_ID, data = attendanceMP4, FUN = sum)

#mergeintoMP Files
MP1Attendance<-merge(enrolledMP1,absencesMP1,by="STUDENT_ID",all=TRUE)
MP2Attendance<-merge(enrolledMP2,absencesMP2,by="STUDENT_ID",all=TRUE)
MP3Attendance<-merge(enrolledMP3,absencesMP3,by="STUDENT_ID",all=TRUE)
MP4Attendance<-merge(enrolledMP4,absencesMP4,by="STUDENT_ID",all=TRUE)

#createattendancepercentagecolumn
MP1Attendance$MP1Percent<-((MP1Attendance$inccount-MP1Attendance$Absent)/(MP1Attendance$inccount))
MP2Attendance$MP2Percent<-((MP2Attendance$inccount-MP2Attendance$Absent)/(MP2Attendance$inccount))
MP3Attendance$MP3Percent<-((MP3Attendance$inccount-MP3Attendance$Absent)/(MP3Attendance$inccount))
MP4Attendance$MP4Percent<-((MP4Attendance$inccount-MP4Attendance$Absent)/(MP4Attendance$inccount))

#change column name
colnames(MP1Attendance)[which(names(MP1Attendance)=="inccount")]<-"MP1 Days Enrolled"
colnames(MP1Attendance)[which(names(MP1Attendance)=="Absent")]<-"MP1 Days Absent"
colnames(MP2Attendance)[which(names(MP2Attendance)=="inccount")]<-"MP2 Days Enrolled"
colnames(MP2Attendance)[which(names(MP2Attendance)=="Absent")]<-"MP2 Days Absent"
colnames(MP3Attendance)[which(names(MP3Attendance)=="inccount")]<-"MP3 Days Enrolled"
colnames(MP3Attendance)[which(names(MP3Attendance)=="Absent")]<-"MP3 Days Absent"
colnames(MP4Attendance)[which(names(MP4Attendance)=="inccount")]<-"MP4 Days Enrolled"
colnames(MP4Attendance)[which(names(MP4Attendance)=="Absent")]<-"MP4 Days Absent"

#merge all together
attendancebyMP<-merge(MP1Attendance,MP2Attendance,"STUDENT_ID",all=TRUE)
attendancebyMP<-merge(attendancebyMP,MP3Attendance,"STUDENT_ID",all=TRUE)
attendancebyMP<-merge(attendancebyMP,MP4Attendance,"STUDENT_ID",all=TRUE)

#clean up percentages
attendancebyMP$MP1Percent <- paste(round((attendancebyMP$MP1Percent)*100,digits=1),"%",sep="")
attendancebyMP$MP2Percent <- paste(round((attendancebyMP$MP2Percent)*100,digits=1),"%",sep="")
attendancebyMP$MP3Percent <- paste(round((attendancebyMP$MP3Percent)*100,digits=1),"%",sep="")
attendancebyMP$MP4Percent <- paste(round((attendancebyMP$MP4Percent)*100,digits=1),"%",sep="")

#combine w EHOUT by quarter
colnames(attendancebyMP)[which(names(attendancebyMP)=="STUDENT_ID")]<-"Student ID"
BAGSbyQuarter<-merge(ehoutbyMP,attendancebyMP,"Student ID",all=TRUE)

#get things in appropriate order
BAGSbyQuarter<-BAGSbyQuarter[,c("Student ID","MP1 Referrals","MP1 Classroom Incidents","MP1 OSS","MP1Percent","MP2 Referrals","MP2 Classroom Incidents","MP2 OSS","MP2 OSS Days","MP2Percent")]
