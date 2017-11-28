library(lubridate)
library(plyr)
library(dplyr)
library(scales)

########Attendance
#cleaning up attendance file for usage
#changing name
attendancedaily<-read.csv("attendancedaily.csv",strip.white = TRUE)
attendance2017<-attendancedaily
attendance2017<-rename(attendance2017,student_id=STUDENT_ID)

attendance2017$cal_date <- as.Date(attendance2017$cal_date) # puts into date format
attendance2017$year<-year(attendance2017$cal_date) # parse year
attendance2017$month<-month(attendance2017$cal_date) # parse month
attendance2017$month<-sprintf("%02d",attendance2017$month) # create 2 digit month
attendance2017$day<-day(attendance2017$cal_date) # parse day
attendance2017$day<-sprintf("%02d",attendance2017$day) # create 2 digit day

attendance2017$date.num <- as.numeric(paste0(attendance2017$year, attendance2017$month, attendance2017$day)) # concatenate


attendance2017$before<-ifelse(attendance2017$date.num<20161031,1,0) 
attendance2017$after<-ifelse(attendance2017$date.num>20170217,1,0)     
attendance2017$before2<-ifelse(attendance2017$date.num<20170227,1,0) 
attendance2017$after2<-ifelse(attendance2017$date.num>20170609,1,0)

attendancesmall<-select(attendance2017,student_id,before,after,before2,after2,Period.Absence..)

attendancesmall$absent<-ifelse(as.numeric(attendancesmall$Period.Absence..)>.5,1,0)

afterattendance<-filter(attendancesmall,after==1)
beforeattendance<-filter(attendancesmall,before==1)
afterattendance2<-filter(attendancesmall,after2==1)
beforeattendance2<-filter(attendancesmall,before2==1)

beforeenrolleddays <- aggregate(before ~ student_id, data = beforeattendance, FUN = sum)
beforeabsentdays<-aggregate(absent ~ student_id, data = beforeattendance, FUN = sum)

afterenrolleddays <- aggregate(after ~ student_id, data = afterattendance, FUN = sum)
afterabsentdays<-aggregate(absent ~ student_id, data = afterattendance, FUN = sum)

beforeenrolleddays2 <- aggregate(before2 ~ student_id, data = beforeattendance2, FUN = sum)
beforeabsentdays2<-aggregate(absent ~ student_id, data = beforeattendance2, FUN = sum)

afterenrolleddays2 <- aggregate(after2 ~ student_id, data = afterattendance2, FUN = sum)
afterabsentdays2<-aggregate(absent ~ student_id, data = afterattendance2, FUN = sum)

after<-merge(afterenrolleddays,afterabsentdays,by="student_id",all=TRUE)
after$afterPercentage<-((after$after-after$absent)/after$after)
after<-select(after,student_id,afterPercentage)

before<-merge(beforeenrolleddays,beforeabsentdays,by="student_id",all=TRUE)
before$beforePercentage<-((before$before-before$absent)/before$before)
before<-select(before,student_id,beforePercentage)

after2<-merge(afterenrolleddays2,afterabsentdays2,by="student_id",all=TRUE)
after2$after2Percentage<-((after2$after2-after2$absent)/after2$after2)
after2<-select(after2,student_id,after2Percentage)

before2<-merge(beforeenrolleddays2,beforeabsentdays2,by="student_id",all=TRUE)
before2$before2Percentage<-((before2$before2-before2$absent)/before2$before2)
before2<-select(before2,student_id,before2Percentage)

attendance<-merge(after,before,by="student_id",all=TRUE)
attendance<-merge(attendance,after2,by="student_id",all=TRUE)
attendance<-merge(attendance,before2,by="student_id",all=TRUE)

########################################################################################################################
ehout<-read.csv("ehout2017.csv",strip.white = TRUE)
#clean up names
colnames(ehout)[which(names(ehout)=="OSS.Days")]<-"OSSDays"
colnames(ehout)[which(names(ehout)=="OffenderID")]<-"student_id"
colnames(ehout)[which(names(ehout)=="OffenderName")]<-"Student Name"

ehout$date <- mdy(ehout$IncidentDate) # puts into date format
ehout$year<-year(ehout$date) # parse year
ehout$month<-month(ehout$date) # parse month
ehout$month<-sprintf("%02d",ehout$month) # create 2 digit month
ehout$day<-day(ehout$date) # parse day
ehout$day<-sprintf("%02d",ehout$day) # create 2 digit day

ehout$date.num <- as.numeric(paste0(ehout$year, ehout$month, ehout$day)) # concatenate

#ytd calculations
ehout$before<-ifelse(ehout$date.num<20161031,1,0) 
ehout$after<-ifelse(ehout$date.num>20170217,1,0)     
ehout$before2<-ifelse(ehout$date.num<20170227,1,0) 
ehout$after2<-ifelse(ehout$date.num>20170609,1,0)    

ehout<-select(ehout,student_id,SchoolName,OSS,OSSDays,IncidentType,before,after,before2,after2,`Student Name`)

ehout$Classroom<-ifelse(ehout$IncidentType=="classroom",1,0)
ehout$Referral<-ifelse(ehout$IncidentType=="referral",1,0)

afterehout<-filter(ehout,after==1)
beforeehout<-filter(ehout,before==1)
afterehout2<-filter(ehout,after2==1)
beforeehout2<-filter(ehout,before2==1)

afterehout<-aggregate(cbind(OSS,Classroom,Referral)~student_id,data=afterehout,FUN=sum)
beforeehout<-aggregate(cbind(OSS,Classroom,Referral)~student_id,data=beforeehout,FUN=sum)
afterehout2<-aggregate(cbind(OSS,Classroom,Referral)~student_id,data=afterehout2,FUN=sum)
beforeehout2<-aggregate(cbind(OSS,Classroom,Referral)~student_id,data=beforeehout2,FUN=sum)

afterehout<-rename(afterehout,afterOSS=OSS)
afterehout<-rename(afterehout,afterClassroom=Classroom)
afterehout<-rename(afterehout,afterReferrals=Referral)

beforeehout<-rename(beforeehout,beforeOSS=OSS)
beforeehout<-rename(beforeehout,beforeClassroom=Classroom)
beforeehout<-rename(beforeehout,beforeReferrals=Referral)

afterehout2<-rename(afterehout2,afterOSS2=OSS)
afterehout2<-rename(afterehout2,afterClassroom2=Classroom)
afterehout2<-rename(afterehout2,afterReferrals2=Referral)

beforeehout2<-rename(beforeehout2,beforeOSS2=OSS)
beforeehout2<-rename(beforeehout2,beforeClassroom2=Classroom)
beforeehout2<-rename(beforeehout2,beforeReferrals2=Referral)

#add enrollment date to discipline file
ehoutmerge<-merge(beforeehout,afterehout, by="student_id",all=TRUE)
ehoutmerge<-merge(ehoutmerge,beforeehout2, by="student_id",all=TRUE)
ehoutmerge<-merge(ehoutmerge,afterehout2, by="student_id",all=TRUE)

Final<-merge(attendance,ehoutmerge,"student_id",all=TRUE)

write.csv(Final, "C:/Users/drobil66/Desktop/RFiles/R Reports/PPRequest.csv") 