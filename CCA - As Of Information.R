ehout<-read.csv("ehout2017.csv")
library(plyr)
library(dplyr)
###parse dates and create time intervals
library(lubridate)
ehout$date <- mdy(ehout$IncidentDate) # puts into date format
ehout$year<-year(ehout$date) # parse year
ehout$month<-month(ehout$date) # parse month
ehout$month<-sprintf("%02d",ehout$month) # create 2 digit month
ehout$day<-day(ehout$date) # parse day
ehout$day<-sprintf("%02d",ehout$day) # create 2 digit day

ehout$date.num <- as.numeric(paste0(ehout$year, ehout$month, ehout$day)) # concatenate

#ytd calculations
ehout$ytd<-ifelse(ehout$date.num>20170601 & ehout$date.num<20170630, 1,0)


#####filter to proper data set
library(plyr)
library(dplyr)
ehout2<-filter(ehout, IncidentType=="referral")
ehout3<-filter(ehout2, ytd==1)
ehout3<-filter(ehout3, Grade!=15)

####data analysis for all incidents

ehout3$inccount=1


total.oss<-aggregate(OSS ~ OffenderID, data=ehout3, FUN=sum)
total.ossdays<-aggregate(OSS.Days  ~ OffenderID, data=ehout3, FUN=sum)
total.referral<-aggregate(inccount ~OffenderID,  data=ehout3, FUN=sum)



all.susp.categories<-merge(total.oss, total.ossdays, by="OffenderID")
all.susp.categories<-merge(all.susp.categories, total.referral,by="OffenderID")

#####################################


#attendance
attendance<-read.csv("attendancedaily.csv")

#convert date to correct format

###parse dates and create time intervals
attendance$date <- as.Date(attendance$cal_date) # puts into date format
attendance$year<-year(attendance$date)
attendance$month<-month(attendance$date)
attendance$month<-sprintf("%02d",attendance$month)
attendance$day<-day(attendance$date)
attendance$day<-sprintf("%02d",attendance$day)

attendance$date.num <- as.numeric(paste0(attendance$year, attendance$month, attendance$day)) 

attendance$MP<-ifelse(attendance$date.num>20170601 & attendance$date.num<20170630, 1,0)
attendance<-filter(attendance, MP==1)

#only select columns
attendance<-attendance[c("STUDENT_ID","MissedPeriods","Period.Absence..","MP")]
#change colname
colnames(attendance)[which(names(attendance)=="Period.Absence..")]<-"PeriodAbsencePercent"


#add column for each day
attendance$inccount = 1
#identify each day as absent or not
attendance$Absent<-as.numeric(attendance$'PeriodAbsencePercent'>.5)

enrolledMP1<- aggregate(inccount ~ STUDENT_ID, data = attendance, FUN = sum)
absencesMP1<- aggregate(Absent ~ STUDENT_ID, data = attendance, FUN = sum)

MP1Attendance<-merge(enrolledMP1,absencesMP1,by="STUDENT_ID",all=TRUE)
MP1Attendance$MP1Percent<-((MP1Attendance$inccount-MP1Attendance$Absent)/(MP1Attendance$inccount))
MP1Attendance$MP1Percent <- paste(round((MP1Attendance$MP1Percent)*100,digits=1),"%",sep="")

#
attendance<-MP1Attendance[c("STUDENT_ID","MP1Percent")]

#
colnames(all.susp.categories)[which(names(all.susp.categories)=="OffenderID")]<-"STUDENT_ID"


#merging

alltogether<-merge(attendance,all.susp.categories,by="STUDENT_ID",all=TRUE)
alltogether<-alltogether[c("STUDENT_ID","inccount","OSS","OSS.Days","MP1Percent")]


CCAList<-read.csv("CCA List.csv")
CCAList<-rename(CCAList,STUDENT_ID=Student.ID)

CCARequest<-merge(CCAList,alltogether,by="STUDENT_ID")
CCARequest<-select(CCARequest,STUDENT_ID,Student,School,Grade,CSSP,Plan,SIT.Date,IEP.504,Coach,Referrals,OSS,OSS.Days,MP1Percent)
CCARequest<-rename(CCARequest,AttendancePercentage=MP1Percent)
CCARequest<-rename(CCARequest,Referrals=inccount)
CCARequest[is.na(CCARequest)]<-0

write.csv(CCARequest, "C:/Users/drobil66/Desktop/RFiles/R Reports/CCA72417.csv")