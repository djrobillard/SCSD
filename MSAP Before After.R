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

MSAPattendance<-filter(attendance2017,name==c("MSAP/CORE","GPS Elementary Program"))
otherattendance<-filter(attendance2017,name!=c("MSAP/CORE"))
otherattendance<-filter(otherattendance,name!=c("GPS Elementary Program"))

#Other Attendance
#only necessary columns
otherattendance<-otherattendance[c("student_id","Period.Absence..")]
#add column for each day
otherattendance$OtherDays = 1
#identify each day as absent or not
otherattendance$OtherAbsent<-as.numeric(otherattendance$Period.Absence..)>.5
#total days and total absences
otherenrolleddays <- aggregate(OtherDays ~ student_id, data = otherattendance, FUN = sum)
otherabsentdays<-aggregate(OtherAbsent ~ student_id, data = otherattendance, FUN = sum)
#merge to one attendance percentage file
otherattendance<-merge(otherenrolleddays,otherabsentdays,by="student_id", all=TRUE)
#createattendancepercentagecolumn
otherattendance$totalpercent<-((otherattendance$OtherDays-otherattendance$OtherAbsent)/(otherattendance$OtherDays))
#change column name
colnames(otherattendance)[which(names(otherattendance)=="totalpercent")]<-"OtherSchoolPercentage"

####MSAP Attendance###################################

#Other Attendance
#only necessary columns
MSAPattendance<-MSAPattendance[c("student_id","Period.Absence..")]
#add column for each day
MSAPattendance$MSAPDays = 1
#identify each day as absent or not
MSAPattendance$MSAPabsent<-as.numeric(MSAPattendance$Period.Absence..)>.5
#total days and total absences
MSAPenrolleddays <- aggregate(MSAPDays ~ student_id, data = MSAPattendance, FUN = sum)
MSAPabsentdays<-aggregate(MSAPabsent ~ student_id, data = MSAPattendance, FUN = sum)
#merge to one attendance percentage file
MSAPattendance<-merge(MSAPenrolleddays,MSAPabsentdays,by="student_id", all=TRUE)
#createattendancepercentagecolumn
MSAPattendance$totalpercent<-((MSAPattendance$MSAPDays-MSAPattendance$MSAPabsent)/(MSAPattendance$MSAPDays))
#change column name
colnames(MSAPattendance)[which(names(MSAPattendance)=="totalpercent")]<-"MSAPPercentage"

Attendance<-merge(MSAPattendance,otherattendance,by="student_id",all=TRUE)
Attendance<-Attendance[c("student_id","MSAPPercentage","MSAPDays","MSAPabsent","OtherSchoolPercentage","OtherDays","OtherAbsent")]

Attendance$MSAPPercentage <- paste(round((Attendance$MSAPPercentage)*100,digits=1),"%",sep="")
Attendance$OtherSchoolPercentage <- paste(round((Attendance$OtherSchoolPercentage)*100,digits=1),"%",sep="")

########################################################################################################################
ehout<-read.csv("ehout2017.csv",strip.white = TRUE)
#clean up names
colnames(ehout)[which(names(ehout)=="OSS.Days")]<-"OSSDays"
colnames(ehout)[which(names(ehout)=="OffenderID")]<-"student_id"
colnames(ehout)[which(names(ehout)=="OffenderName")]<-"Student Name"

ehout<-ehout[c("student_id","SchoolName","OSS","OSSDays","IncidentType","IncidentDate","Student Name")]

ehout$Classroom<-ifelse(ehout$IncidentType=="classroom",1,0)
ehout$Referral<-ifelse(ehout$IncidentType=="referral",1,0)

MSAPehout<-filter(ehout,SchoolName==c("CORE"))
otherehout<-filter(ehout,SchoolName!=c("CORE"))

MSAPehout<-aggregate(cbind(OSS,Classroom,Referral)~student_id,data=MSAPehout,FUN=sum)
otherehout<-aggregate(cbind(OSS,Classroom,Referral)~student_id,data=otherehout,FUN=sum)


MSAPehout<-rename(MSAPehout,MSAPOSS=OSS)
MSAPehout<-rename(MSAPehout,MSAPClassroom=Classroom)
MSAPehout<-rename(MSAPehout,MSAPReferrals=Referral)

otherehout<-rename(otherehout,otherOSS=OSS)
otherehout<-rename(otherehout,otherClassroom=Classroom)
otherehout<-rename(otherehout,otherReferrals=Referral)

#add enrollment date to discipline file
ehoutmerge<-merge(otherehout,MSAPehout, by="student_id")

#######

demo<-read.csv("enrollmentallyears.csv")
demo1<-select(demo,SCHOOL_YEAR,STUDENT_ID,Ethnicity,IEP,ELL,Grade)
demo2017<-filter(demo1,SCHOOL_YEAR=="2017")
demo2017<-rename(demo2017,student_id=STUDENT_ID)

#cleanupcurrentMSAP

Name<-select(attendance2017,student_id,StudentName)

Final<-merge(Attendance,ehoutmerge,"student_id",all=TRUE)
Final<-merge(Final,demo2017,"student_id",all=TRUE)
Final<-merge(Final,Name,"student_id",all=TRUE)

Final<-unique(Final)




#GettingColumnsIn Order
FinalFile<-Final[,c("student_id","StudentName","Grade","otherReferrals","MSAPReferrals",
                    "otherClassroom","MSAPClassroom","otherOSS","MSAPOSS","OtherDays","OtherAbsent","OtherSchoolPercentage",
                    "MSAPDays","MSAPabsent","MSAPPercentage")]

FinalFile$OtherSchoolPercentage<-as.numeric(as.character(FinalFile$OtherSchoolPercentage))
FinalFile$OtherSchoolPercentage <- paste(round((FinalFile$OtherSchoolPercentage)*100,digits=1),"%",sep="")
FinalFile$MSAPPercentage<-as.numeric(as.character(FinalFile$MSAPPercentage))
FinalFile$MSAPPercentage <- paste(round((FinalFile$MSAPPercentage)*100,digits=1),"%",sep="")
FinalFile[is.na(FinalFile)]<-0

#printtoCSV
