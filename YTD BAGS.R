library(plyr)
library(dplyr)
library(scales)
library(xlsx)
library(lubridate)

#
attendance<-read.csv("attendancedaily.csv")

#only select columns
attendance<-attendance[c("STUDENT_ID","MissedPeriods","Period.Absence..")]
#change colname
colnames(attendance)[which(names(attendance)=="Period.Absence..")]<-"PeriodAbsencePercent"

#add column for each day
attendance$inccount = 1
#identify each day as absent or not
attendance$Absent<-as.numeric(attendance$'PeriodAbsencePercent'>.5)

#aggregate enrolled days and total absences
enrolled<- aggregate(inccount ~ STUDENT_ID, data = attendance, FUN = sum)
absences<- aggregate(Absent ~ STUDENT_ID, data = attendance, FUN = sum)


#mergeintoMP Files
attendance<-merge(enrolled,absences,by="STUDENT_ID",all=TRUE)


#createattendancepercentagecolumn
attendance$Percentage<-((attendance$inccount-attendance$Absent)/(attendance$inccount))

#change column name
colnames(attendance)[which(names(attendance)=="inccount")]<-"Days Enrolled"
colnames(attendance)[which(names(attendance)=="Absent")]<-"Days Absent"



#clean names
colnames(attendance)[which(names(attendance)=="STUDENT_ID")]<-"Student ID"

############################################
#EHOUT
ehout<-read.csv("ehout2017.csv")

#create a counter for every incident (will use to sum by ID)
ehout$inccount = 1

#Subset by MP
ehoutref<-subset(ehout,IncidentType=="classroom")
ehoutclass<-subset(ehout,IncidentType=="referral")


#sum referrals,OSS,OSS Days by MP

RefOSS<-aggregate(cbind(OSS,OSS.Days,inccount)~OffenderID,data=ehoutref,FUN=sum)

#subset classroom incidents by MP
Class<-aggregate(cbind(inccount)~OffenderID,data=ehoutclass,FUN=sum)


#change all column names
colnames(RefOSS)[which(names(RefOSS)=="inccount")]<-"Referrals"
colnames(RefOSS)[which(names(RefOSS)=="OSS")]<-"OSS"
colnames(RefOSS)[which(names(RefOSS)=="OSS.Days")]<-"OSS Days"
colnames(Class)[which(names(Class)=="inccount")]<-"Classroom Incidents"

#merge all together
ehoutfinal<-merge(RefOSS,Class,"OffenderID",all=TRUE)

colnames(ehoutfinal)[which(names(ehoutfinal)=="OffenderID")]<-"Student ID"

########################################################
#ENROLLMENT
#clean latest enrollment file
enrollment<-read.csv("enrollment 2017.csv")
colnames(enrollment)[which(names(enrollment)=="STUDENT_ID")]<-"Student ID"
enrollment$StudentName<-paste(enrollment$LastName,enrollment$FirstName,sep=", ", collapse = NULL)

enrollment<-enrollment[,c("Student ID","StudentName","Grade","EnrollmentSchoolName")]

#############################################################
#STAR Data

#load all files
WSM<-read.csv("WinterSM_cleaned.csv")
WSR<-read.csv("WinterSR_cleaned.csv")
WSEL<-read.csv("WinterSEL_cleaned.csv")

#cleaning STAR information
WSM<-WSM[c("Win16Math_StudentLocalID","Win16Math_ScreeningCategoryGroupAdjustment")]
WSR<-WSR[c("Win16Read_StudentLocalID","Win16Read_ScreeningCategoryGroupAdjustment")]
WSEL<-WSEL[c("Win16EL_StudentLocalID","Win16EL_ScreeningCategoryGroupAdjustment")]


#change column names
colnames(WSM)[which(names(WSM)=="Win16Math_ScreeningCategoryGroupAdjustment")]<-"STAR Math"
colnames(WSM)[which(names(WSM)=="Win16Math_StudentLocalID")]<-"Student ID"
colnames(WSR)[which(names(WSR)=="Win16Read_ScreeningCategoryGroupAdjustment")]<-"STAR Reading"
colnames(WSR)[which(names(WSR)=="Win16Read_StudentLocalID")]<-"Student ID"
colnames(WSEL)[which(names(WSEL)=="Win16EL_ScreeningCategoryGroupAdjustment")]<-"STAR EL"
colnames(WSEL)[which(names(WSEL)=="Win16EL_StudentLocalID")]<-"Student ID"

#merging
StarYTD<-merge(WSR,WSM,by="Student ID", all=TRUE)
StarYTD<-merge(StarYTD,WSEL,by="Student ID",all=TRUE)


##############################################################
#mark reporting information
library(dplyr)


MarkReporting<-read.csv("MarkReporting.csv")
MarkReporting<-MarkReporting[,c("student_id","MP1","MP2","Final.Mark")]


#identify courses and passinggrades grades
FinalGrades<-subset(MarkReporting,MarkReporting$Final.Mark>0)
MP2Grades<-subset(MarkReporting,is.na(MarkReporting$Final.Mark))
FinalGrades$FinalFail<-ifelse(FinalGrades$Final.Mark<65,1,0)
MP2Grades$MP2Fail<-ifelse(MP2Grades$MP2<65,1,0)

#Total for Each
MP2GradesFail<-aggregate(MP2Fail~student_id,data=MP2Grades,FUN=sum)
FinalGradesFail<-aggregate(FinalFail~student_id,data=FinalGrades,FUN=sum)

#merge
AllGrades<-merge(MP2GradesFail,FinalGradesFail,"student_id")
AllGrades$TotalFail<-(AllGrades$MP2Fail+AllGrades$FinalFail)

#clean for merge
colnames(AllGrades)[which(names(AllGrades)=="student_id")]<-"Student ID"
colnames(AllGrades)[which(names(AllGrades)=="TotalFail")]<-"Total Failed Courses Semester 1"
AllGrades<-AllGrades[,c("Student ID","Total Failed Courses Semester 1")]


############################################################
#enrollment information
enrollment<-read.csv("enrollment 2017.csv")
colnames(enrollment)[which(names(enrollment)=="STUDENT_ID")]<-"Student ID"
enrollment$StudentName<-paste(enrollment$LastName,enrollment$FirstName,sep=", ", collapse = NULL)

enrollment<-enrollment[,c("Student ID","StudentName","Grade","EnrollmentSchoolName","Gender","Ethnicity","ELL","IEP")]


############################################################
#combining files

#merge with everything
BAGSYTD<-merge(enrollment,attendance,"Student ID",all=TRUE)
BAGSYTD<-merge(BAGSYTD,ehoutfinal,"Student ID",all=TRUE)
BAGSYTD<-merge(BAGSYTD,StarYTD,"Student ID",all=TRUE)
BAGSYTD<-merge(BAGSYTD,AllGrades,"Student ID",all=TRUE)

#removes NA rows
BAGSYTD<-BAGSYTD[complete.cases(BAGSYTD[,2,4]),]

#create columns for flags
BAGSYTD$BehaviorFlag<-as.numeric(BAGSYTD$`Referrals`>=3)
BAGSYTD$GradeFlag<-as.numeric(BAGSYTD$`Total Failed Courses Semester 1`>=1)
BAGSYTD$AttendanceFlag<-as.numeric(BAGSYTD$`Percentage`<.9)
BAGSYTD$TotalFlags<-as.numeric(BAGSYTD$AttendanceFlag+BAGSYTD$BehaviorFlag+BAGSYTD$GradeFlag)
colnames(BAGSYTD)[which(names(BAGSYTD)=="BehaviorFlag")]<-"Behavior Flag"
colnames(BAGSYTD)[which(names(BAGSYTD)=="AttendanceFlag")]<-"Attendance Flag"
colnames(BAGSYTD)[which(names(BAGSYTD)=="GradeFlag")]<-"Grade Flag"
colnames(BAGSYTD)[which(names(BAGSYTD)=="TotalFlags")]<-"Total Flags"

#clean up percentages
BAGSYTD$Percentage <- paste(round((BAGSYTD$Percentage)*100,digits=1),"%",sep="")


#organize for final
BAGSYTD<-BAGSYTD[,c("Student ID","StudentName","EnrollmentSchoolName","Grade","Gender","Ethnicity","ELL","IEP","Total Flags","Behavior Flag","Attendance Flag","Grade Flag","Referrals","Classroom Incidents","OSS","OSS Days","Days Enrolled","Days Absent","Percentage","Total Failed Courses Semester 1","STAR Reading","STAR Math","STAR EL")]




write.csv(BAGSYTD, "C:/Users/drobil66/Desktop/RFiles/BAGSYTD.csv") 