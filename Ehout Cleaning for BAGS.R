library(plyr)
library(dplyr)
library(scales)
library(xlsx)

#Cleaning Up EHOUT for Usage

#only columns from ehout that are necessary
ehout2017<-read.csv("ehout2017.csv")
smallehout<-ehout2017[c("OffenderID","OSS","OSS.Days","IncidentType")]
#only referrals from ehout
smallehoutref<-subset(smallehout,IncidentType=="referral")
smallehoutclass<-subset(smallehout,IncidentType=="classroom")
#change OSS Days Name to one word
colnames(smallehoutref)[which(names(smallehoutref)=="OSS.Days")]<-"OSSDays"

#aggregate number of OSS and OSS Days by student IDs
stuidOSS<-aggregate(cbind(OSS,OSSDays)~OffenderID,data=smallehoutref,FUN=sum)
#create a counter for every incident (will use to sum by ID)
smallehoutref$inccount = 1
smallehoutclass$classcount=1
#sum the counter by unique OffenderID
totrefstuid <- aggregate(inccount ~ OffenderID, data = smallehoutref, FUN = sum)
totclasstuid<-aggregate(classcount~OffenderID,data=smallehoutclass, FUN = sum)
#attach OSS Incidents, OSS Days
alldiscipline1<-merge(totrefstuid,stuidOSS,"OffenderID",all=TRUE)
alldiscipline<-merge(alldiscipline1,totclasstuid,"OffenderID",all=TRUE)
#change column names
colnames(alldiscipline)[which(names(alldiscipline)=="inccount")]<-"Total Referrals"
colnames(alldiscipline)[which(names(alldiscipline)=="OSS")]<-"Total OSS Incidents"
colnames(alldiscipline)[which(names(alldiscipline)=="OSSDays")]<-"Total OSS Days"
colnames(alldiscipline)[which(names(alldiscipline)=="OffenderID")]<-"STUDENT_ID"
colnames(alldiscipline)[which(names(alldiscipline)=="classcount")]<-"Classroom Incidents"

#cleaning up attendance file for usage
#changing name
attendancedaily<-read.csv("attendance.csv")
attendance2017<-attendancedaily
#only necessary columns
smallattendance2017<-attendance2017[c("STUDENT_ID","Period.Absence..")]
#add column for each day
smallattendance2017$inccount = 1
#identify each day as absent or not
smallattendance2017$Absent<-smallattendance2017$Period.Absence..>.5
#total days and total absences
enrolleddays <- aggregate(inccount ~ STUDENT_ID, data = smallattendance2017, FUN = sum)
absentdays<-aggregate(Absent ~ STUDENT_ID, data = smallattendance2017, FUN = sum)
#merge to one attendance percentage file
finalattendance<-merge(enrolleddays,absentdays,by="STUDENT_ID", all=TRUE)
#createattendancepercentagecolumn
finalattendance$totalpercent<-((finalattendance$inccount-finalattendance$Absent)/(finalattendance$inccount))
#change column name
colnames(finalattendance)[which(names(finalattendance)=="inccount")]<-"Days Enrolled"
colnames(finalattendance)[which(names(finalattendance)=="Absent")]<-"Days Absent"
colnames(finalattendance)[which(names(finalattendance)=="totalpercent")]<-"Attendance Percentage"


#cleaning STAR information
SEL_cleaned<-read.csv("WinterSEL_cleaned.csv")
SM_cleaned<-read.csv("WinterSM_cleaned.csv")
SR_cleaned<-read.csv("WinterSR_cleaned.csv")
SEL<-SEL_cleaned[c("Win16EL_StudentLocalID","Win16EL_LiteracyClassification")]
SM<-SM_cleaned[c("Win16Math_StudentLocalID","Win16Math_ScreeningCategoryGroupAdjustment")]
SR<-SR_cleaned[c("Win16Read_StudentLocalID","Win16Read_ScreeningCategoryGroupAdjustment")]
#change column names
colnames(SEL)[which(names(SEL)=="Win16EL_LiteracyClassification")]<-"Early Literacy"
colnames(SM)[which(names(SM)=="Win16Math_ScreeningCategoryGroupAdjustment")]<-"STAR Reading"
colnames(SR)[which(names(SR)=="Win16Read_ScreeningCategoryGroupAdjustment")]<-"STAR Math"
colnames(SEL)[which(names(SEL)=="Win16EL_StudentLocalID")]<-"STUDENT_ID"
colnames(SM)[which(names(SM)=="Win16Math_StudentLocalID")]<-"STUDENT_ID"
colnames(SR)[which(names(SR)=="Win16Read_StudentLocalID")]<-"STUDENT_ID"
#merging
FinalSTAR1<-merge(SEL,SM,by="STUDENT_ID", all=TRUE)
FinalStar<-merge(FinalSTAR1,SR,by="STUDENT_ID",all=TRUE)

#getting columns from enrollment
enrollment17<-read.csv("demographics.csv")
enrollment17<-subset(enrollment17,SCHOOL_YEAR=="2017")
enrollment17<-subset(enrollment17,current_status=="A")
enrollment<-enrollment17[c("STUDENT_ID","FirstName","LastName","Gender","Grade","Ethnicity","ELL","IEP","EnrollmentSchoolName")]

#merging all four
final1<-merge(enrollment,alldiscipline,by="STUDENT_ID",all=TRUE)
final2<-merge(final1,finalattendance,by="STUDENT_ID",all=TRUE)
final<-merge(final2,FinalStar,by="STUDENT_ID",all=TRUE)

#organizing table
finaltable<-final[,c("STUDENT_ID","FirstName","LastName","EnrollmentSchoolName","Grade","Gender","IEP","ELL","Ethnicity","Total Referrals","Classroom Incidents","Total OSS Incidents","Total OSS Days","Days Enrolled", "Days Absent", "Attendance Percentage","STAR Reading", "STAR Math","Early Literacy")]
finaltable<-merge(finaltable,AllGrades,"STUDENT_ID")

#exporting to XLSX
write.csv(final, "C:/Users/drobil66/Desktop/RFiles/BAGS227.csv")



##########################################

#merging all three files together BOOM
discandmr<-merge(alldiscipline,AllGrades, by="student_id", all=TRUE)
alltogether<-merge(discandmr,cleanattendance,by="student_id", all=TRUE)
# turning NA into 0
alltogether[is.na(alltogether)]<-0
clean<-subset(alltogether, student_id>10)
#Organizing Final Table
#getting only necessary columns
clean1<-clean[,c("student_id","EnrollmentSchoolName","grade","Gender","Ethnicity","ELL","IEP","F.R.Lunch","AttendanceFlag","Total Referrals","DailyAttendancePercentage","sum","Total OSS Incidents","Total OSS Days","DaysEnrolled","DailyAbsences")]
#cleaning up column names
colnames(clean1)[which(names(clean1)=="EnrollmentSchoolName")]<-"School"
colnames(clean1)[which(names(clean1)=="grade")]<-"Grade"
colnames(clean1)[which(names(clean1)=="F.R.Lunch")]<-"FRPL"
colnames(clean1)[which(names(clean1)=="sum")]<-"Total Failed Courses"
colnames(clean1)[which(names(clean1)=="DaysEnrolled")]<-"Days Enrolled"
colnames(clean1)[which(names(clean1)=="DailyAbsences")]<-"Days Absent"
colnames(clean1)[which(names(clean1)=="DailyAttendancePercentage")]<-"Attendance Percentage"
colnames(clean1)[which(names(clean1)=="student_id")]<-"Student ID"
colnames(clean1)[which(names(clean1)=="AttendanceFlag")]<-"Attendance Flag"
#creating columns for flags
clean1$BehaviorFlag<-as.numeric(clean1$`Total Referrals`>=3)
clean1$GradeFlag<-as.numeric(clean1$`Total Failed Courses`>=1)
colnames(clean1)[which(names(clean1)=="BehaviorFlag")]<-"Behavior Flag"
colnames(clean1)[which(names(clean1)=="GradeFlag")]<-"Grade Flag"
clean1$TotalFlags<-as.numeric(clean1$`Attendance Flag`+clean1$'Behavior Flag'+clean1$'Grade Flag')
colnames(clean1)[which(names(clean1)=="TotalFlags")]<-"Total Flags"

#organizing final table
finaltable<-clean1[,c("Total Flags","Student ID","School","Grade","Gender","Ethnicity","ELL","IEP","FRPL","Behavior Flag","Attendance Flag","Grade Flag","Total Referrals","Attendance Percentage","Total Failed Courses","Total OSS Incidents","Total OSS Days","Days Enrolled","Days Absent")]
#exporting to XLSX
write.csv(finaltable, "C:/Users/drobil66/Desktop/RFiles/BAGS.csv") 