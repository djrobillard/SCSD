library(plyr)
library(dplyr)

#read files
ehout2017<-read.csv("ehout2017.csv")
attendancedaily<-read.csv("attendancedaily.csv")
enrollment<-read.csv("2018Enrollment.csv",strip.white = TRUE)
SEL_cleaned<-read.csv("starearlyliteracy.csv")
SM_cleaned<-read.csv("starmath.csv")
SR_cleaned<-read.csv("starreading.csv")
MarkReporting<-read.csv("MarkReporting.csv")
MKV<-read.csv("MKV.csv")
Demo<-read.csv("enrollmentallyears.csv")

#Cleaning Up EHOUT for Usage

#only columns from ehout that are necessary
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
alldiscipline<-merge(totrefstuid,stuidOSS,"OffenderID",all=TRUE)
alldiscipline<-merge(alldiscipline,totclasstuid,"OffenderID",all=TRUE)
#change column names
colnames(alldiscipline)[which(names(alldiscipline)=="inccount")]<-"Total Referrals"
colnames(alldiscipline)[which(names(alldiscipline)=="OSS")]<-"Total OSS Incidents"
colnames(alldiscipline)[which(names(alldiscipline)=="OSSDays")]<-"Total OSS Days"
colnames(alldiscipline)[which(names(alldiscipline)=="OffenderID")]<-"student_id"
colnames(alldiscipline)[which(names(alldiscipline)=="classcount")]<-"Classroom Incidents"

#cleaning up attendance file for usage
#changing name
attendance2017<-attendancedaily
attendance2017<-rename(attendance2017,student_id=STUDENT_ID)
#only necessary columns
smallattendance2017<-attendance2017[c("student_id","Period.Absence..")]
#add column for each day
smallattendance2017$Days = 1
#identify each day as absent or not
smallattendance2017$Absent<-as.numeric(smallattendance2017$Period.Absence..)>.5
#total days and total absences
enrolleddays <- aggregate(Days ~ student_id, data = smallattendance2017, FUN = sum)
absentdays<-aggregate(Absent ~ student_id, data = smallattendance2017, FUN = sum)
#merge to one attendance percentage file
finalattendance<-merge(enrolleddays,absentdays,by="student_id", all=TRUE)
#createattendancepercentagecolumn
finalattendance$totalpercent<-((finalattendance$Days-finalattendance$Absent)/(finalattendance$Days))
#change column name
colnames(finalattendance)[which(names(finalattendance)=="Days")]<-"Days Enrolled"
colnames(finalattendance)[which(names(finalattendance)=="Absent")]<-"Days Absent"
colnames(finalattendance)[which(names(finalattendance)=="totalpercent")]<-"Attendance Percentage"


#cleaning STAR information
SEL<-SEL_cleaned[c("Student.ID","W16EL_LiteracyClassification")]
SM<-SM_cleaned[c("Student.ID","S16Math_ScreeningCategoryGroupAdjustment")]
SR<-SR_cleaned[c("Student.ID","S16Read_ScreeningCategoryGroupAdjustment")]

#change column names
colnames(SEL)[which(names(SEL)=="W16EL_LiteracyClassification")]<-"Winter Early Literacy"
colnames(SM)[which(names(SM)=="S16Math_ScreeningCategoryGroupAdjustment")]<-"Spring STAR Reading"
colnames(SR)[which(names(SR)=="S16Read_ScreeningCategoryGroupAdjustment")]<-"Spring STAR Math"
colnames(SEL)[which(names(SEL)=="Student.ID")]<-"STUDENT_ID"
colnames(SM)[which(names(SM)=="Student.ID")]<-"STUDENT_ID"
colnames(SR)[which(names(SR)=="Student.ID")]<-"STUDENT_ID"

#merging
FinalStar<-merge(SEL,SM,by="STUDENT_ID", all=TRUE)
FinalStar<-merge(FinalStar,SR,by="STUDENT_ID",all=TRUE)
FinalStar<-rename(FinalStar,student_id=STUDENT_ID)


#####markreporting
MarkReporting<-MarkReporting[,c("student_id","MP1","MP2","MP3","MP4","Final.Mark")]

############################

#identify courses and passinggrades grades
FinalGrades<-subset(MarkReporting,MarkReporting$Final.Mark>0)
FinalGrades$FinalFail<-ifelse(FinalGrades$Final.Mark<65,1,0)

#Total for Each
FinalGradesFail<-aggregate(FinalFail~student_id,data=FinalGrades,FUN=sum)

#merge
AllGrades<-merge(MP3GradesFail,FinalGradesFail,"student_id")
AllGrades<-FinalGradesFail
AllGrades$TotalFail<-(AllGrades$MP3Fail+AllGrades$FinalFail)

#clean for merge
colnames(AllGrades)[which(names(AllGrades)=="TotalFail")]<-"Total Failed Courses 2017"

#getting columns from enrollment
enrollmentclean<-enrollment[c("Student.ID","Student.Name","Building","Grade")]
enrollmentclean<-rename(enrollmentclean,student_id=Student.ID)

#MKV Information
MKV$McKinneyVento<-1
MKV<-select(MKV,Student.Id,McKinneyVento)
MKV<-rename(MKV,student_id=Student.Id)

##Getting Demographic Columns
Demo1<- Demo %>%
  filter(SCHOOL_YEAR %in% c(2017))%>%
  select(SCHOOL_YEAR,STUDENT_ID,Ethnicity,IEP)%>%
  rename(student_id=STUDENT_ID)


#merging all
final1<-merge(enrollmentclean,alldiscipline,by="student_id",all=TRUE)
final2<-merge(final1,finalattendance,by="student_id",all=TRUE)
final3<-merge(final2,FinalStar,by="student_id",all=TRUE)
final4<-merge(final3,MKV,by="student_id",all=TRUE)
final5<-merge(final4,Demo1,by="student_id",all=TRUE)
finaltable<-merge(final5,AllGrades,by="student_id",all=TRUE)

#organizing table
finaltable$BehaviorFlag<-ifelse(finaltable$`Total Referrals`>=3,1,ifelse(finaltable$`Classroom Incidents`>=1,0))
finaltable$AttendanceFlag<-ifelse(finaltable$`Attendance Percentage`<=.9,1,0)
finaltable$GradeFlag<-ifelse(finaltable$FinalFail>=1,1,ifelse((finaltable$`Spring STAR Reading`=="Urgent Intervention"),1,0))
finaltable$TotalFlags<-((finaltable$BehaviorFlag)+(finaltable$AttendanceFlag)+(finaltable$GradeFlag))


finaltable<-finaltable[,c("student_id","Student.Name","Building","Gender","Ethnicity","ELL",
                          "Grade","McKinneyVento","TotalFlags","BehaviorFlag",
                          "AttendanceFlag","GradeFlag","Total Referrals","Classroom Incidents",
                          "Total OSS Incidents","Total OSS Days","Days Enrolled", "Days Absent", 
                          "Attendance Percentage","Spring STAR Reading", "Spring STAR Math","Winter Early Literacy",
                          "FinalFail")]

finaltable$Building<-strtrim(finaltable$Building, 31)

finaltable$`Attendance Percentage` <- paste(round((finaltable$`Attendance Percentage`)*100,digits=1),"%",sep="")

finaltable<-filter(finaltable,`Days Enrolled`>1)


#exporting to csv
write.csv(finaltable, "C:/Users/drobil66/Desktop/RFiles/R Reports/NextYearBAGS.csv")
