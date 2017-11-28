library(plyr)
library(dplyr)
library(scales)
library(xlsx)
library(lubridate)

#
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

attendance$MP<-ifelse(attendance$date.num>2016822 & attendance$date.num<20161031, 1, 
                      ifelse(attendance$date.num>20161101 & attendance$date.num<20170127, 2, 
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

#clean names
colnames(attendancebyMP)[which(names(attendancebyMP)=="STUDENT_ID")]<-"Student ID"
attendancebyMP[is.na(attendancebyMP)]<-0


attendancebyMP$TotalDaysEnrolled<-(attendancebyMP$`MP1 Days Enrolled`+attendancebyMP$`MP2 Days Enrolled`+
                                      attendancebyMP$`MP3 Days Enrolled`+attendancebyMP$`MP4 Days Enrolled`)
attendancebyMP$TotalDaysAbsent<-(attendancebyMP$`MP1 Days Absent`+attendancebyMP$`MP2 Days Absent`+
                                    attendancebyMP$`MP3 Days Absent`+attendancebyMP$`MP4 Days Absent`)
attendancebyMP$TotalAttendancePercentage<-(1-(attendancebyMP$TotalDaysAbsent/attendancebyMP$TotalDaysEnrolled))
attendancebyMP$TotalAttendancePercentage <- paste(round((attendancebyMP$TotalAttendancePercentage)*100,digits=1),"%",sep="")


############################################
#EHOUT
ehout<-read.csv("ehout2017.csv")
library(lubridate)

###parse dates and create time intervals
ehout$date <- mdy(ehout$IncidentDate) # puts into date format
ehout$year<-year(ehout$date)
ehout$month<-month(ehout$date)
ehout$month<-sprintf("%02d",ehout$month)
ehout$day<-day(ehout$date)
ehout$day<-sprintf("%02d",ehout$day)

ehout$date.num <- as.numeric(paste0(ehout$year, ehout$month, ehout$day)) 

ehout$MP<-ifelse(ehout$date.num>2016822 & ehout$date.num<20161031, 1, 
                 ifelse(ehout$date.num>20161101 & ehout$date.num<20170127, 2, 
                        ifelse(ehout$date.num>20170128 & ehout$date.num<20170407, 3, 
                               ifelse(ehout$date.num>20170408 & ehout$date.num<20170622, 4, 0)
                        )
                 )
)
# cleaning up the rest
#Cleaning Up EHOUT for Usage

#only columns from ehout that are necessary
ehout<-ehout[c("OffenderID","OSS","OSS.Days","IncidentType","MP")]
#only referrals from ehout
ehoutref<-subset(ehout,IncidentType=="referral")
ehoutclass<-subset(ehout,IncidentType=="classroom")
#change OSS Days Name to one word
colnames(ehoutref)[which(names(ehoutref)=="OSS.Days")]<-"OSSDays"

#create a counter for every incident (will use to sum by ID)
ehoutref$inccount = 1
ehoutclass$classcount=1

#Subset by MP
ehoutrefMP1<-subset(ehoutref,MP=="1")
ehoutrefMP2<-subset(ehoutref,MP=="2")
ehoutrefMP3<-subset(ehoutref,MP=="3")
ehoutrefMP4<-subset(ehoutref,MP=="4")
ehoutclassMP1<-subset(ehoutclass,MP=="1")
ehoutclassMP2<-subset(ehoutclass,MP=="2")
ehoutclassMP3<-subset(ehoutclass,MP=="3")
ehoutclassMP4<-subset(ehoutclass,MP=="4")

#sum referrals,OSS,OSS Days by MP

MP1RefOSS<-aggregate(cbind(OSS,OSSDays,inccount)~OffenderID,data=ehoutrefMP1,FUN=sum)
MP2RefOSS<-aggregate(cbind(OSS,OSSDays,inccount)~OffenderID,data=ehoutrefMP2,FUN=sum)
MP3RefOSS<-aggregate(cbind(OSS,OSSDays,inccount)~OffenderID,data=ehoutrefMP3,FUN=sum)
MP4RefOSS<-aggregate(cbind(OSS,OSSDays,inccount)~OffenderID,data=ehoutrefMP4,FUN=sum)

#subset classroom incidents by MP
MP1Class<-aggregate(cbind(classcount)~OffenderID,data=ehoutclassMP1,FUN=sum)
MP2Class<-aggregate(cbind(classcount)~OffenderID,data=ehoutclassMP2,FUN=sum)
MP3Class<-aggregate(cbind(classcount)~OffenderID,data=ehoutclassMP3,FUN=sum)
MP4Class<-aggregate(cbind(classcount)~OffenderID,data=ehoutclassMP4,FUN=sum)

#change all column names
colnames(MP1RefOSS)[which(names(MP1RefOSS)=="inccount")]<-"MP1 Referrals"
colnames(MP1RefOSS)[which(names(MP1RefOSS)=="OSS")]<-"MP1 OSS"
colnames(MP1RefOSS)[which(names(MP1RefOSS)=="OSSDays")]<-"MP1 OSS Days"
colnames(MP1Class)[which(names(MP1Class)=="classcount")]<-"MP1 Classroom Incidents"

colnames(MP2RefOSS)[which(names(MP2RefOSS)=="inccount")]<-"MP2 Referrals"
colnames(MP2RefOSS)[which(names(MP2RefOSS)=="OSS")]<-"MP2 OSS"
colnames(MP2RefOSS)[which(names(MP2RefOSS)=="OSSDays")]<-"MP2 OSS Days"
colnames(MP2Class)[which(names(MP2Class)=="classcount")]<-"MP2 Classroom Incidents"

colnames(MP3RefOSS)[which(names(MP3RefOSS)=="inccount")]<-"MP3 Referrals"
colnames(MP3RefOSS)[which(names(MP3RefOSS)=="OSS")]<-"MP3 OSS"
colnames(MP3RefOSS)[which(names(MP3RefOSS)=="OSSDays")]<-"MP3 OSS Days"
colnames(MP3Class)[which(names(MP3Class)=="classcount")]<-"MP3 Classroom Incidents"

colnames(MP4RefOSS)[which(names(MP4RefOSS)=="inccount")]<-"MP4 Referrals"
colnames(MP4RefOSS)[which(names(MP4RefOSS)=="OSS")]<-"MP4 OSS"
colnames(MP4RefOSS)[which(names(MP4RefOSS)=="OSSDays")]<-"MP4 OSS Days"
colnames(MP4Class)[which(names(MP4Class)=="classcount")]<-"MP4 Classroom Incidents"

#merge all together
ehoutbyMP<-merge(MP1RefOSS,MP1Class,"OffenderID",all=TRUE)
ehoutbyMP<-merge(ehoutbyMP,MP2RefOSS,"OffenderID",all=TRUE)
ehoutbyMP<-merge(ehoutbyMP,MP2Class,"OffenderID",all=TRUE)
ehoutbyMP<-merge(ehoutbyMP,MP3RefOSS,"OffenderID",all=TRUE)
ehoutbyMP<-merge(ehoutbyMP,MP3Class,"OffenderID",all=TRUE)
ehoutbyMP<-merge(ehoutbyMP,MP4RefOSS,"OffenderID",all=TRUE)
ehoutbyMP<-merge(ehoutbyMP,MP4Class,"OffenderID",all=TRUE)

ehoutbyMP$TotalOSS<-sum(ehoutbyMP$`MP1 OSS`,ehoutbyMP$`MP2 OSS`,ehoutbyMP$`MP3 OSS`,ehoutbyMP$`MP4 OSS`)
ehoutbyMP$TotalReferral<-sum(ehoutbyMP$`MP1 Referral`,ehoutbyMP$`MP2 Referral`,ehoutbyMP$`MP3 Referral`,ehoutbyMP$`MP4 Referral`)
ehoutbyMP$TotalOSSDays<-sum(ehoutbyMP$`MP1 OSSDays`,ehoutbyMP$`MP2 OSSDays`,ehoutbyMP$`MP3 OSSDays`,ehoutbyMP$`MP4 OSSDays`)
ehoutbyMP$TotalClassroom<-sum(ehoutbyMP$`MP1 Classroom`,ehoutbyMP$`MP2 Classroom`,ehoutbyMP$`MP3 Classroom`,ehoutbyMP$`MP4 Classroom`)

colnames(ehoutbyMP)[which(names(ehoutbyMP)=="OffenderID")]<-"Student ID"



########################################################
#ENROLLMENT
#clean latest enrollment file
enroll<-read.csv("enrollmentallyears.csv")
enrollment<- enroll %>%
  filter(SCHOOL_YEAR %in% c(2017))%>%
  mutate(StudentName=paste(LastName, FirstName,sep=', ')) %>%
  select(STUDENT_ID,StudentName,Grade,EnrollmentSchoolName)
 
colnames(enrollment)[which(names(enrollment)=="STUDENT_ID")]<-"Student ID"

#############################################################
#STAR Data

#load all files
FSM<-read.csv("Fall16SM_cleaned.csv")
FSR<-read.csv("Fall16SR_cleaned.csv")
FSEL<-read.csv("Fall16SEL_cleaned.csv")
WSM<-read.csv("WinterSM_cleaned.csv")
WSR<-read.csv("WinterSR_cleaned.csv")
WSEL<-read.csv("WinterSEL_cleaned.csv")
SSM<-read.csv("springmath.csv")
SSR<-read.csv("springread.csv")
SSEL<-read.csv("springel.csv")
#cleaning STAR information
FSEL<-FSEL[c("F16EL_StudentLocalID","F16EL_GradePlacement","F16EL_ScreeningCategoryGroupAdjustment")]
FSM<-FSM[c("F16Math_StudentLocalID","F16Math_ScreeningCategoryGroupAdjustment")]
FSR<-FSR[c("F16Read_StudentLocalID","F16_GradePlacement","F16_GradeEquivalent")]
WSM<-WSM[c("Win16Math_StudentLocalID","Win16Math_ScreeningCategoryGroupAdjustment")]
WSR<-WSR[c("Win16Read_StudentLocalID","Win16Read_ScreeningCategoryGroupAdjustment")]
WSEL<-WSEL[c("Win16EL_StudentLocalID","Win16EL_LiteracyClassification")]
SSM<-SSM[c("Sp16_StudentLocalID","Sp16_ScreeningCategoryGroupAdjustment")]
SSR<-SSR[c("Sp16_StudentLocalID","Sp16_GradePlacement","Sp16_GradeEquivalent")]
SSEL<-SSEL[c("Sp16_StudentLocalID","Sp16_ScreeningCategoryGroupAdjustment")]

#change column names
colnames(FSM)[which(names(FSM)=="F16Math_ScreeningCategoryGroupAdjustment")]<-"Fall Math"
colnames(FSM)[which(names(FSM)=="F16Math_StudentLocalID")]<-"Student ID"
colnames(FSR)[which(names(FSR)=="F16Read_ScreeningCategoryGroupAdjustment")]<-"Fall Reading"
colnames(FSR)[which(names(FSR)=="F16Read_StudentLocalID")]<-"Student ID"
colnames(FSEL)[which(names(FSEL)=="F16EL_StudentLocalID")]<-"Student ID"
colnames(FSEL)[which(names(FSEL)=="F16EL_ScreeningCategoryGroupAdjustment")]<-"Fall EL"
colnames(WSM)[which(names(WSM)=="Win16Math_ScreeningCategoryGroupAdjustment")]<-"Winter Math"
colnames(WSM)[which(names(WSM)=="Win16Math_StudentLocalID")]<-"Student ID"
colnames(WSR)[which(names(WSR)=="Win16Read_ScreeningCategoryGroupAdjustment")]<-"Winter Reading"
colnames(WSR)[which(names(WSR)=="Win16Read_StudentLocalID")]<-"Student ID"
colnames(WSEL)[which(names(WSEL)=="Win16EL_ScreeningCategoryGroupAdjustment")]<-"Winter EL"
colnames(WSEL)[which(names(WSEL)=="Win16EL_StudentLocalID")]<-"Student ID"
colnames(SSEL)[which(names(SSEL)=="Sp16_ScreeningCategoryGroupAdjustment")]<-"Spring EL"
colnames(SSEL)[which(names(SSEL)=="Sp16_StudentLocalID")]<-"Student ID"
colnames(SSR)[which(names(SSR)=="Sp16_ScreeningCategoryGroupAdjustment")]<-"Spring Reading"
colnames(SSR)[which(names(SSR)=="Sp16_StudentLocalID")]<-"Student ID"
colnames(SSM)[which(names(SSM)=="Sp16_ScreeningCategoryGroupAdjustment")]<-"Spring Math"
colnames(SSM)[which(names(SSM)=="Sp16_StudentLocalID")]<-"Student ID"


#merging
StarbyMP<-merge(FSM,FSR,by="Student ID", all=TRUE)
StarbyMP<-merge(StarbyMP,FSEL,by="Student ID",all=TRUE)
StarbyMP<-merge(StarbyMP,WSM,by="Student ID",all=TRUE)
StarbyMP<-merge(StarbyMP,WSR,by="Student ID",all=TRUE)
StarbyMP<-merge(StarbyMP,WSEL,by="Student ID",all=TRUE)
StarbyMP<-merge(StarbyMP,SSEL,by="Student ID",all=TRUE)
StarbyMP<-merge(StarbyMP,SSR,by="Student ID",all=TRUE)
StarbyMP<-merge(StarbyMP,SSM,by="Student ID",all=TRUE)

##############################################################
#mark reporting information
library(dplyr)


MarkReporting<-read.csv("MarkReporting.csv")
MarkReporting<-MarkReporting[,c("student_id","MP1","MP2","MP3","MP4","Final.Mark")]


#identify courses and passinggrades grades
MarkReporting$MP1Enroll<-ifelse(MarkReporting$MP1>=0,1,0)
MarkReporting$MP2Enroll<-ifelse(MarkReporting$MP2>=0,1,0)
MarkReporting$MP3Enroll<-ifelse(MarkReporting$MP3>=0,1,0)
MarkReporting$MP4Enroll<-ifelse(MarkReporting$MP4>=0,1,0)
MarkReporting$FinalEnroll<-ifelse(MarkReporting$Final.Mark>=0,1,0)
MarkReporting$MP1Pass<-ifelse(MarkReporting$MP1>=65,1,0)
MarkReporting$MP2Pass<-ifelse(MarkReporting$MP2>=65,1,0)
MarkReporting$MP3Pass<-ifelse(MarkReporting$MP3>=65,1,0)
MarkReporting$MP4Pass<-ifelse(MarkReporting$MP4>=65,1,0)
MarkReporting$FinalPass<-ifelse(MarkReporting$Final.Mark>=65,1,0)

#Total for Each
FinalMarkReporting<-aggregate(cbind(MP1Enroll,MP1Pass,MP2Enroll,MP2Pass,MP3Enroll,MP3Pass,
                                MP4Enroll,MP4Pass,FinalEnroll,FinalPass)~student_id,data=MarkReporting,FUN=sum)
FinalMarkReporting$MP1PassPercent<-(FinalMarkReporting$MP1Pass/FinalMarkReporting$MP1Enroll)
FinalMarkReporting$MP2PassPercent<-(FinalMarkReporting$MP2Pass/FinalMarkReporting$MP2Enroll)
FinalMarkReporting$MP3PassPercent<-(FinalMarkReporting$MP3Pass/FinalMarkReporting$MP3Enroll)
FinalMarkReporting$MP4PassPercent<-(FinalMarkReporting$MP4Pass/FinalMarkReporting$MP4Enroll)
FinalMarkReporting$FinalPassPercent<-(FinalMarkReporting$FinalPass/FinalMarkReporting$FinalEnroll)
FinalMarkReporting$MP1Fail<-ifelse(FinalMarkReporting$MP1PassPercent>100,1,0)
FinalMarkReporting$MP2Fail<-ifelse(FinalMarkReporting$MP2PassPercent>100,1,0)
FinalMarkReporting$MP3Fail<-ifelse(FinalMarkReporting$MP3PassPercent>100,1,0)
FinalMarkReporting$MP4Fail<-ifelse(FinalMarkReporting$MP4PassPercent>100,1,0)
FinalMarkReporting$FinalFail<-ifelse(FinalMarkReporting$FinalPassPercent>100,1,0)
FinalMarkReporting<-select(FinalMarkReporting,student_id,MP1PassPercent,MP1Fail,MP2PassPercent,MP2Fail,
                           MP3PassPercent,MP3Fail,MP4PassPercent,MP4Fail,FinalPassPercent,FinalFail)

#clean for merge
colnames(FinalMarkReporting)[which(names(FinalMarkReporting)=="student_id")]<-"Student ID"


FinalMarkReporting$MP1PassPercent <- paste(round((FinalMarkReporting$MP1PassPercent)*100,digits=1),"%",sep="")
FinalMarkReporting$MP2PassPercent <- paste(round((FinalMarkReporting$MP2PassPercent)*100,digits=1),"%",sep="")
FinalMarkReporting$MP3PassPercent <- paste(round((FinalMarkReporting$MP3PassPercent)*100,digits=1),"%",sep="")
FinalMarkReporting$MP4PassPercent <- paste(round((FinalMarkReporting$MP4PassPercent)*100,digits=1),"%",sep="")
FinalMarkReporting$FinalPassPercent<-paste(round((FinalMarkReporting$FinalPassPercent)*100,digits=1),"%",sep="")

############################################################
#combining files with PZList

PZList<-read.csv("PZList10182017.csv")
colnames(PZList)[which(names(PZList)=="Student_ID")]<-"Student ID"

#merge with everything
BAGSbyQuarter<-merge(enrollment,attendancebyMP,"Student ID",all=TRUE)
BAGSbyQuarter<-merge(BAGSbyQuarter,ehoutbyMP,"Student ID",all=TRUE)
BAGSbyQuarter<-merge(BAGSbyQuarter,StarbyMP,"Student ID",all=TRUE)
BAGSbyQuarter<-merge(BAGSbyQuarter,FinalMarkReporting,"Student ID",all=TRUE)
BAGSbyQuarter<-merge(BAGSbyQuarter,PZList,"Student ID",all=TRUE)
BAGSbyQuarter[is.na(BAGSbyQuarter)]<-0

BAGSbyQuarter<-filter(BAGSbyQuarter,TotalDaysEnrolled!=0)
BAGSbyQuarter<-mutate(BAGSbyQuarter,BehaviorFlag=ifelse(TotalReferral>=3,1,0))
BAGSbyQuarter$GradeBand<-revalue(BAGSbyQuarter$Grade,c("KF"="PK-2","PKH"="PK-2","PKF"="PK-2","01"="PK-2","02"="PK-2","U2"="PK-2",
                                                      "03"="3-5","04"="3-5","05"="3-5","U4"="3-5","U3"="3-5",
                                                      "06"="6-8","07"="6-8","08"="6-8","U6"="6-8","U7"="6-8","U8"="6-8",
                                                      "09"="9-12","10"="9-12","11"="9-12","12"="9-12","T+"="9-12","US"="9-12","14"="9-12","13"="3-5"))


################################################################################
#run student list by homeroom in Cognos (Registration Reports - "Student List by Homeroom")
homeroom<-read.csv("homeroom.csv")
colnames(homeroom)[which(names(homeroom)=="Student.Id")]<-"Student ID"
colnames(homeroom)[which(names(homeroom)=="Student.Name.Last.First.Middle")]<-"Student Name"
colnames(homeroom)[which(names(homeroom)=="Name...Homeroom.Primary.Teacher")]<-"Homeroom Teacher"

#merge with BAGS by quarter
PPbyQuarter<-merge(homeroom,BAGSbyQuarter,"Student ID",all=TRUE)

#clean latest enrollment file
enrollment<-read.csv("enrollment 2017.csv")
colnames(enrollment)[which(names(enrollment)=="STUDENT_ID")]<-"Student ID"

enrollment<-enrollment[,c("Student ID","Grade","EnrollmentSchoolName")]

PPbyQuarter<-merge(enrollment,PPbyQuarter,"Student ID",all=TRUE)

PPbyQuarter[is.na(PPbyQuarter)]<-0

write.csv(BAGSbyQuarter, "C:/Users/drobil66/Desktop/RFiles/R Reports/BAGSbyquarter.csv") 





write.csv(BAGSbyQuarter, "C:/Users/drobil66/Desktop/RFiles/BAGSbyquarter.csv") 
