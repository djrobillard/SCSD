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

attendance$MP<-ifelse(attendance$date.num>=2016822 & attendance$date.num<=20170226, 1, 
                      ifelse(attendance$date.num>=20170227 & attendance$date.num<=20170609, 2, 
                             ifelse(attendance$date.num>=20170610 & attendance$date.num<=20170630, 3,4) ))

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
attendancebyMP<-MP1Attendance
attendancebyMP<-merge(attendancebyMP,MP2Attendance,"STUDENT_ID",all=TRUE)
attendancebyMP<-merge(attendancebyMP,MP3Attendance,"STUDENT_ID",all=TRUE)
attendancebyMP<-merge(attendancebyMP,MP4Attendance,"STUDENT_ID",all=TRUE)

#clean up percentages
attendancebyMP$MP1Percent <- paste(round((attendancebyMP$MP1Percent)*100,digits=1),"%",sep="")
attendancebyMP$MP2Percent <- paste(round((attendancebyMP$MP2Percent)*100,digits=1),"%",sep="")
attendancebyMP$MP3Percent <- paste(round((attendancebyMP$MP3Percent)*100,digits=1),"%",sep="")
attendancebyMP$MP4Percent <- paste(round((attendancebyMP$MP4Percent)*100,digits=1),"%",sep="")

#clean names
colnames(attendancebyMP)[which(names(attendancebyMP)=="STUDENT_ID")]<-"Student ID"

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

ehout$MP<-ifelse(ehout$date.num>=2016822 & ehout$date.num<=20170226, 1, 
                      ifelse(ehout$date.num>20170227 & ehout$date.num<=20170609, 2, 
                             ifelse(ehout$date.num>20170610 & ehout$date.num<20170630, 3, 4
                                   
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

colnames(ehoutbyMP)[which(names(ehoutbyMP)=="OffenderID")]<-"Student ID"

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
FSM<-read.csv("Fall16SM_cleaned.csv")
FSR<-read.csv("Fall16SR_cleaned.csv")
FSEL<-read.csv("Fall16SEL_cleaned.csv")
WSM<-read.csv("WinterSM_cleaned.csv")
WSR<-read.csv("WinterSR_cleaned.csv")
WSEL<-read.csv("WinterSEL_cleaned.csv")
SR<-read.csv("starreading.csv")
SM<-read.csv("starmath.csv")

#cleaning STAR information
FSEL<-FSEL[c("F16EL_StudentLocalID","F16EL_LiteracyClassification")]
FSM<-FSM[c("F16Math_StudentLocalID","F16Math_ScreeningCategoryGroupAdjustment")]
FSR<-FSR[c("F16Read_StudentLocalID","F16Read_ScreeningCategoryGroupAdjustment")]
WSM<-WSM[c("Win16Math_StudentLocalID","Win16Math_ScreeningCategoryGroupAdjustment")]
WSR<-WSR[c("Win16Read_StudentLocalID","Win16Read_ScreeningCategoryGroupAdjustment")]
WSEL<-WSEL[c("Win16EL_StudentLocalID","Win16EL_LiteracyClassification")]
SM<-SM[c("Student.ID","S16Math_ScreeningCategoryGroupAdjustment")]
SR<-SR[c("Student.ID","S16Read_ScreeningCategoryGroupAdjustment")]

#change column names
colnames(FSM)[which(names(FSM)=="F16Math_ScreeningCategoryGroupAdjustment")]<-"Fall Math"
colnames(FSM)[which(names(FSM)=="F16Math_StudentLocalID")]<-"Student ID"
colnames(FSR)[which(names(FSR)=="F16Read_ScreeningCategoryGroupAdjustment")]<-"Fall Reading"
colnames(FSR)[which(names(FSR)=="F16Read_StudentLocalID")]<-"Student ID"
colnames(FSEL)[which(names(FSEL)=="F16EL_StudentLocalID")]<-"Student ID"
colnames(FSEL)[which(names(FSEL)=="F16EL_LiteracyClassification")]<-"Fall EL"
colnames(WSM)[which(names(WSM)=="Win16Math_ScreeningCategoryGroupAdjustment")]<-"Winter Math"
colnames(WSM)[which(names(WSM)=="Win16Math_StudentLocalID")]<-"Student ID"
colnames(WSR)[which(names(WSR)=="Win16Read_ScreeningCategoryGroupAdjustment")]<-"Winter Reading"
colnames(WSR)[which(names(WSR)=="Win16Read_StudentLocalID")]<-"Student ID"
colnames(WSEL)[which(names(WSEL)=="Win16EL_LiteracyClassification")]<-"Winter EL"
colnames(WSEL)[which(names(WSEL)=="Win16EL_StudentLocalID")]<-"Student ID"
colnames(SM)[which(names(SM)=="Student.ID")]<-"Student ID"
colnames(SR)[which(names(SR)=="S16Read_ScreeningCategoryGroupAdjustment")]<-"Spring Reading"
colnames(SR)[which(names(SR)=="Student.ID")]<-"Student ID"
colnames(SM)[which(names(SM)=="S16EL_ScreeningCategoryGroupAdjustment")]<-"Spring Math"


#merging
StarbyMP<-merge(FSM,FSR,by="Student ID", all=TRUE)
StarbyMP<-merge(StarbyMP,FSEL,by="Student ID",all=TRUE)
StarbyMP<-merge(StarbyMP,WSM,by="Student ID",all=TRUE)
StarbyMP<-merge(StarbyMP,WSR,by="Student ID",all=TRUE)
StarbyMP<-merge(StarbyMP,WSEL,by="Student ID",all=TRUE)
StarbyMP<-merge(StarbyMP,SM,by="Student ID",all=TRUE)
StarbyMP<-merge(StarbyMP,SR,by="Student ID",all=TRUE)

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
#combining files

#merge with everything
BAGSbyQuarter<-merge(enrollment,attendancebyMP,"Student ID",all=TRUE)
BAGSbyQuarter<-merge(BAGSbyQuarter,ehoutbyMP,"Student ID",all=TRUE)
BAGSbyQuarter<-merge(BAGSbyQuarter,StarbyMP,"Student ID",all=TRUE)
BAGSbyQuarter<-merge(BAGSbyQuarter,AllGrades,"Student ID",all=TRUE)

#removes NA rows
BAGSbyQuarter<-BAGSbyQuarter[complete.cases(BAGSbyQuarter[,2,4]),]

######################################################################

#run student list by homeroom in Cognos (Registration Reports - "Student List by Homeroom")
homeroom<-read.csv("homeroom.csv")
colnames(homeroom)[which(names(homeroom)=="Student.Id")]<-"Student ID"
colnames(homeroom)[which(names(homeroom)=="Student.Name.Last.First.Middle")]<-"Student Name"
colnames(homeroom)[which(names(homeroom)=="Name...Homeroom.Primary.Teacher")]<-"Homeroom Teacher"

#merge with BAGS by quarter
PPbyQuarter<-merge(homeroom,BAGSbyQuarter,"Student ID",all=TRUE)
#getting columns from enrollment
enroll<-read.csv("enrollmentallyears.csv")
enroll1<- enroll %>%
  filter(SCHOOL_YEAR %in% c(2017))%>%
  mutate(Name=paste(LastName, FirstName,sep=', ')) %>%
  select(SCHOOL_YEAR,STUDENT_ID,Name,EnrollmentSchoolName,Grade)


##student names
Demo<-read.csv("Demographics.csv")
Demo<-select(Demo,STUDENT_ID,LastName,FirstName)
demo<-rename(Demo,"Student ID"=STUDENT_ID)

PPbyQuarter<-merge(ehoutbyMP,attendancebyMP,"Student ID",all=TRUE)

PPbyQuarter[is.na(PPbyQuarter)]<-0

write.csv(PPbyQuarter, "C:/Users/drobil66/Desktop/RFiles/R Reports/PPbyQuarter.csv") 
