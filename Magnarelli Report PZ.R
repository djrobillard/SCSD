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
colnames(MP1Attendance)[which(names(MP1Attendance)=="inccount")]<-"MP1DaysEnrolled"
colnames(MP1Attendance)[which(names(MP1Attendance)=="Absent")]<-"MP1DaysAbsent"
colnames(MP2Attendance)[which(names(MP2Attendance)=="inccount")]<-"MP2DaysEnrolled"
colnames(MP2Attendance)[which(names(MP2Attendance)=="Absent")]<-"MP2DaysAbsent"
colnames(MP3Attendance)[which(names(MP3Attendance)=="inccount")]<-"MP3DaysEnrolled"
colnames(MP3Attendance)[which(names(MP3Attendance)=="Absent")]<-"MP3DaysAbsent"
colnames(MP4Attendance)[which(names(MP4Attendance)=="inccount")]<-"MP4DaysEnrolled"
colnames(MP4Attendance)[which(names(MP4Attendance)=="Absent")]<-"MP4DaysAbsent"

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
colnames(attendancebyMP)[which(names(attendancebyMP)=="STUDENT_ID")]<-"student_ID"
attendancebyMP[is.na(attendancebyMP)]<-0


attendancebyMP$TotalDaysEnrolled<-(attendancebyMP$`MP1DaysEnrolled`+attendancebyMP$`MP2DaysEnrolled`+
                                     attendancebyMP$`MP3DaysEnrolled`+attendancebyMP$`MP4DaysEnrolled`)
attendancebyMP$TotalDaysAbsent<-(attendancebyMP$`MP1DaysAbsent`+attendancebyMP$`MP2DaysAbsent`+
                                   attendancebyMP$`MP3DaysAbsent`+attendancebyMP$`MP4DaysAbsent`)
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
colnames(MP1RefOSS)[which(names(MP1RefOSS)=="inccount")]<-"MP1Referrals"
colnames(MP1RefOSS)[which(names(MP1RefOSS)=="OSS")]<-"MP1OSS"
colnames(MP1RefOSS)[which(names(MP1RefOSS)=="OSSDays")]<-"MP1OSSDays"
colnames(MP1Class)[which(names(MP1Class)=="classcount")]<-"MP1ClassroomIncidents"

colnames(MP2RefOSS)[which(names(MP2RefOSS)=="inccount")]<-"MP2Referrals"
colnames(MP2RefOSS)[which(names(MP2RefOSS)=="OSS")]<-"MP2OSS"
colnames(MP2RefOSS)[which(names(MP2RefOSS)=="OSSDays")]<-"MP2OSSDays"
colnames(MP2Class)[which(names(MP2Class)=="classcount")]<-"MP2ClassroomIncidents"

colnames(MP3RefOSS)[which(names(MP3RefOSS)=="inccount")]<-"MP3Referrals"
colnames(MP3RefOSS)[which(names(MP3RefOSS)=="OSS")]<-"MP3OSS"
colnames(MP3RefOSS)[which(names(MP3RefOSS)=="OSSDays")]<-"MP3OSSDays"
colnames(MP3Class)[which(names(MP3Class)=="classcount")]<-"MP3ClassroomIncidents"

colnames(MP4RefOSS)[which(names(MP4RefOSS)=="inccount")]<-"MP4Referrals"
colnames(MP4RefOSS)[which(names(MP4RefOSS)=="OSS")]<-"MP4OSS"
colnames(MP4RefOSS)[which(names(MP4RefOSS)=="OSSDays")]<-"MP4OSSDays"
colnames(MP4Class)[which(names(MP4Class)=="classcount")]<-"MP4ClassroomIncidents"

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

colnames(ehoutbyMP)[which(names(ehoutbyMP)=="OffenderID")]<-"student_ID"



########################################################
#ENROLLMENT
#clean latest enrollment file
enroll<-read.csv("enrollmentallyears.csv")
enrollment<- enroll %>%
  filter(SCHOOL_YEAR %in% c(2017))%>%
  filter(WITHDRAWAL_DATE %in% c(""))%>%
  mutate(StudentName=paste(LastName, FirstName,sep=', ')) %>%
  select(STUDENT_ID,StudentName,Grade,EnrollmentSchoolName)

colnames(enrollment)[which(names(enrollment)=="STUDENT_ID")]<-"student_ID"

#############################################################
#STAR Data

#load all files
FSR<-read.csv("Fall16SR_cleaned.csv")
FSEL<-read.csv("Fall16SEL_cleaned.csv")
WSR<-read.csv("WinterSR_cleaned.csv")
WSEL<-read.csv("WinterSEL_cleaned.csv")
SSR<-read.csv("springread.csv")
SSEL<-read.csv("springel.csv")
#cleaning STAR information
FSEL<-FSEL[c("F16EL_StudentLocalID","F16EL_GradePlacement")]
FSR<-FSR[c("F16Read_StudentLocalID","F16Read_GradePlacement","F16Read_GradeEquivalent")]
WSR<-WSR[c("Win16Read_StudentLocalID","Win16Read_GradePlacement","Win16Read_GradeEquivalent")]
WSEL<-WSEL[c("Win16EL_StudentLocalID","Win16EL_LiteracyClassification")]
SSR<-SSR[c("Sp16_StudentLocalID","Sp16_GradePlacement","Sp16_GradeEquivalent")]
SSEL<-SSEL[c("Sp16_StudentLocalID","Sp16_ScreeningCategoryGroupAdjustment")]

#change column names
colnames(FSR)[which(names(FSR)=="F16Read_GradeEquivalent")]<-"FallReading"
colnames(FSR)[which(names(FSR)=="F16Read_StudentLocalID")]<-"student_ID"
colnames(FSEL)[which(names(FSEL)=="F16EL_StudentLocalID")]<-"student_ID"
colnames(FSEL)[which(names(FSEL)=="F16EL_ScreeningCategoryGroupAdjustment")]<-"FallEL"
colnames(WSR)[which(names(WSR)=="Win16Read_GradeEquivalent")]<-"WinterReading"
colnames(WSR)[which(names(WSR)=="Win16Read_StudentLocalID")]<-"student_ID"
colnames(WSEL)[which(names(WSEL)=="Win16EL_ScreeningCategoryGroupAdjustment")]<-"WinterEL"
colnames(WSEL)[which(names(WSEL)=="Win16EL_StudentLocalID")]<-"student_ID"
colnames(SSEL)[which(names(SSEL)=="Sp16_ScreeningCategoryGroupAdjustment")]<-"SpringEL"
colnames(SSEL)[which(names(SSEL)=="Sp16_StudentLocalID")]<-"student_ID"
colnames(SSR)[which(names(SSR)=="Sp16_GradeEquivalent")]<-"SpringReading"
colnames(SSR)[which(names(SSR)=="Sp16_StudentLocalID")]<-"student_ID"


#merging
StarbyMP<-merge(FSEL,FSR,by="student_ID", all=TRUE)
StarbyMP<-merge(StarbyMP,WSR,by="student_ID",all=TRUE)
StarbyMP<-merge(StarbyMP,WSEL,by="student_ID",all=TRUE)
StarbyMP<-merge(StarbyMP,SSEL,by="student_ID",all=TRUE)
StarbyMP<-merge(StarbyMP,SSR,by="student_ID",all=TRUE)

StarbyMP$GradeLevel<-ifelse(((StarbyMP$`SpringReading`)>=(StarbyMP$Sp16_GradePlacement)),1,0)
StarbyMP$Growth<-ifelse((StarbyMP$`SpringReading`-StarbyMP$`FallReading`)>=1,1,0)
StarbyMP$Growth2<-ifelse((StarbyMP$`SpringReading`-StarbyMP$`WinterReading`)>=1,1,0)
StarbyMP$FinalGrowth<-ifelse((StarbyMP$Growth+StarbyMP$Growth2)>0,0,1)

StarbyMP<-select(StarbyMP,student_ID,GradeLevel,FinalGrowth)

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
colnames(FinalMarkReporting)[which(names(FinalMarkReporting)=="student_id")]<-"student_ID"


FinalMarkReporting$MP1PassPercent <- paste(round((FinalMarkReporting$MP1PassPercent)*100,digits=1),"%",sep="")
FinalMarkReporting$MP2PassPercent <- paste(round((FinalMarkReporting$MP2PassPercent)*100,digits=1),"%",sep="")
FinalMarkReporting$MP3PassPercent <- paste(round((FinalMarkReporting$MP3PassPercent)*100,digits=1),"%",sep="")
FinalMarkReporting$MP4PassPercent <- paste(round((FinalMarkReporting$MP4PassPercent)*100,digits=1),"%",sep="")
FinalMarkReporting$FinalPassPercent<-paste(round((FinalMarkReporting$FinalPassPercent)*100,digits=1),"%",sep="")

############################################################
#combining files with PZList

PZList<-read.csv("PZList10182017.csv")
colnames(PZList)[which(names(PZList)=="Student_ID")]<-"student_ID"

###38 Test Scores
State382017<-read.csv("38Test2017.csv")
State382016<-read.csv("38Test2016.csv")
State382017ELA<-State382017 %>%
  select(STUDENT_ID,SUBJECT,STANDARD_ACHIEVED,NUMERIC_SCORE)%>%
  rename(student_ID=STUDENT_ID,Subject2017=SUBJECT,Performance2017=STANDARD_ACHIEVED,Scale2017=NUMERIC_SCORE)%>%
  filter(Subject2017=="ELA")%>%
  rename(ELAPerform2017=Performance2017,ELAScale2017=Scale2017)%>%
  select(student_ID,ELAPerform2017,ELAScale2017)


State382017MATH<-State382017 %>%
  select(STUDENT_ID,SUBJECT,STANDARD_ACHIEVED,NUMERIC_SCORE)%>%
  rename(student_ID=STUDENT_ID,Subject2017=SUBJECT,Performance2017=STANDARD_ACHIEVED,Scale2017=NUMERIC_SCORE)%>%
  filter(Subject2017=="MATH")%>%
  rename(MathPerform2017=Performance2017,MathScale2017=Scale2017)%>%
  select(student_ID,MathPerform2017,MathScale2017)


State382016Math<-State382016 %>%
  select(Student.ID,Report.Subject,Standard.Achieved,Score)%>%
  rename(student_ID=Student.ID,Subject2016=Report.Subject,Performance2016=Standard.Achieved,Scale2016=Score)%>%
  filter(Subject2016=="Mathematics")%>%
  rename(MathPerform2016=Performance2016,MathScale2016=Scale2016)%>%
  select(student_ID,MathPerform2016,MathScale2016)

State382016ELA<-State382016 %>%
  select(Student.ID,Report.Subject,Standard.Achieved,Score)%>%
  rename(student_ID=Student.ID,Subject2016=Report.Subject,Performance2016=Standard.Achieved,Scale2016=Score)%>%
  filter(Subject2016=="ELA")%>%
  rename(ELAPerform2016=Performance2016,ELAScale2016=Scale2016)%>%
  select(student_ID,ELAPerform2016,ELAScale2016)

TestScores<-merge(State382016ELA,State382016Math,by="student_ID",all = TRUE)
TestScores<-merge(TestScores,State382017ELA,by="student_ID",all=TRUE)
TestScores<-merge(TestScores,State382017MATH,by="student_ID",all=TRUE)

#Dropoutlist
Dropouts<-read.csv("Dropout2017.csv")
Dropouts<-Dropouts %>%
  rename(student_ID=student_id)
  



#merge with everything
BAGSbyQuarter<-merge(enrollment,attendancebyMP,by="student_ID",all=TRUE)
BAGSbyQuarter<-merge(BAGSbyQuarter,ehoutbyMP,"student_ID",all=TRUE)
BAGSbyQuarter<-merge(BAGSbyQuarter,StarbyMP,"student_ID",all=TRUE)
BAGSbyQuarter<-merge(BAGSbyQuarter,FinalMarkReporting,"student_ID",all=TRUE)
BAGSbyQuarter<-merge(BAGSbyQuarter,PZList,"student_ID",all=TRUE)
BAGSbyQuarter<-merge(BAGSbyQuarter,TestScores,"student_ID",all=TRUE)
BAGSbyQuarter<-merge(BAGSbyQuarter,Dropouts,"student_ID",all=TRUE)


BAGSbyQuarter<-filter(BAGSbyQuarter,TotalDaysEnrolled!=0)
BAGSbyQuarter<-mutate(BAGSbyQuarter,BehaviorFlag=ifelse(TotalReferral>=3,1,0))
BAGSbyQuarter<-mutate(BAGSbyQuarter,AttendanceFlag=ifelse(TotalAttendancePercentage<.9,1,0))
BAGSbyQuarter$GradeBand<-BAGSbyQuarter$Grade
BAGSbyQuarter$GradeBand<-revalue(BAGSbyQuarter$Grade,c("PS"="PK-2","PH"="PK-2","PK"="PK-2","ps"="PK-2","KF"="PK-2","1"="PK-2","2"="PK-2","U2"="PK-2","U1"="PK-2","UK"="PK-2",
                                                       "3"="3to5","4"="3to5","5"="3to5","U4"="3to5","U3"="3to5","U5"="3to5","UP"="PK-2",
                                                       "6"="6to8","7"="6to8","8"="6to8","U6"="6to8","U7"="6to8","U8"="6to8","UG"="9to12",
                                                       "9"="9to12","10"="9to12","11"="9to12","12"="9to12","T+"="9to12","US"="9to12","13"="3to5","G1"="9to12","AD"="9to12"))
BAGSbyQuarter$StateTestImproveELA<-ifelse(BAGSbyQuarter$ELAScale2017-BAGSbyQuarter$ELAScale2016>0,1,0)
BAGSbyQuarter$StateTestImproveMath<-ifelse(BAGSbyQuarter$MathScale2017-BAGSbyQuarter$MathScale2016>0,1,0)

BAGSbyQuarter$SchoolBand<-BAGSbyQuarter$EnrollmentSchoolName
BAGSbyQuarter$SchoolBand<-revalue(BAGSbyQuarter$SchoolBand,c(
"Fowler High School"="HS",                  "JVC EPIC Program"="ALT",                       
"Nottingham High School"="HS",            "Corcoran High School"="HS"   ,                    
"Twilight Academy @ Henninger"="HS",      "McCarthy at Beard School"="ALT" ,                  
"Twilight Academy @ Corcoran"="HS",       "Henninger High School"="HS"     ,                 
"Twilight Academy @ Nottingham"="HS",     "Institute of Technology at Syracuse Central"="HS",
"Twilight Academy @ Fowler"="HS",         "Onondaga County Sheriff Dept"="OTH"               ,
"Twilight Academy @ PSLA"="HS",          "Charter Syracuse Academy of Science"="OTH"        ,
"Building 555"="OTH",                     "Public Service Leadership Academy at Fowler"="HS",
"Johnson Center Transition Program"="ALT","C.B.A."="OTH"                                     ,
"Faith Heritage"="OTH",                  "P-Tech"="HS"                                     ,
"Home School - Parent Teach"="OTH",     "Clary Middle School"="MS"                       ,
"Elmcrest Children's Center School"="ALT",    "Edward Smith K-8 School"="K8"                    ,
"Westside Academy at Blodgett"="MS" ,        "Grant Middle School"="MS"                         ,
"Bishop Ludden"="OTH",                "Frazer K-8 School"="K8"                          ,
"Charter Southside Academy"="OTH",    "Huntington K-8 School"="K8"                      ,
"Lincoln Middle School"="MS" ,         "Expeditionary Learning Middle School"="MS"        ,
"Hurlbut W. Smith K-8"="K8",          "Danforth Middle School"="MS"                      ,
"Bishop Grimes"="OTH",                "BOCES OCM Cedar Street"="OTH"                     ,
"Roberts K-8 School"="K8",           "Living Word"="OTH"                                ,
"MSAP/CORE"="ALT",                  "Salem Hyde Elementary School" ="E"              ,
"Parkview"="OTH",                     "Blessed Virgin Mary Academy"="OTH"                ,
"Bishop Grimes Jr."="OTH",        "Hughes Elementary School"="E"                    ,
"Manlius Pebble Hill"="OTH",      "Porter Elementary School"="E"                    ,
"Syracuse Academy Middle At St. Charles"="OTH",   "Bishop Ludden Jr."="OTH"                          ,
"Bellevue Elementary School"="E" ,       "Dr. King Elementary School"="E"                  ,
"Dr. Weeks Elementary School"="E" ,      "Meachem Elementary School"="E"                   ,
"Seymour Dual Language Academy"="E" ,     "McKinley - Brighton Elementary School"="E"       ,
"Webster Elementary School"="E" ,         "Van Duyn Elementary School" ="E"                 ,
"Franklin Elementary School"="E" ,        "Delaware Academy"="E"                            ,
"GPS Elementary Program"="ALT",       "Blessed Sacrament"="OTH"                         ,
"Cathedral Academy at Pompei"="OTH",  "Lemoyne Elementary School"="E"                   ,
"Eagle Wings Academy"="OTH",         "Bishop Academy at Most Holy Rosary"="OTH"         ,
"Syracuse Latin School"="E"         ,    "Syracuse Academy Elementary At St. James"="OTH"   ,
"BOCES OCM Stellata"="OTH",         "Delaware Primary School"="E"                     ,
"St. Margaret"="OTH",                 "Word of Life"="OTH"                               ,
"Parkside ARC School"="OTH",         "Little Lukes" ="OTH"                              ,
"OCS Rockwell Program "="OTH",          "Jowonio"="OTH"                                    ,
"Thrive By 5"="OTH",                   "Spice @ Elmcrest"="OTH"                           ,
"N Syr Early Ed Progr"="OTH",          "The New School"="OTH"                             ,
"Hear 2 Learn "="OTH",                "Liberty Post"="OTH"                               ,
"Enable"="OTH",                       "Connections"="OTH"                                ,
"Children's Therapy Network"="OTH",   "All Saints Elementary"="OTH"                      ,
"Holy Cross"="OTH",                  "CPSE Pre-K"="OTH"                                 ,
"DEC"="OTH",                         "Ihsan School of Excellence"="OTH"))


BAGSbyQuarterFinal<-BAGSbyQuarter%>%
  filter(!SchoolBand=="OTH")
################################################################################

PZOctoberRawData<-select(BAGSbyQuarterFinal,student_ID,StudentName,All.Year,FourPlus,Grade,EnrollmentSchoolName,GradeBand,SchoolBand,BehaviorFlag,AttendanceFlag,
                         StateTestImproveELA,StateTestImproveMath,GradeLevel,FinalGrowth,MP1DaysEnrolled,MP1DaysAbsent,MP1Percent,MP2DaysEnrolled,MP2Percent,
                         MP3DaysEnrolled,MP3DaysAbsent,MP3Percent,MP4DaysEnrolled,MP4DaysAbsent,MP4Percent,TotalDaysEnrolled,TotalDaysAbsent,
                         TotalAttendancePercentage,MP1Referrals,MP1ClassroomIncidents,MP1OSS,MP1OSSDays,MP2Referrals,MP2ClassroomIncidents,MP2OSS,MP2OSSDays,
                         MP3Referrals,MP3ClassroomIncidents,MP3OSS,MP3OSSDays,MP4Referrals,MP4ClassroomIncidents,MP4OSS,MP4OSSDays,
                         TotalReferral,TotalClassroom,TotalOSS,TotalOSSDays,MP1PassPercent,MP1Fail,MP2PassPercent,MP2Fail,MP3PassPercent,MP3Fail,MP4PassPercent,
                         MP4Fail,FinalPassPercent,FinalFail,ELAPerform2016,ELAScale2016,MathPerform2016,MathScale2016,ELAPerform2017,ELAScale2017,MathPerform2017,
                         MathScale2017,Dropout)



write.csv(PZOctoberRawData, "C:/Users/drobil66/Desktop/RFiles/R Reports/PZRequestMagnarelliOct2017.csv") 





write.csv(BAGSbyQuarter, "C:/Users/drobil66/Desktop/RFiles/BAGSbyquarter.csv") 
