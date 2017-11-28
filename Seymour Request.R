library(lubridate)
library(plyr)
library(dplyr)

rawattendance<-read.csv("attendance2018.csv")

cleanattendance <- rawattendance %>%
  filter(Attendance.Location.Name=="Seymour Dual Language Academy")%>%
  mutate(Day=1)%>%
  mutate(Pres=ifelse(Attendance.Desc=="Present",1,0))%>%
  mutate(Abs=ifelse(Pres==0,1,0))%>%
  mutate(StudentName=paste(Student.Last.Nm,Student.First.Nm,sep=", "))%>%
  group_by(Student.Id,StudentName,Rptg.Race.Ethnicity.Desc,Has.Iep,Male) %>%
  summarise(TotalEnrolled=sum(Day),DaysPresent=sum(Pres),DaysAbsent=sum(Abs))%>%
  mutate(AttendancePercentage=DaysPresent/TotalEnrolled)%>%
  mutate(AttendancePercentage=(paste(round((AttendancePercentage)*100,digits=1),"%",sep="")))%>%
  rename(Race=Rptg.Race.Ethnicity.Desc,Gender=Male,IEP=Has.Iep)%>%
  select(Student.Id,StudentName,Race,Gender,IEP,TotalEnrolled,DaysPresent,AttendancePercentage)


Demo<-rawattendance %>%
  filter(Attendance.Location.Name=="Seymour Dual Language Academy")%>%
  select(Student.Id,Student.Last.Nm,Student.First.Nm,Male,Rptg.Race.Ethnicity.Desc,Has.Iep)%>%
  mutate(StudentName=paste(Student.Last.Nm,Student.First.Nm,sep=","))
  

rawattendance$Percent<-as.numeric(rawattendance$'Period.Absence..'>.5)
rawattendance$daycount = 1
rawattendance$Month<-month(rawattendance$cal_date)

rawattendance$Month<-month.abb[rawattendance$Month]

YTDdays<-as.data.frame(aggregate(daycount ~ STUDENT_ID,  data=rawattendance, FUN=sum))
YTDabsent<-as.data.frame(aggregate(Absent ~ STUDENT_ID,  data=rawattendance, FUN=sum))
YTD<-merge(YTDdays,YTDabsent,by="STUDENT_ID",all=TRUE)
YTD$YTDAtt<-((YTD$daycount-YTD$Absent)/(YTD$daycount))
YTD$YTDAtt <- paste(round((YTD$YTDAtt)*100,digits=1),"%",sep="")

YTD<-YTD[,c("STUDENT_ID","YTDAtt")]

totaldays<-as.data.frame(aggregate(daycount ~ STUDENT_ID + Month,  data=rawattendance, FUN=sum))
totalabsent<-as.data.frame(aggregate(Absent ~ STUDENT_ID + Month,  data=rawattendance, FUN=sum))
together<-merge(totaldays,totalabsent, by=c("STUDENT_ID","Month"),all=TRUE)



together$Percentage<-((together$daycount-together$Absent)/(together$daycount))

together$Percentage <- paste(round((together$Percentage)*100,digits=1),"%",sep="")

together<-together[,c("STUDENT_ID","Month","Percentage")]

togetherfinal<-reshape(together, idvar="STUDENT_ID", timevar="Month", direction="wide")

togetherfinal<-rename(togetherfinal,DecAtt=Percentage.Dec,
                      FebAtt=Percentage.Feb,
                      JanAtt=Percentage.Jan,
                      NovAtt=Percentage.Nov,
                      OctAtt=Percentage.Oct,
                      SepAtt=Percentage.Sep,
                      MarAtt=Percentage.Mar,
                      AprAtt=Percentage.Apr,
                      MayAtt=Percentage.May,
                      JuneAtt=Percentage.Jun)
togetherfinal<-merge(togetherfinal,YTD,by="STUDENT_ID",all=TRUE)

rawattendance<-togetherfinal[,c("STUDENT_ID","YTDAtt","SepAtt","OctAtt","NovAtt","DecAtt","JanAtt","FebAtt","MarAtt","AprAtt","MayAtt","JuneAtt")]

### STAR DATA

#STAR Data

#load all files
FSM<-read.csv("Fall16SM_cleaned.csv")
FSR<-read.csv("Fall16SR_cleaned.csv")
FSEL<-read.csv("Fall16SEL_cleaned.csv")
WSM<-read.csv("WinterSM_cleaned.csv")
WSR<-read.csv("WinterSR_cleaned.csv")
WSEL<-read.csv("WinterSEL_cleaned.csv")

#cleaning STAR information
FSEL<-FSEL[c("F16EL_StudentLocalID","F16EL_LiteracyClassification")]
FSM<-FSM[c("F16Math_StudentLocalID","F16Math_GradeEquivalent")]
FSR<-FSR[c("F16Read_StudentLocalID","F16Read_GradeEquivalent")]
WSM<-WSM[c("Win16Math_StudentLocalID","Win16Math_GradeEquivalent")]
WSR<-WSR[c("Win16Read_StudentLocalID","Win16Read_GradeEquivalent")]
WSEL<-WSEL[c("Win16EL_StudentLocalID","Win16EL_LiteracyClassification")]


#change column names
colnames(FSM)[which(names(FSM)=="F16Math_GradeEquivalent")]<-"Fall Math"
colnames(FSM)[which(names(FSM)=="F16Math_StudentLocalID")]<-"STUDENT_ID"
colnames(FSR)[which(names(FSR)=="F16Read_GradeEquivalent")]<-"Fall Reading"
colnames(FSR)[which(names(FSR)=="F16Read_StudentLocalID")]<-"STUDENT_ID"
colnames(FSEL)[which(names(FSEL)=="F16EL_StudentLocalID")]<-"STUDENT_ID"
colnames(FSEL)[which(names(FSEL)=="F16EL_LiteracyClassification")]<-"Fall EL"
colnames(WSM)[which(names(WSM)=="Win16Math_GradeEquivalent")]<-"Winter Math"
colnames(WSM)[which(names(WSM)=="Win16Math_StudentLocalID")]<-"STUDENT_ID"
colnames(WSR)[which(names(WSR)=="Win16Read_GradeEquivalent")]<-"Winter Reading"
colnames(WSR)[which(names(WSR)=="Win16Read_StudentLocalID")]<-"STUDENT_ID"
colnames(WSEL)[which(names(WSEL)=="Win16EL_LiteracyClassification")]<-"Winter EL"
colnames(WSEL)[which(names(WSEL)=="Win16EL_StudentLocalID")]<-"STUDENT_ID"

#merging
StarbyMP<-merge(FSM,FSR,by="STUDENT_ID", all=TRUE)
StarbyMP<-merge(StarbyMP,FSEL,by="STUDENT_ID",all=TRUE)
StarbyMP<-merge(StarbyMP,WSM,by="STUDENT_ID",all=TRUE)
StarbyMP<-merge(StarbyMP,WSR,by="STUDENT_ID",all=TRUE)
StarbyMP<-merge(StarbyMP,WSEL,by="STUDENT_ID",all=TRUE)


####
#getting columns from enrollment
enroll1<-read.csv("enrollmentallyears.csv",strip.white = TRUE)
enroll1<-rename(enroll1,SchoolCode=EnrollmentSchoolID)

enroll1<-filter(enroll1,SCHOOL_YEAR=="2017")
enroll1$Active<-ifelse(enroll1$WITHDRAWAL_CODE=="",1,0)
enroll<-filter(enroll1,Active=="1")


##student names
Demo<-read.csv("Demographics.csv")
Demo<-select(Demo,STUDENT_ID,LastName,FirstName)


#merging all four
final1<-merge(enroll,rawattendance,by="STUDENT_ID",all=TRUE)
final<-merge(final1,togetherfinal,by="STUDENT_ID",all=TRUE)
final<-merge(final,Demo,by="STUDENT_ID",all=TRUE)



#cleanup
Report<-subset(final1,EnrollmentSchoolName=="Seymour Dual Language Academy")
Report<-Report[,c("STUDENT_ID","LastName","FirstName","IEP","Gender","Grade","ELL","Ethnicity","YTDAtt","SepAtt","OctAtt","NovAtt","DecAtt","JanAtt","FebAtt","MarAtt","AprAtt","MayAtt","JuneAtt")]
Report<-sapply(Report, as.character)

Report[is.na(Report)]<-" "

write.csv(cleanattendance,"C:/Users/drobil66/Desktop/RFiles/R Reports/SeymourAttendance2018.csv")

