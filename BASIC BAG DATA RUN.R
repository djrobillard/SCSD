library(stringi)
library(stringr)

library(tidyverse)

#read files
ehout2018<-read.csv("ehout2018.csv")
attendancetotab<-read.csv("attendtotab.csv",strip.white = TRUE)
enrollment<-read.csv("attendance2018.csv",strip.white = TRUE)
SEL_cleaned<-read.csv("SEL_cleaned.csv")
SM_cleaned<-read.csv("SM_cleaned.csv")
SR_cleaned<-read.csv("SR_cleaned.csv")
McKinneyVento<-read.csv("MKV.csv",strip.white = TRUE)
MarkReporting<-read.csv("MarkReporting2018.csv",strip.white = TRUE)
#ehout
EhoutClean<-ehout2018%>%
  rename(student_id=OffenderID,OSSDays=OSS.Days)%>%
  mutate(RefCount=ifelse(IncidentType=="referral",1,0))%>%
  mutate(ClassCount=ifelse(IncidentType=="classroom",1,0))%>%
  group_by(student_id)%>%
  summarise(OSS=sum(OSS),Referrals=sum(RefCount),ClassroomIncidents=sum(ClassCount),
            OSSDays=sum(OSSDays))%>%
  select(student_id,Referrals,ClassroomIncidents,OSS,OSSDays)
    
#attendance
attendance2018<-attendancetotab %>%
  filter(End=="11/08/2017") %>%
  select(Attendance.Percentage,student_id,DaysEnrolled,DailyAbsences)%>%
  rename(student_id=student_id)

#cleaning STAR information
SEL<-SEL_cleaned%>%
  select(Student.ID,F17EL_ScreeningCategoryGroupAdjustment)%>%
  rename("Fall Early Literacy"=F17EL_ScreeningCategoryGroupAdjustment,student_id=Student.ID)
SR<-SR_cleaned%>%
  select(Student.ID,F17Read_ScreeningCategoryGroupAdjustment)%>%
  rename("Fall STAR Reading"=F17Read_ScreeningCategoryGroupAdjustment,student_id=Student.ID)
SM<-SM_cleaned%>%
  select(Student.ID,F17Math_ScreeningCategoryGroupAdjustment)%>%
  rename("Fall STAR Math"=F17Math_ScreeningCategoryGroupAdjustment,student_id=Student.ID)
StarClean<-merge(SEL,SM,by="student_id",all=TRUE)
StarClean<-merge(StarClean,SR,by="student_id",all=TRUE)
StarClean[is.na(StarClean)]<-NoScore

#enrollment and demographics
enrollmentclean<-enrollment%>%
  filter(Current.Status=="A")%>%
  rename(student_id=Student.Id) %>%
  rename(Grade=Curr.Grade.Lvl) %>%
  rename(Building=Attendance.Location.Name) %>%
  mutate(Ethnicity=str_replace_all(Rptg.Race.Ethnicity.Desc,c("Black or African American"="B","Asian"="A",
                                       "White"="W","Multiracial"="M","Hispanic"="H",
                                       "American Indian or Alaska native"="I",
                                       "Native Hawaiian / Other Pacific Islander"="P"))) %>%
  mutate(ENL=str_replace_all(Eng.Proficiency,c("Proficient"="1","Advanced"="1","Beginner"="1","Intermediate"="1",
                           "Transitioning"="1","Commanding"="1","Entering"="1","Emerging"="1","Expanding"="1",
                           "b"="1")))%>%
  mutate(IEP=str_replace_all(Has.Iep,c("Y"="1","N"="0")))%>%
  mutate(StudentName=paste(Student.Last.Nm,Student.First.Nm,sep=", "))%>%
  mutate(Gender=str_replace_all(Student.Gender,c("Female"="F","Male"="M")))%>%
  select(student_id,StudentName,Building,Grade,Ethnicity,IEP,ENL,Gender) %>%
  mutate(Building=str_replace_all(Building,c("P-Tech"="Institute of Technology at Syracuse Central",
                                     "Hurlbut W. Smith Elementary School"="HW Smith K8",
                                      "Hurlbut W. Smith Middle School"="HW Smith K8",
                                     "Frazer Middle School"="Frazer K8",
                                     "Frazer Elementary School"="Frazer K8",
                                     "Hughes Elementary School"="Syracuse Latin - Hughes",
                                     "Syracuse Latin School"="Syracuse Latin - Hughes",
                                     "Montessori School @ Lemoyne"="Lemoyne Elementary School",
                                     "GPS Elementary Program"="MSAP - CORE",
                                     "Huntington Middle School"="Huntington K8",
                                     "Huntington Elementary School"="Huntington K8",
                                     "Delaware Academy"="Delaware Academy - Primary",
                                     "Delaware Primary School"="Delaware Academy - Primary",
                                     "Twilight Academy @ Corcoran"="Corcoran High School",
                                     "Twilight Academy @ Nottingham" = "Nottingham High School",
                                     "Twilight Academy @ PSLA" = "Public Service Leadership Academy at Fowler",
                                     "Twilight Academy @ Henninger" = "Henninger High School",
                                     "Roberts Elementary School" = "Roberts K8",
                                     "Roberts Middle School"="Roberts K8",
                                     "Edward Smith Elementary School"="Ed Smith K8",
                                     "Edward Smith Middle"="Ed Smith K8",
                                     "JVC EPIC Program"="Johnson Center",
                                     "Johnson Center Transition Program"="Johnson Center")))%>%
  mutate(Grade=str_replace_all(Grade,c("U8"="8","U1"="1","U2"="2","U3"="3","U4"="4","U5"="5","U6"="6",
                               "U7"="7","UK"="KF")))%>%
  unique()

#McKinney-Vento
MKV<-McKinneyVento %>%
  mutate(McKinneyVento=1)%>%
  mutate(End.Date=as.numeric(End.Date))%>%
  filter(End.Date==1)%>%
  select(Student.Id,McKinneyVento)%>%
  rename(student_id=Student.Id)%>%
  unique()

#####################################
#####markreporting
MarkReportingClean <- MarkReporting %>%
  mutate(Class.Average=as.numeric(Class.Average))%>%
  mutate(Failing=ifelse(Class.Average<65,1,0))%>%
  group_by(student_id)%>%
  summarise(NumberofFailingCourses=sum(Failing))%>%
  select(student_id,NumberofFailingCourses)

#merging all
final<-left_join(enrollmentclean,EhoutClean,by="student_id")
final<-left_join(final,MarkReportingClean,by="student_id")
final<-left_join(final,StarClean,by="student_id")
final<-left_join(final,MKV,by="student_id")
final<-left_join(final,attendance2018,by="student_id")
final[is.na(final)]<-0



#organizing table
final1<-final%>%
  mutate(BehaviorFlag=ifelse(Referrals>=3,1,ifelse(ClassroomIncidents>=1,1,0)))%>%
  mutate(AttendanceFlag=ifelse(Attendance.Percentage<=.9,1,0))%>%
  mutate(SecondaryGradeFlag=ifelse(NumberofFailingCourses>0,1,0))%>%
  mutate(ReadingLevelFlag=ifelse('Fall Star Reading'=="Urgent Intervention",1,ifelse('Fall Early Literacy'=="Urgent Intervention",1,0)))%>%
  mutate(TotalFlags=BehaviorFlag+AttendanceFlag+SecondaryGradeFlag+ReadingLevelFlag)%>%
  select(StudentName,Grade,IEP,ENL,Ethnicity,Gender,
         McKinneyVento,TotalFlags,BehaviorFlag,AttendanceFlag,SecondaryGradeFlag,
         ReadingLevelFlag,Referrals, ClassroomIncidents,OSS,OSSDays,DaysEnrolled, 
         DailyAbsences,Attendance.Percentage,NumberofFailingCourses,
         `Fall STAR Reading`, `Fall STAR Math`,`Fall Early Literacy`,student_id,Building)%>%
  mutate(Attendance.Percentage=paste(round((Attendance.Percentage)*100,digits=1),"%",sep=""))

final1$Building<-strtrim(final1$Building, 31)


#exporting to csv
write.csv(final1, "C:/Users/drobil66/Desktop/RFiles/R Reports/BAGS1030.csv")