library(plyr)
library(dplyr)

#read files
ehout2018<-read.csv("ehout2018.csv")
attendancetotab<-read.csv("attendtotab.csv",strip.white = TRUE)
enrollment<-read.csv("CurrentEnrollment.csv",strip.white = TRUE)
SR_cleaned<-read.csv("starreading.csv")

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
  filter(End=="10/23/2017") %>%
  select(Attendance.Percentage,student_id,DaysEnrolled,DailyAbsences)%>%
  rename(student_id=student_id)

#cleaning STAR information
SR<-SR_cleaned%>%
  select(StudentLocalID,GradeEquivalent)%>%
  rename("Fall STAR Reading"=GradeEquivalent,student_id=StudentLocalID)
SR[is.na(SR)]<-""

#enrollment and demographics
enrollmentclean<-enrollment%>%
  rename(student_id=Student.Id) %>%
  rename(Grade=Curr.Grade.Lvl) %>%
  rename(Building=Enrollment.Location.Name1) %>%
  mutate(Ethnicity=revalue(Rptg.Race.Ethnicity.Desc,c("Black or African American"="B","Asian"="A",
                                                      "White"="W","Multiracial"="M","Hispanic"="H",
                                                      "American Indian or Alaska native"="I",
                                                      "Native Hawaiian / Other Pacific Islander"="P"))) %>%
  mutate(ENL=revalue(Eng.Proficiency,c("Proficient"="1","Advanced"="1","Beginner"="1","Intermediate"="1",
                                       "Transitioning"="1","Commanding"="1","Entering"="1","Emerging"="1",
                                       "b"="1")))%>%
  mutate(IEP=revalue(Has.Iep,c("Y"="1","N"="0")))%>%
  mutate(StudentName=paste(Student.Last.Nm,Student.First.Nm,sep=", "))%>%
  mutate(Gender=revalue(Student.Gender,c("Female"="F","Male"="M")))%>%
  select(student_id,StudentName,Building,Grade,Ethnicity,IEP,ENL,Gender,Current.Status) %>%
  mutate(Building=revalue(Building,c("P-Tech"="Institute of Technology at Syracuse Central",
                                     "Hurlbut W. Smith Elementary School"="HW Smith K8",
                                     "Hurlbut W. Smith Middle School"="HW Smith K8",
                                     "Frazer Middle School"="Frazer K8",
                                     "Frazer Elementary School"="Frazer K8",
                                     "Hughes Elementary School"="Syracuse Latin/Hughes",
                                     "Syracuse Latin School"="Syracuse Latin/Hughes",
                                     "Montessori School @ Lemoyne"="Lemoyne Elementary School",
                                     "GPS Elementary Program"="MSAP/CORE",
                                     "Huntington Middle School"="Huntington K8",
                                     "Huntington Elementary School"="Huntington K8",
                                     "Delaware Academy"="Delaware Academy/Primary",
                                     "Delaware Primary School"="Delaware Academy/Primary",
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
  mutate(Grade=revalue(Grade,c("U8"="8","U1"="1","U2"="2","U3"="3","U4"="4","U5"="5","U6"="6",
                               "U7"="7","UK"="KF")))


#merging all
final<-merge(enrollmentclean,EhoutClean,by="student_id",all=TRUE)
final<-merge(final,attendance2018,by="student_id",all=TRUE)
final<-merge(final,SR,by="student_id",all=TRUE)
final1<-final %>%
  filter(Current.Status=="A")%>%
  filter(Grade %in% c("05","5","06","6","08","8","09","9"))%>%
  mutate(Grade=as.numeric(Grade))%>%
  mutate(Attendance.Percentage=paste(round((Attendance.Percentage)*100,digits=1),"%",sep=""))%>%
  mutate(Gap=(Grade-`Fall STAR Reading`))%>%
  replace(.,is.na(.),0)%>%
  filter(!OSSDays==0)


write.csv(final1, "C:/Users/drobil66/Desktop/RFiles/R Reports/HopeRequestOc2017.csv")


