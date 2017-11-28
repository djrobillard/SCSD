library(plyr)
library(dplyr)

#3-8 Tests
sixteen<-read.csv("38Test2016.csv")
seventeen<-read.csv("38Test2017.csv")

sixteen$Report.Subject<-revalue(sixteen$Report.Subject,c("Mathematics"="MATH"))


cleansix<-sixteen %>%
  select(Report.Subject,Student.ID,Grade,Score,Standard.Achieved)%>%
  rename(Score16=Score)%>%
  rename(Proficiency16=Standard.Achieved)

cleanseven<-seventeen %>%
  select(SUBJECT,STUDENT_ID,NUMERIC_SCORE,STANDARD_ACHIEVED,LOCATION_NAME)%>%
  rename(Score17=NUMERIC_SCORE)%>%
  rename(Student.ID=STUDENT_ID)%>%
  rename(Report.Subject=SUBJECT)%>%
  rename(Proficiency17=STANDARD_ACHIEVED)

Together<-merge(cleansix,cleanseven,by=c("Student.ID","Report.Subject"),all=TRUE)

Together<-Together[complete.cases(Together),]

Answers<-Together %>%
  mutate(ScoreImprove=ifelse((Score17-Score16)>0,1,0))%>%
  mutate(Count=1)%>%
  mutate(ImproveProficiency=ifelse(Proficiency16 %in% c("Level 4") & Proficiency17 %in% c("Level 4"),1,
         ifelse(Proficiency16 %in% c("Level 3") & Proficiency17 %in% c("Level 3","Level 4"),1,
                ifelse(Proficiency16 %in% c("Level 2") & Proficiency17 %in% c("Level 2", "Level 3","Level 4"),1,
                       ifelse(Proficiency16 %in% c("Level 1") & Proficiency17 %in% c("Level 2","Level 3","Level 4"),1,0)))))%>%
  select(Student.ID,Count,Report.Subject,ScoreImprove,ImproveProficiency)

******************************
write.csv(alldiscipline, "C:/Users/drobil66/Desktop/RFiles/R Reports/alldiscipline.csv")

ehout2016<-read.csv("ehout2016.csv",strip.white = TRUE)
ehout2017<-read.csv("ehout2017.csv",strip.white = TRUE)

ehout16<-ehout2016 %>%
  mutate(count=1)%>%
  select(OffenderID,count,OSS,ISS)%>%
  group_by(OffenderID)%>%
  summarise(Ref16=sum(count),OSS16=sum(OSS),ISS16=sum(ISS))%>%
  rename(Student.ID=OffenderID)%>%
  replace(.,is.na(.),0)
  



ehout17<-ehout2017 %>%
  mutate(count=1)%>%
  filter(IncidentType=="referral")%>%
  select(OffenderID,count,OSS,ISS)%>%
  group_by(OffenderID)%>%
  summarise(Ref17=sum(count),OSS17=sum(OSS),ISS17=sum(ISS))%>%
  rename(Student.ID=OffenderID)%>%
  replace(.,is.na(.),0)

Together<-filter(Together,LOCATION_NAME=="DANFORTH MIDDLE SCHOOL")


alldiscipline<-merge(ehout16,ehout17,by="Student.ID",all=TRUE)
alldiscipline[is.na(alldiscipline)]<-0


alldiscipline<-alldiscipline %>%
  mutate(ImproveRef=ifelse(Ref17-Ref16<1,1,0))%>%
  mutate(ImproveOSS=ifelse(OSS17-OSS16<1,1,0))%>%
  mutate(ImproveISS=ifelse(ISS17-ISS16<1,1,0))%>%
  select(Student.ID,ImproveRef,ImproveOSS,ImproveISS)


ehoutforclean<-ehout2017 %>%
  rename(STUDENT_ID=OffenderID)%>%
  rename(LOCATION_NAME=SchoolName)%>%
  select(STUDENT_ID,LOCATION_NAME)

everything<-merge(ehoutforclean,seventeen,by=c("STUDENT_ID","LOCATION_NAME"),all=TRUE)

everythingclean<-everything %>%
  select(STUDENT_ID,LOCATION_NAME)%>%
  filter(LOCATION_NAME %in% c("DANFORTH MIDDLE SCHOOL", "Danforth Middle School"))%>%
  select(STUDENT_ID)%>%
  rename(Student.ID=STUDENT_ID)%>%
  unique()

alldiscipline<-left_join(everythingclean,alldiscipline,by="Student.ID",all=TRUE)
DanforthProgress<-left_join(everythingclean,DanforthProgress,by="Student.ID",all=TRUE)

Danforth<-DanforthProgress %>%
  mutate(Count=1)%>%


Danforth$ImproveRef[is.na(Danforth$ImproveRef)]<-1
Danforth$ImproveOSS[is.na(Danforth$ImproveOSS)]<-1
Danforth$ImproveISS[is.na(Danforth$ImproveISS)]<-1
Danforth$ImproveProficiency[is.na(Danforth$ImproveProficiency)]<-Notest
Danforth$ImproveScore[is.na(Danforth$ImproveScore)]<-Notest



Danforth<-DanforthProgress %>%
  mutate(SchoolName="Danforth Middle School")%>%
  group_by(SchoolName)%>%
  summarise(TotalStudents=sum(Count),ImproveScore=sum(ScoreImprove),ImproveLevel=sum(ImproveProficiency),
            RefImprove=sum(ImproveRef),OSSImprove=sum(ImproveOSS),ISSImprove=sum(ImproveISS))%>%
  mutate(PercentScoreImprove=ImproveScore/TotalStudents)%>%
  mutate(PercentLevelImprove=ImproveLevel/TotalStudents)%>%
  mutate(PercentOSSImprove=OSSImprove/TotalStudents)%>%
  mutate(PercentISSImprove=ISSImprove/TotalStudents)%>%
  mutate(PercentRefImprove=RefImprove/TotalStudents)%>%
