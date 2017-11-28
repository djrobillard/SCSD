library(plyr)
library(dplyr)

attendanceallyears<-read.csv("attendanceallyears.csv",strip.white = TRUE)

cleanattendance<-attendanceallyears %>%
  filter(!summer_school=="Y")%>%
  filter(!school_year=="2017")%>%
  filter(!EnrollmentBuildingType %in% c("PK","NPUB","CS","JAIL","CLSD","SS","PS",
                                        "ADM","GED","OOD","HOME","PSOTH","RF"))%>%
  group_by(school_year,student_id)%>%
  summarise(TotalDaysEnrolled=sum(DaysEnrolled),TotalDaysAbsent=sum(DailyAbsences))%>%
  mutate(Percentage=((TotalDaysEnrolled-TotalDaysAbsent)/(TotalDaysEnrolled)))%>%
  mutate(Chronic=ifelse(Percentage<.9,1,0))%>%
  mutate(Total=1)%>%
  select(school_year,Chronic,Total)%>%
  group_by(school_year) %>%
  summarise(NumberChronic=sum(Chronic),TotalStudents=sum(Total))%>%
  mutate(ChronicPercent=(NumberChronic/TotalStudents))%>%
  mutate(ChronicPercent=round(ChronicPercent,4))
  
write.csv(cleanattendance,"C:/Users/drobil66/Desktop/ChronicforPZ.csv") #write table to xlsx


#3-8 Tests
sixteen<-read.csv("38Test2016.csv")
fifteen<-read.csv("38Test2015.csv")

cleansix<-sixteen %>%
  select(Report.Subject,Student.ID,Grade,Score)%>%
  rename(Score16=Score)%>%
  rename(Grade16=Grade)

cleanfive<-fifteen %>%
  select(Report.Subject,Student.ID,Grade,Score)%>%
  rename(Score15=Score)%>%
  rename(Grade15=Grade)

Together<-merge(cleansix,cleanfive,by=c("Student.ID","Report.Subject"),all=TRUE)

Together<-Together[complete.cases(Together),]
Answers<-Together %>%
  mutate(Improve=ifelse((Score16-Score15)>0,1,0))%>%
  mutate(FourFive=ifelse(Grade16=="4"|Grade16=="5",1,0))%>%
  mutate(SixEight=ifelse(Grade16=="6"|Grade16=="7"|Grade16=="8",1,0))%>%
  filter(!Grade16=="3")%>%
  mutate(Category=ifelse(SixEight=="1",1,0))%>%
  group_by(Report.Subject,Category)%>%
  summarise(TotalImprove=sum(Improve),TotalFourFive=sum(FourFive),TotalSixEight=sum(SixEight))%>%
  mutate(FourFiveImprovePercent=TotalImprove/TotalFourFive)%>%
  mutate(SixEightImprovePercent=TotalImprove/TotalSixEight)


write.csv(Answers,"C:/Users/drobil66/Desktop/38ForPZ.csv") #write table to xlsx


