library(plyr)
library(dplyr)


Credits<-read.csv("StudentCreditsbyBuilding2.csv")
JCCredits<-filter(Credits,CourseSchoolID==670)
JCCredits$Attempted<-1
total.attempts.course<-as.data.frame(aggregate(ATT_CREDIT ~ STUDENT_ID + Course,data=JCCredits, FUN=sum))
total.earned.course<-as.data.frame(aggregate(EARN_CREDIT~STUDENT_ID+Course,data=JCCredits,FUN=sum))

AttwCredits<-merge(total.attempts.course,total.earned.course,by=c("STUDENT_ID","Course"))

Credits.small<-unique(select(Credits,Course,CourseName_CourseSession))

Att.Credits<-merge(AttwCredits,Credits.small,by="Course")


enroll1<-read.csv("enrollmentallyears.csv",strip.white = TRUE)
enroll<-filter(enroll1,ï..SCHOOL_YEAR=="2017")
enroll$Active<-ifelse(enroll$WITHDRAWAL_CODE=="",1,0)
enroll<-filter(enroll,Active=="1")
enroll<-select(enroll,STUDENT_ID,EnrollmentSchoolName,AccountabilitySchoolName,Grade,DateEnteredGrade9)

Att.Credits.Demo<-unique(merge(Att.Credits,enroll,by="STUDENT_ID"))
Att.Credits.Demo$More<-ifelse(Att.Credits.Demo$ATT_CREDIT>1,1,0)
Att.Credits.Demo<-filter(Att.Credits.Demo,ATT_CREDIT>1)
Att.Credits.Demo<-filter(Att.Credits.Demo,EARN_CREDIT==0)

##student names
Demo<-read.csv("Demographics.csv")
Demo<-select(Demo,STUDENT_ID,LastName,FirstName)

Att.Credits.Demo.Name<-merge(Demo,Att.Credits.Demo,by="STUDENT_ID")
Att.Credits.Demo.Name<-select(Att.Credits.Demo.Name,STUDENT_ID,LastName,FirstName,EnrollmentSchoolName,AccountabilitySchoolName,Grade,DateEnteredGrade9,ATT_CREDIT,EARN_CREDIT,CourseName_CourseSession,Course)