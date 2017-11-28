library(dplyr)


MarkReporting<-read.csv("MarkReporting.csv")
MarkReporting<-MarkReporting[,c("student_id","MP3")]


#identify courses and passinggrades grades
MP3Grades<-subset(MarkReporting,MarkReporting$MP3>0)
MP2Grades<-subset(MarkReporting,is.na(MarkReporting$Final.Mark))
MP3Grades$Fail<-ifelse(MP3Grades$MP3<65,1,0)
MP2Grades$MP2Fail<-ifelse(MP2Grades$MP2<65,1,0)

#Total for Each
MP3GradesFail<-aggregate(Fail~student_id,data=MP3Grades,FUN=sum)
FinalGradesFail<-aggregate(FinalFail~student_id,data=FinalGrades,FUN=sum)

#merge
AllGrades<-merge(MP2GradesFail,FinalGradesFail,"student_id")
AllGrades$TotalFail<-(AllGrades$MP2Fail+AllGrades$FinalFail)

#clean for merge
colnames(MP3GradesFail)[which(names(MP3GradesFail)=="Fail")]<-"Total Failed Courses Quarter 3"
