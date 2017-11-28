MarkReporting<-read.csv("MarkReporting2018.csv")
Upward<-read.csv("UpwardBoundList.csv")

MarkReportingClean <- MarkReporting %>%
  select(student_id,Student.Name,Schd.School,Course.Name,MP.Start,Class.Average)

Combined<-left_join(Upward,MarkReportingClean,by="student_id",all=TRUE)

write.csv(Combined, "C:/Users/drobil66/Desktop/RFiles/R Reports/UpwardBound.csv")