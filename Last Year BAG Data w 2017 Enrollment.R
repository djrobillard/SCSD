#BAGS Data for Forthcoming Students in Your Building
#Get Enrollment Information from Eschool File
Enrollment<-Beginning.Year.2016.17.Enrollment
#clean up that file
colnames(Enrollment)[which(names(Enrollment)=="Student.ID")]<-"Student ID"
colnames(Enrollment)[which(names(Enrollment)=="Student.Name")]<-"Student Name"
#changing Final Table
FinalTable<-finaltable[,c("Student ID","Total Flags","Behavior Flag","Attendance Flag","Grade Flag","Total Referrals","Attendance Percentage","Total Failed Courses","Total OSS Incidents","Total OSS Days","Days Enrolled","Days Absent")]
#Merge with BAGS Data from Last Year
Enroll17BAGS<-merge(Enrollment,FinalTable,by="Student ID")
#clean up columns
colnames(Enroll17BAGS)[which(names(Enroll17BAGS)=="Has.Iep")]<-"IEP"
colnames(Enroll17BAGS)[which(names(Enroll17BAGS)=="Esl")]<-"ESL"
colnames(Enroll17BAGS)[which(names(Enroll17BAGS)=="Student.Name")]<-"Student Name"
#Clean Up to Get Updated Columns - SO NICE
FinalEnroll17BAGS<-Enroll17BAGS[,c("Student ID","Building","Grade.x","Total Flags","Behavior Flag","Attendance Flag","Grade Flag","Total Referrals","Attendance Percentage","Total Failed Courses","Total OSS Incidents","Total OSS Days","Days Enrolled","Days Absent")]
#send to Excel as a CSV OBVIOUSLY
write.csv(Enroll17BAGS, "C:/Users/drobil66/Desktop/RFiles/FinalEnroll17BAGS.csv") 