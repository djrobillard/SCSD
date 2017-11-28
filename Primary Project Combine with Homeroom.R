#run student list by homeroom in Cognos (Registration Reports - "Student List by Homeroom")
homeroom<-read.csv("homeroom.csv")
colnames(homeroom)[which(names(homeroom)=="Student.Id")]<-"Student ID"
colnames(homeroom)[which(names(homeroom)=="Student.Name.Last.First.Middle")]<-"Student Name"
colnames(homeroom)[which(names(homeroom)=="Name...Homeroom.Primary.Teacher")]<-"Homeroom Teacher"

#merge with BAGS by quarter
PPbyQuarter<-merge(homeroom,PPbyQuarter,"Student ID",all=TRUE)

#clean latest enrollment file
enrollment<-read.csv("enrollment 2017.csv")
colnames(enrollment)[which(names(enrollment)=="STUDENT_ID")]<-"Student ID"

enrollment<-enrollment[,c("Student ID","Grade","EnrollmentSchoolName")]

#merge with BAGS
BAGSbyQuarter<-merge(enrollment,BAGSbyQuarter,"Student ID",all=TRUE)

BAGSbyQuarter[is.na(BAGSbyQuarter)]<-0

write.csv(BAGSbyQuarter, "C:/Users/drobil66/Desktop/RFiles/BAGSbyquarter.csv") 