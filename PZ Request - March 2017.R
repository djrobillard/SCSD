#get all files necessary
ehout2016<-read.csv("ehout2016.csv")

SM<-read.csv("springmath.csv")
SEL<-read.csv("springel.csv")
SR<-read.csv("springread.csv")
attendance<-read.csv("attendancedaily2016.csv")

#load packages
library(plyr)
library(dplyr)


#ehout cleaning
referrals<-subset(ehout2016,IncidentType=="referral")
referrals$inccount<-1
referrals<-aggregate(cbind(inccount,OSS.Days)~OffenderID,data=referrals,FUN=sum)
referrals<-rename(referrals,STUDENT_ID=OffenderID)

#attendance
#only necessary columns
attendance<-attendance[c("STUDENT_ID","Period.Absence..","grade")]
#add column for each day
attendance$daycount = 1
#identify each day as absent or not
attendance$Absent<-ifelse(attendance$Period.Absence..>0,1,0) #change based on State/Local rule for attendance
#total days and total absences
enrolleddays <- aggregate(daycount ~ STUDENT_ID, data = attendance, FUN = sum)
absentdays<-aggregate(Absent ~ STUDENT_ID, data = attendance, FUN = sum)
#merge to one attendance percentage file
finalattendance<-merge(enrolleddays,absentdays,by="STUDENT_ID", all=TRUE)
#createattendancepercentagecolumn
finalattendance$totalpercent<-((finalattendance$daycount-finalattendance$Absent)/(finalattendance$daycount))

finalattendance$AttendanceGroup<-ifelse(finalattendance$totalpercent>=.9,1,
                            ifelse(finalattendance$totalpercent>=.8,2,3)
  )
finalattendance<-finalattendance[c("STUDENT_ID","AttendanceGroup","grade")]

###test score improvement

ST2016<-read.csv("38test2016.csv")
ST2015<-read.csv("38test2015.csv")
ST2015<-ST2015[c("Student.ID","Report.Subject","Score","Standard.Achieved")]
ST2016<-ST2016[c("Student.ID","Report.Subject","Score","Standard.Achieved")]

ST2016<-rename(ST2016,ProficiencyLevel=Standard.Achieved)
ST2015<-rename(ST2015,ProficiencyLevel=Standard.Achieved)

colnames(ST2015)[colnames(ST2015)=="Score"]<-"Score15"
colnames(ST2016)[colnames(ST2016)=="Score"]<-"Score16"

Reading15<-subset(ST2015,Report.Subject=="ELA")
Reading15<-rename(Reading15,ProficiencyLevelReading15=ProficiencyLevel)
Math15<-subset(ST2015,Report.Subject=="Mathematics")
Math15<-rename(Math15,ProficiencyLevelMath15=ProficiencyLevel)
Reading16<-subset(ST2016,Report.Subject=="ELA")
Reading16<-rename(Reading16,ProficiencyLevelReading16=ProficiencyLevel)
Math16<-subset(ST2016,Report.Subject=="Mathematics")
Math16<-rename(Math16,ProficiencyLevelMath16=ProficiencyLevel)

Reading15<-Reading15[c("Student.ID","Score15","ProficiencyLevelReading15")]
Math15<-Math15[c("Student.ID","Score15","ProficiencyLevelMath15")]
Reading16<-Reading16[c("Student.ID","Score16","ProficiencyLevelReading16")]
Math16<-Math16[c("Student.ID","Score16","ProficiencyLevelMath16")]


Reading<-merge(Reading15,Reading16,by="Student.ID",all=TRUE)
Math<-merge(Math15,Math16,by="Student.ID",all=TRUE)

Reading<-Reading[c("Student.ID","Score15","Score16","ProficiencyLevelReading16")]
Math<-Math[c("Student.ID","Score15","Score16","ProficiencyLevelMath16")]

Reading$ReadingImprovement<-ifelse(Reading$Score16-Reading$Score15>0,1,0)
Math$MathImprovement<-ifelse(Math$Score16-Math$Score15>0,1,0)

allgrowth<-merge(Reading, Math, by="Student.ID",all=TRUE)
Growth38<-allgrowth[c("Student.ID","ReadingImprovement","MathImprovement","ProficiencyLevelReading16","ProficiencyLevelMath16")]

Growth38<-rename(Growth38,STUDENT_ID=Student.ID)

###STAR
SM$MathGap<-as.numeric(SM$Sp16_CurrentGrade)-as.numeric(SM$Sp16_GradeEquivalent)
SR$ReadingGap<-as.numeric(SR$Sp16_CurrentGrade)-as.numeric(SR$Sp16_GradeEquivalent)

SM$MathGapFlag<-ifelse(SM$MathGap>=0,1,
                    ifelse(SM$MathGap>(-1),2,3)
                           )
SR$ReadingGapFlag<-ifelse(SR$ReadingGap>=0,1,
                   ifelse(SR$ReadingGap>(-1),2,3)
)                    

SM<-SM[c("Sp16_StudentLocalID","MathGapFlag")]
SR<-SR[c("Sp16_StudentLocalID","ReadingGapFlag")]
SR<-rename(SR,STUDENT_ID=Sp16_StudentLocalID)
SM<-rename(SM,STUDENT_ID=Sp16_StudentLocalID)

Star<-merge(SR,SM,by="STUDENT_ID",all=TRUE)

##### Dropout Information

Dropout<-read.csv("Dropout2016.csv")
Dropout$Dropout<-1
Dropout<-Dropout[c("STUDENT_ID","Dropout")]

###15-16 Mark Reporting

MarkReporting<-read.csv("MarkReporting2016.csv")
MarkReporting<-MarkReporting[,c("student_id","Final.Mark")]

#dropping courses w/o a final mark
MarkReporting<-subset(MarkReporting,MarkReporting$Final.Mark>=0)

#identify courses and passinggrades grades
MarkReporting$EnrolledCourse<-1
MarkReporting$Passed<-ifelse(MarkReporting$Final.Mark>=65,1,0)

#
MRFinal1<-aggregate(EnrolledCourse~student_id,data=MarkReporting,FUN=sum)
MRFinal2<-aggregate(Passed~student_id,data=MarkReporting,FUN=sum)
#
MRFinal<-merge(MRFinal1,MRFinal2,by="student_id",all=TRUE)
MRFinal<-rename(MRFinal,STUDENT_ID=student_id)

###bringing in PZ list
PZ<-read.csv("PZList.CSV")
PZ<-select(PZ,STUDEND_ID,Tier)
PZ<-rename(PZ,STUDENT_ID=STUDEND_ID)

#Enrollment
enrollment<-read.csv("enrollmentallyears.csv")

enrollment<-filter(enrollment,enrollmentschoolname %in% c("Lincoln Middle School", "Institute of Technology at Syracuse Central",
                                "Grant Middle School", "Seymour Dual Languate Academy", "Nottingham High School",
                                "Delaware Primary School", "Corcoran High School", "Westside Academy at Blodgett",
                                "Fowler High School", "Frazer K-8 School", "Huntington K-8 School",
                                "Public Service Leadership Academy at Fowler", "Hughes Elementary School",
                                "Dr. Weeks Elementary School", "Dr. King Elementary School",       
                                "Franklin Elementary School", "Hurlbut W. Smith K-8", "Webster Elementary School",
                                "Danforth Middle School", "Roberts K-8 School", "Henninger High School",
                                "Salem Hyde Elementary School", "Lemoyne Elementary School", "Porter Elementary School",
                                "Edward Smith K-8 School", "JVC EPIC Program", "Syracuse Latin School", "Bellevue Elementary School",
                                "Clary Middle School", "P-Tech", "Van Duyn Elementary School", "MSAP/CORE", "McCarthy at Beard School",
                                "Meachem Elementary School", "McKinley - Brighton Elementary School", "Elmcrest Children's Center School",
                                "Expeditionary Learning Middle School","Twilight Academy @ Corcoran",
                                "Twilight Academy @ Fowler", "Twilight Academy @ Henninger", 
                                "Twilight Academy @ Nottingham",
                                "Twilight Academy @ PSLA", "Johnson Center Transition Program"))

enroll16<-subset(enrollment,school_year=="2016")
enroll16<-select(enroll16,student_id)
enroll16<-rename(enroll16,STUDENT_ID=student_id)
########Merging everything

Final1<-merge(PZ,referrals,by="STUDENT_ID",all=TRUE)
Final2<-merge(Final1,Growth38,by="STUDENT_ID",all=TRUE)
Final3<-merge(Final2,Star,by="STUDENT_ID",all=TRUE)
Final4<-merge(Final3,Dropout,by="STUDENT_ID",all=TRUE)
Final5<-merge(Final4,MRFinal,by="STUDENT_ID",all=TRUE)
Final6<-merge(Final5,enroll16,by="STUDENT_ID",all=TRUE)
Final<-merge(Final6,finalattendance,by="STUDENT_ID",all=TRUE)

Final<-unique(Final,incomparables=FALSE)
###
write.csv(Final,"C:/Users/drobil66/Desktop/RFiles/R Reports/PZRequest.csv")

