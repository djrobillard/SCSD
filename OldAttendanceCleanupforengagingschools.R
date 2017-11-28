library(reshape2)
library(lubridate)
library(plyr)
library(dplyr)

oldattendance1<-read.csv("YearlyAttendance.csv")

oldattendance<-filter(oldattendance1,school_year=="2016"|school_year=="2015"|school_year=="2014")

oldattendance<-filter(oldattendance,EnrollmentSchoolName=="Nottingham High School"|EnrollmentSchoolName=="Henninger High School"|
                        EnrollmentSchoolName=="Institute of Technology at Syracuse Central"|
                        EnrollmentSchoolName=="Roberts K-8 School"|EnrollmentSchoolName=="Edward Smith K-8 School"|
                        EnrollmentSchoolName=="Lincoln Middle School"|
                        EnrollmentSchoolName=="P-Tech"|EnrollmentSchoolName=="Dr. Weeks Elementary School")
oldattendance<-filter(oldattendance,summer_school!="Y")
oldattendance<-select(oldattendance,school_year,student_id,Ethnicity,ELL,IEP,EnrollmentSchoolName,SepEnrolled,SepDailyAbs,
                      NovEnrolled,NovDailyAbs,OctEnrolled,OctDailyAbs,DecEnrolled,DecDailyAbs,JanEnrolled,JanDailyAbs,
                     FebEnrolled,FebDailyAbs,MarEnrolled,MarDailyAbs,AprEnrolled,AprDailyAbs,MayEnrolled,MayDailyAbs)
oldattendance$Q1Enrolled<-(oldattendance$SepEnrolled+oldattendance$OctEnrolled)
oldattendance$Q1Absent<-(oldattendance$SepDailyAbs+oldattendance$OctDailyAbs)
oldattendance$Q2Enrolled<-(oldattendance$NovEnrolled+oldattendance$DecEnrolled+oldattendance$JanEnrolled)
oldattendance$Q2Absent<-(oldattendance$NovDailyAbs+oldattendance$DecDailyAbs+oldattendance$JanDailyAbs)
oldattendance$Q3Enrolled<-(oldattendance$FebEnrolled+oldattendance$MarEnrolled)
oldattendance$Q3Absent<-(oldattendance$FebDailyAbs+oldattendance$MarDailyAbs)
oldattendance$Q4Enrolled<-(oldattendance$AprEnrolled+oldattendance$MayEnrolled)
oldattendance$Q4Absent<-(oldattendance$AprDailyAbs+oldattendance$MayDailyAbs)

Q1Enrolled<-aggregate(Q1Enrolled~school_year+ELL+IEP+Ethnicity,data=oldattendance,FUN=sum)
Q1Absent<-aggregate(Q1Absent~school_year+ELL+IEP+Ethnicity,data=oldattendance,FUN=sum)
Q2Enrolled<-aggregate(Q2Enrolled~school_year+ELL+IEP+Ethnicity,data=oldattendance,FUN=sum)
Q2Absent<-aggregate(Q2Absent~school_year+ELL+IEP+Ethnicity,data=oldattendance,FUN=sum)
Q3Enrolled<-aggregate(Q3Enrolled~school_year+ELL+IEP+Ethnicity,data=oldattendance,FUN=sum)
Q3Absent<-aggregate(Q3Absent~school_year+ELL+IEP+Ethnicity,data=oldattendance,FUN=sum)
Q4Enrolled<-aggregate(Q4Enrolled~school_year+ELL+IEP+Ethnicity,data=oldattendance,FUN=sum)
Q4Absent<-aggregate(Q4Absent~school_year+ELL+IEP+Ethnicity,data=oldattendance,FUN=sum)

oldattend<-merge(Q1Enrolled,Q1Absent,by=c("school_year","ELL","IEP","Ethnicity"))
oldattend<-merge(oldattend,Q2Enrolled,by=c("school_year","ELL","IEP","Ethnicity"))
oldattend<-merge(oldattend,Q2Absent,by=c("school_year","ELL","IEP","Ethnicity"))
oldattend<-merge(oldattend,Q3Enrolled,by=c("school_year","ELL","IEP","Ethnicity"))
oldattend<-merge(oldattend,Q3Absent,by=c("school_year","ELL","IEP","Ethnicity"))
oldattend<-merge(oldattend,Q4Enrolled,by=c("school_year","ELL","IEP","Ethnicity"))
oldattend<-merge(oldattend,Q4Absent,by=c("school_year","ELL","IEP","Ethnicity"))

oldattend<-rename(oldattend,SCHOOL_YEAR=school_year)
#######################################################################################
#Attendance YTD and by quarter for elementary, middle and high schools ####################################################################
attendance<-read.csv("attendancedaily.csv")

attendance<-filter(attendance,name=="Nottingham High School"|name=="Henninger High School"|
                        name=="Institute of Technology at Syracuse Central"|
                     name=="Roberts K-8 School"|name=="Edward Smith K-8 School"|
                     name=="Lincoln Middle School"|
                     name=="P-Tech"|name=="Dr. Weeks Elementary School")
attendance<-rename(attendance,SCHOOL_YEAR=ï..School_Year)

####adding demographic data
enrollment<-read.csv("enrollmentallyears.csv")
enroll1<-rename(enrollment,SCHOOL_YEAR=ï..SCHOOL_YEAR)
enroll1<-filter(enroll1,SCHOOL_YEAR=="2017")
enroll1<-select(enroll1,SCHOOL_YEAR,STUDENT_ID,IEP,ELL,Ethnicity)

attendance<-merge(attendance,enroll1,by=c("STUDENT_ID","SCHOOL_YEAR"),all=TRUE)


#convert date to correct format

###parse dates and create time intervals
attendance$date <- as.Date(attendance$cal_date) # puts into date format
attendance$year<-year(attendance$date)
attendance$month<-month(attendance$date)
attendance$month<-sprintf("%02d",attendance$month)
attendance$day<-day(attendance$date)
attendance$day<-sprintf("%02d",attendance$day)

attendance$date.num <- as.numeric(paste0(attendance$year, attendance$month, attendance$day)) 

attendance$MP<-ifelse(attendance$date.num>=20160815 & attendance$date.num<20161127, 1, 
                      ifelse(attendance$date.num>=20161127 & attendance$date.num<20170127, 2, 
                             ifelse(attendance$date.num>=20170127 & attendance$date.num<20170407, 3, 
                                    ifelse(attendance$date.num>=20170407 & attendance$date.num<20170622, 4, 0)
                             )
                      )
)


#change colname
colnames(attendance)[which(names(attendance)=="Period.Absence..")]<-"PeriodAbsencePercent"


#add column for each day
attendance$inccount = 1
#identify each day as absent or not
attendance$Absent<-ifelse(as.numeric(attendance$'PeriodAbsencePercent')<1,0,1)


#aggregate enrolled days and total absences
enrolled<- aggregate(inccount~MP+IEP+ELL+Ethnicity, data = attendance, FUN = sum)
absent<- aggregate(Absent~MP+IEP+ELL+Ethnicity, data = attendance, FUN = sum)

#mergeintoMP Files
attendance17<-merge(enrolled,absent,by=c("MP","IEP","ELL","Ethnicity"),all=TRUE)
attendance17$SCHOOL_YEAR<-2017

attendance18<-reshape(attendance17, idvar=c("SCHOOL_YEAR","IEP","ELL","Ethnicity"), timevar="MP", direction="wide")

#change column name
attendance18<-rename(attendance18,Q1Enrolled=inccount.1)
attendance18<-rename(attendance18,Q2Enrolled=inccount.2)
attendance18<-rename(attendance18,Q3Enrolled=inccount.3)
attendance18<-rename(attendance18,Q4Enrolled=inccount.4)
attendance18<-rename(attendance18,Q1Absent=Absent.1)
attendance18<-rename(attendance18,Q2Absent=Absent.2)
attendance18<-rename(attendance18,Q3Absent=Absent.3)
attendance18<-rename(attendance18,Q4Absent=Absent.4)


#merge all together
allattendance<-rbind(attendance18,oldattend)

write.csv(allattendance,"C:/Users/drobil66/Desktop/RFiles/R Reports/EngagingSchoolsAttendance.csv")