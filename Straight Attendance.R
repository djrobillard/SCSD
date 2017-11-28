library(plyr)
library(dplyr)
library(scales)
library(xlsx)
library(lubridate)
library(eeptools)


#
attendance1<-read.csv("attendancedaily.csv")

attendance1$name<-revalue(attendance1$name, c(
  "Twilight Academy @ Fowler"="Fowler High School",
  "Twilight Academy @ Corcoran"="Corcoran High School",
  "Twilight Academy @ Nottingham" = "Nottingham High School",
  "Twilight Academy @ PSLA" = "Public Service Leadership Academy at Fowler",
  "Twilight Academy @ Henninger" = "Henninger High School"))

#only select columns
attendance<-attendance1[c("STUDENT_ID","cal_date","name","Period.Absence..")]
#change colname
colnames(attendance)[which(names(attendance)=="Period.Absence..")]<-"PeriodAbsencePercent"

#add column for each day
attendance$inccount = 1
#identify each day as absent or not
attendance$Present<-as.numeric(attendance$'PeriodAbsencePercent'<1)

#aggregate enrolled days and total absences
enrolled<- aggregate(inccount ~ STUDENT_ID+name, data = attendance, FUN = sum)
Present<- aggregate(Present ~ STUDENT_ID+name, data = attendance, FUN = sum)


#mergeintoMP Files
attendance2<-merge(enrolled,Present,by=c("STUDENT_ID","name"),all=TRUE)


#createattendancepercentagecolumn
attendance2$YTDPercentage<-(1-(attendance2$inccount-attendance2$Present)/(attendance2$inccount))


#clean names
colnames(attendance2)[which(names(attendance2)=="STUDENT_ID")]<-"Student.ID"

schoolattendance<-select(attendance2,Student.ID,name,YTDPercentage)

schoolattendance$chronic<-ifelse(schoolattendance$YTDPercentage>=.9,0,1)
schoolattendance$studentenrolled<-1

chronic<- aggregate(chronic ~ name, data = schoolattendance, FUN = sum)
studentenrolled<- aggregate(studentenrolled ~ name, data = schoolattendance, FUN = sum)
schoolattendance<-merge(studentenrolled,chronic,by="name",all=TRUE)
schoolattendance$chronicpercentage<-(schoolattendance$chronic/schoolattendance$studentenrolled)

####district attendance
denrolled<- aggregate(inccount ~ STUDENT_ID, data = attendance, FUN = sum)
dPresent<- aggregate(Present ~ STUDENT_ID, data = attendance, FUN = sum)
dattendance<-merge(denrolled,dPresent,by="STUDENT_ID")
dattendance$YTDPercentage<-(1-(dattendance$inccount-dattendance$Present)/(dattendance$inccount))
colnames(dattendance)[which(names(dattendance)=="STUDENT_ID")]<-"Student.ID"
districtattendance<-select(dattendance,Student.ID,YTDPercentage)
districtattendance$chronic<-ifelse(dattendance$YTDPercentage>=.9,0,1)
districtattendance$studentenrolled<-1
dchronic<- sum(districtattendance$chronic)
dstudentenrolled<-sum(districtattendance$studentenrolled)
districtattendance<-as.data.frame(t(c(dchronic,dstudentenrolled)))
districtattendance<-districtattendance%>%
  rename(chronic=V1)%>%
  rename(studentenrolled=V2)%>%
  mutate(chronicpercentage=(chronic/studentenrolled))%>%
  mutate(name="District")%>%
  select(name, studentenrolled,chronic,chronicpercentage)

allchronic<-rbind(districtattendance,schoolattendance)

sum(schoolattendance$studentenrolled)
