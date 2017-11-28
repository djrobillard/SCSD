library(plyr)
library(dplyr)
library(scales)
library(xlsx)
library(lubridate)
library(eeptools)


#
attendance1<-read.csv("attendancedaily.csv")

#only select columns
attendance<-attendance1[c("name","cal_date","Period.Absence..")]
#change colname
colnames(attendance)[which(names(attendance)=="Period.Absence..")]<-"PeriodAbsencePercent"

attendance<-subset(attendance, name=="Grant Middle School")

#add column for each day
attendance$inccount = 1
#identify each day as absent or not
attendance$Absent<-as.numeric(attendance$'PeriodAbsencePercent'>.5)

#aggregate enrolled days and total absences
enrolled<- aggregate(inccount ~ name+cal_date, data = attendance, FUN = sum)
absences<- aggregate(Absent ~ name+cal_date, data = attendance, FUN = sum)


#mergeintoMP Files
attendance<-merge(enrolled,absences,by=c("name","cal_date"),all=TRUE)

attendance$day <- weekdays(as.Date(attendance$cal_date))

enrolled<- aggregate(inccount ~ name+day, data = attendance, FUN = sum)
absences<- aggregate(Absent ~ name+day, data = attendance, FUN = sum)

attendance<-merge(enrolled,absences,by=c("name","day"),all=TRUE)


#createattendancepercentagecolumn
attendance$YTDPercentage<-((attendance$inccount-attendance$Absent)/(attendance$inccount))

attendance$YTDPercentage <- paste(round((attendance$YTDPercentage)*100,digits=1),"%",sep="")

write.csv(attendance, "C:/Users/drobil66/Desktop/RFiles/R Reports/Grant Attendance by day of Week.csv") 
