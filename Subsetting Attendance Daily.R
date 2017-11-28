#students with asbsences

attendance<-attendancedaily
#change column name
colnames(attendance)[which(names(attendance)=="Period Absence %")]<-"Absent"
#subsetting
absent<-subset(attendance,Absent>.5)

as.character(absent$cal_date)

absent1<-subset(absent,cal_date=="2017-01-23 UTC")

#add ehout information
Finalabsent<-merge(absent,alldiscipline,by="student_id", all=TRUE)


#add column for each day
attendance2$daycount = 1
#identify each day as absent or not
smallattendance2017$Absent<-as.numeric(smallattendance2017$'Period Absence %'>.5)

#onlystudents absent
attendancefinal<-subset(attendance2,Period Absence %)