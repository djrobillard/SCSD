#Behavior Tables
#student ID, race, gender, enrollment building, IEP, ELL, referrals, suspension incidents, unique student?
#Cleaning Up EHOUT for Usage
library(plyr)
library(dplyr)
library(lubridate)
#only columns from ehout that are necessary
ehout2017<-read.csv("ehout2017.csv")
smallehout<-ehout2017[c("OffenderID","OSS","OSS.Days","IncidentType","SchoolCode","Gender","IEP","ELL","RaceEthnicity")]
#only referrals from ehout
smallehoutref<-subset(smallehout,IncidentType=="referral")
#change OSS Days Name to one word
colnames(smallehoutref)[which(names(smallehoutref)=="OSS.Days")]<-"OSSDays"

#aggregate number of OSS and OSS Days by student IDs
stuidOSS<-aggregate(cbind(OSS,OSSDays)~OffenderID+SchoolCode+Gender+IEP+ELL+RaceEthnicity,data=smallehoutref,FUN=sum)
#create a counter for every incident (will use to sum by ID)
smallehoutref$inccount = 1
#sum the counter by unique OffenderID
totrefstuid <- aggregate(inccount ~ OffenderID+SchoolCode+Gender+IEP+ELL+RaceEthnicity, data = smallehoutref, FUN = sum)

#attach OSS Incidents, OSS Days
alldiscipline<-merge(totrefstuid,stuidOSS,by=c("OffenderID","SchoolCode","Gender","IEP","ELL","RaceEthnicity"),all=TRUE)

#add columns for recidivism
alldiscipline$OSSMore<-ifelse(alldiscipline$OSS>1,2,
                              ifelse(alldiscipline$OSS==1,1,0))

alldiscipline<-rename(alldiscipline,STUDENT_ID=OffenderID)

alldiscipline$SchoolCode[alldiscipline$SchoolCode==21]<-11

alldiscipline$SchoolCode<-as.factor(alldiscipline$SchoolCode
)

alldiscipline$SchoolCode<-revalue(alldiscipline$SchoolCode,c(
  "45"="360",
  "25"="17",
  "647"="646",
  "29"="2",
  "15"="361",
  "18"="42",
  "101"="1",
  "607"="7",
    "603"="3",
    "606"="6",
    "604"="4",
    "602"="303",
  "670"="608",
    "54"="608"
 
  
))


###Creating Building Tables
alldiscipline$MultOSS<-ifelse(alldiscipline$OSS>1,1,0)
alldiscipline$OneOSS<-ifelse(alldiscipline$OSS>=1,1,0)
buildingdiscipline<-aggregate(cbind(inccount,MultOSS,OSSDays,OneOSS,OSS)~SchoolCode+IEP+RaceEthnicity+ELL+Gender,data=alldiscipline,FUN=sum)
alldiscipline<-select(alldiscipline,STUDENT_ID,SchoolCode,inccount,OSS,OSSDays,OSSMore,MultOSS,OneOSS)




#Attendance Tables
#cleaning up attendance file for usage
#changing name
attendance2017<-read.csv("attendancedaily.csv",strip.white = TRUE)
#only necessary columns
attendance2017<-attendance2017[c("STUDENT_ID","Period.Absence..","building")]
#add column for each day
attendance2017$daycount = 1
#identify each day as absent or not
attendance2017$Absent<-ifelse(attendance2017$Period.Absence..>.5,1,0)
attendance2017$StateAbsent<-ifelse(attendance2017$Period.Absence..>=1,1,0)
#total days and total absences
enrolleddays <- aggregate(daycount ~ STUDENT_ID+building, data = attendance2017, FUN = sum)
absentdays<-aggregate(Absent ~ STUDENT_ID+building, data = attendance2017, FUN = sum)
stateabsentdays<-aggregate(StateAbsent ~ STUDENT_ID+building, data = attendance2017, FUN = sum)
#merge to one attendance percentage file
finalattendance<-merge(enrolleddays,absentdays, by=c("STUDENT_ID","building"), all=TRUE)
finalattendance<-merge(finalattendance,stateabsentdays, by=c("STUDENT_ID","building"), all=TRUE)
#createattendancepercentagecolumn
finalattendance$totalpercent<-((finalattendance$daycount-finalattendance$Absent)/(finalattendance$daycount))
finalattendance$statepercent<-((finalattendance$daycount-finalattendance$StateAbsent)/(finalattendance$daycount))

#change column name
colnames(finalattendance)[which(names(finalattendance)=="daycount")]<-"Days Enrolled"
colnames(finalattendance)[which(names(finalattendance)=="Absent")]<-"Days Absent"
colnames(finalattendance)[which(names(finalattendance)=="totalpercent")]<-"Attendance Percentage"
colnames(finalattendance)[which(names(finalattendance)=="statepercent")]<-"State Attendance Percentage"

#type of absenteeism
finalattendance$chronic<-ifelse(finalattendance$`Attendance Percentage`>.95,1,
                                ifelse(finalattendance$`Attendance Percentage`>=.9,2,
                                       ifelse(finalattendance$`Attendance Percentage`>=.8,3,4)))
finalattendance$statechronic<-ifelse(finalattendance$`State Attendance Percentage`>.95,1,
                                ifelse(finalattendance$`State Attendance Percentage`>=.9,2,
                                       ifelse(finalattendance$`State Attendance Percentage`>=.8,3,4)))



finalattendance<-rename(finalattendance,SchoolCode=building)
finalattendance$SchoolCode<-as.factor(finalattendance$SchoolCode
                                    )

finalattendance$SchoolCode<-revalue(finalattendance$SchoolCode,c(
  "45"="360",
  "25"="17",
  "647"="646",
  "29"="2",
  "15"="361",
  "18"="42",
  "101"="1",
  "607"="7",
  "603"="3",
  "606"="6",
  "604"="4",
  "602"="303",
  "670"="608",
  "54"="608"
))

#enrollment

###match w/most recent building

enroll1<-read.csv("enrollmentallyears.csv",strip.white = TRUE)
enroll1<-rename(enroll1,SchoolCode=EnrollmentSchoolID)

enroll<-filter(enroll1,SCHOOL_YEAR=="2017")
enroll$Active<-ifelse(enroll$WITHDRAWAL_CODE=="",1,0)
enroll<-select(enroll,STUDENT_ID,SchoolCode,Grade,Gender,Ethnicity,ELL,IEP,F.R.Lunch,ENTRY_DATE,Active)



enroll$Ethnicity<-revalue(enroll$Ethnicity,c(
  "A"="AO",
  "P"="AO",
  "w"="W",
  "b"="B",
  "I"="AO",
  "M"="AO"
))

enroll$SchoolCode<-as.factor(enroll$SchoolCode
)

enroll$SchoolCode<-revalue(enroll$SchoolCode,c(
  "45"="360",
  "25"="17",
  "647"="646",
  "29"="2",
  "15"="361",
  "18"="42",
  "101"="1",
  "607"="7",
  "603"="3",
  "606"="6",
  "604"="4",
  "602"="303",
  "670"="608",
  "54"="608"
))

enrollbuilding<-select(enroll1,EnrollmentSchoolName,SchoolCode)
enrollbuilding<-unique(enrollbuilding)

##merge all together
PSPfinal<-merge(enroll,alldiscipline,by=c("STUDENT_ID","SchoolCode"),all=TRUE)
PSPfinal<-merge(PSPfinal,finalattendance,by=c("STUDENT_ID","SchoolCode"),all=TRUE)
PSPfinal<-select(PSPfinal,STUDENT_ID,SchoolCode,Grade,Ethnicity,ELL,IEP,F.R.Lunch,Active,Gender,inccount,OSS,OSSDays,OSSMore,chronic,statechronic)
PSPfinal<-merge(PSPfinal,enrollbuilding,by="SchoolCode")
PSPfinal<-select(PSPfinal,STUDENT_ID,EnrollmentSchoolName,SchoolCode,Grade,Ethnicity,ELL,IEP,F.R.Lunch,Active,Gender,inccount,OSS,OSSDays,OSSMore,chronic,statechronic)

PSPfinal<-filter(PSPfinal,SchoolCode==16|SchoolCode==555|SchoolCode==8|SchoolCode==7|SchoolCode==11|SchoolCode==22|SchoolCode==322|SchoolCode==20|SchoolCode==34|
                 SchoolCode==360|SchoolCode==27|SchoolCode==363|SchoolCode==3|SchoolCode==24|SchoolCode==647|SchoolCode==9|SchoolCode==6|SchoolCode==28|SchoolCode==2|
                 SchoolCode==361|SchoolCode==1|SchoolCode==608|SchoolCode==33|SchoolCode==13|SchoolCode==48|SchoolCode==37|SchoolCode==323|SchoolCode==646|SchoolCode==4|SchoolCode==40|
                 SchoolCode==303|SchoolCode==42|SchoolCode==30|SchoolCode==44|SchoolCode==328|SchoolCode==49|SchoolCode==51|SchoolCode==364|SchoolCode==51|SchoolCode==17|SchoolCode==36)

PSPfinal[is.na(PSPfinal)]<-0

buildingdiscipline<-merge(buildingdiscipline,enrollbuilding,"SchoolCode")
buildingdiscipline$Space1<-0
buildingdiscipline$Space2<-0
buildingdiscipline<-select(buildingdiscipline,EnrollmentSchoolName,Space1,SchoolCode,RaceEthnicity,ELL,IEP,Space2,Gender,inccount,OSS,OSSDays,MultOSS,OneOSS)
buildingdiscipline$RaceEthnicity<-revalue(buildingdiscipline$RaceEthnicity,c(
  "A"="AO",
  "P"="AO",
  "w"="W",
  "b"="B",
  "I"="AO",
  "M"="AO"
))



write.csv(PSPfinal, "C:/Users/drobil66/Desktop/RFiles/R Reports/PSP.csv") 
write.csv(buildingdiscipline, "C:/Users/drobil66/Desktop/RFiles/R Reports/buildingPSP.csv") 