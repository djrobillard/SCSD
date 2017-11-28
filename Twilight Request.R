library(plyr)
library(dplyr)
library(lubridate)
library(scales)




####attendance############################################################################################################

attendance<-read.csv("attendancedaily.csv")

#add column for each day
attendance$day = 1
#identify each day as absent or not
attendance$Absent<-attendance$Period.Absence..>.5

TwiAttend<-filter(attendance,building == 607|building==603|building==606|building==605|building==604|building==602)
NonTwiAttend<-filter(attendance,!building %in% c(602,603,604,605,606,607))

Twienrolleddays <- aggregate(day ~ STUDENT_ID+name, data = TwiAttend, FUN = sum)
Twiabsentdays<-aggregate(Absent ~ STUDENT_ID+name, data = TwiAttend, FUN = sum)
TwiAttendance<-merge(Twienrolleddays,Twiabsentdays, by=c("STUDENT_ID","name"))
TwiAttendance$TwiPercentage<-((TwiAttendance$day-TwiAttendance$Absent)/(TwiAttendance$day))
TwiAttendance<-select(TwiAttendance,STUDENT_ID,name,TwiPercentage)
TwiAttendance<-rename(TwiAttendance,TwiSchool=name)


NonTwienrolleddays <- aggregate(day ~ STUDENT_ID, data = NonTwiAttend, FUN = sum)
NonTwiabsentdays<-aggregate(Absent ~ STUDENT_ID, data = NonTwiAttend, FUN = sum)
NonTwiAttendance<-merge(NonTwienrolleddays,NonTwiabsentdays,by="STUDENT_ID")
NonTwiAttendance$NonTwiPercentage<-((NonTwiAttendance$day-NonTwiAttendance$Absent)/(NonTwiAttendance$day))
NonTwiAttendance<-select(NonTwiAttendance,STUDENT_ID,NonTwiPercentage)

FinalAttendance<-merge(TwiAttendance,NonTwiAttendance,by="STUDENT_ID",all=TRUE)

FinalAttendance<-rename(FinalAttendance,student_id=STUDENT_ID)




#### Credits

Credits<-read.csv("StudentCreditsbyBuilding2.csv")

Credits<-filter(Credits,CourseSchoolID== 1|CourseSchoolID== 3|CourseSchoolID== 4|CourseSchoolID== 6|CourseSchoolID== 54|CourseSchoolID== 101|
                  CourseSchoolID== 303|CourseSchoolID== 410|CourseSchoolID== 402|CourseSchoolID== 403|CourseSchoolID== 404|CourseSchoolID== 405|
                  CourseSchoolID== 406|CourseSchoolID== 433|CourseSchoolID== 434|CourseSchoolID== 436|CourseSchoolID== 437|CourseSchoolID== 503|
                  CourseSchoolID== 554|CourseSchoolID== 602|CourseSchoolID== 603|CourseSchoolID== 604|CourseSchoolID== 605|CourseSchoolID== 606|
                  CourseSchoolID== 608|CourseSchoolID== 650|CourseSchoolID== 651|CourseSchoolID== 670|CourseSchoolID== 912|CourseSchoolID== 944|
                 CourseSchoolID== 945|CourseSchoolID== 999)
                  
Credits$SchoolType<-mapvalues(Credits$CourseSchoolID, from = c(602,603,604,605,606,607,433,434,436,437,
                                                               1,3,4,6,54,101,303,401,402,403,404,405,406,503,506,507,554,608,650,651,670,912,944,945,999),
                              to=c("Twilight","Twilight","Twilight","Twilight","Twilight","Twilight","Twilight","Twilight","Twilight","Twilight",
                                   "Non-Twi","Non-Twi","Non-Twi","Non-Twi","Non-Twi","Non-Twi","Non-Twi","Non-Twi","Non-Twi","Non-Twi","Non-Twi","Non-Twi",
                                   "Non-Twi","Non-Twi","Non-Twi","Non-Twi","Non-Twi","Non-Twi","Non-Twi","Non-Twi","Non-Twi","Non-Twi",
                                   "Non-Twi","Non-Twi","Non-Twi"))

total.earned.building<-as.data.frame(aggregate(EARN_CREDIT ~ STUDENT_ID + SchoolType,data=Credits, FUN=sum))
total.attempted.building<-as.data.frame(aggregate(ATT_CREDIT ~ STUDENT_ID + SchoolType,data=Credits, FUN=sum))

total<-merge(total.attempted.building,total.earned.building)

TotalCredits<-reshape(total,idvar = "STUDENT_ID",timevar = "SchoolType",direction = "wide")

colnames(TotalCredits)[colnames(TotalCredits)=="STUDENT_ID"]<-"student_id"


TotalCredits<-rename(TotalCredits,TwiEarnCredits=EARN_CREDIT.Twilight)
TotalCredits<-rename(TotalCredits,TwiAttCredits=ATT_CREDIT.Twilight)
TotalCredits<-rename(TotalCredits,NonTwiEarnCredits=`EARN_CREDIT.Non-Twi`)
TotalCredits<-rename(TotalCredits,NonTwiAttCredits=`ATT_CREDIT.Non-Twi`)

#### merge

FinalTwi<-merge(FinalAttendance,TotalCredits,"student_id",all=TRUE)

FinalTwi<-FinalTwi[!is.na(FinalTwi$TwiPercentage),]



##add current demographic information
#getting columns from enrollment
enrollment17<-read.csv("demographics.csv")
enrollment17<-subset(enrollment17,SCHOOL_YEAR=="2017")
enrollment<-enrollment17[c("STUDENT_ID","FirstName","LastName","Gender","Grade","Ethnicity","ELL","IEP","AccountabilitySchoolName")]
enrollment<-rename(enrollment,student_id=STUDENT_ID)

#merge
FinalTwi3<-merge(FinalTwi,enrollment,"student_id",all.x=TRUE)

FinalTwi1<-select(FinalTwi3,student_id,LastName,FirstName,Grade,Gender,Ethnicity,ELL,IEP,TwiSchool,AccountabilitySchoolName,TwiPercentage,NonTwiPercentage,TwiAttCredits,TwiEarnCredits,NonTwiAttCredits,NonTwiEarnCredits)
FinalTwi1$NonTwiPercentage <- paste(round((FinalTwi1$NonTwiPercentage)*100,digits=1),"%",sep="")
FinalTwi1$TwiPercentage <- paste(round((FinalTwi1$TwiPercentage)*100,digits=1),"%",sep="")

write.csv(FinalTwi1, "C:/Users/drobil66/Desktop/RFiles/R Reports/TwilightAttCredits.csv") 
