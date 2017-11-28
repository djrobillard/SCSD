#Engaging Schools Request
library(plyr)
library(dplyr)
library(lubridate)
#Pass rates for 6-12 (Can you provide data from 2013-2014 through this school year. If not, please do what feels viable?)
Credits1<-read.csv("StudentCredits.csv",strip.white = TRUE)
Credits1<-rename(Credits1,SCHOOL_YEAR=ï..SCHOOL_YEAR)
Credits<-select(Credits1,SCHOOL_YEAR,STUDENT_ID,SUMMER_SCHOOL,Ethnicity,DateEnteredGrade9,CourseSchoolName,SubjArea_CourseCatalog,ATT_CREDIT,EARN_CREDIT,STUDENT_ID,Mark1_Value,Mark2_Value,Mark3_Value,Mark4_Value,
                Mark1_Passing,Mark2_Passing,Mark3_Passing,Mark4_Passing)
Credits<-filter(Credits,SCHOOL_YEAR==2014|SCHOOL_YEAR==2015|SCHOOL_YEAR==2016|SCHOOL_YEAR==2017)
Credits<-filter(Credits,SUMMER_SCHOOL=="N")
Credits<-filter(Credits,CourseSchoolName=="Nottingham High School"|CourseSchoolName=="Henninger High School"|
                  CourseSchoolName=="Institute of Technology at Syracuse Central"|
                                    CourseSchoolName=="P-Tech")

Credits$SubjArea_CourseCatalog<-revalue(Credits$SubjArea_CourseCatalog, c(
  "English"="ELA",
  "Physical Education"="Special",
  "Elective Science"="Elective",
  "Health"="Special",
  "Elective"="Elective",
  "CTE"="CTE",
  "Language"="Foreign Language",
  "Art"="Special",
  "Music"="Special",
  "Participation in Government"="Social Studies",
  "Physical Science"="Science",
  "Economics"="Social Studies",
  "Mathematics"="Math",
  "Life Science"="Science",
  "History"="Social Studies",
  "Electives / Sequences"="Electives",
  "English Language Arts"="ELA",
  "Lab"="Science",
  "Languages other than English"="Foreign Language"
))

Credits$SubjArea_CourseCatalog <- sub("^$", "Other", Credits$SubjArea_CourseCatalog)

Credits$date <- mdy(Credits$DateEnteredGrade9) # puts into date format
Credits$CohortYear<-year(Credits$date) # parse year

###ADDING IN ESL, IEP Status################
enrollment<-read.csv("enrollmentallyears.csv")
enroll1<-select(enrollment,ï..SCHOOL_YEAR,STUDENT_ID,IEP,ELL)
enroll1<-rename(enroll1,SCHOOL_YEAR=ï..SCHOOL_YEAR)

Credits2<-merge(Credits,enroll1,by=c("STUDENT_ID","SCHOOL_YEAR"),all=TRUE)

MP1<-select(Credits2,SCHOOL_YEAR,Mark1_Value,IEP,ELL,Ethnicity)
MP1<-MP1[!is.na(as.numeric(as.character(MP1$Mark1_Value))),]
MP2<-select(Credits2,SCHOOL_YEAR,IEP,ELL,Ethnicity,Mark2_Value)
MP2<-MP2[!is.na(as.numeric(as.character(MP2$Mark2_Value))),]
MP3<-select(Credits2,SCHOOL_YEAR,IEP,ELL,Ethnicity,Mark3_Value)
MP3<-MP3[!is.na(as.numeric(as.character(MP3$Mark3_Value))),]
MP4<-select(Credits2,SCHOOL_YEAR,IEP,ELL,Ethnicity,Mark4_Value)
MP4<-MP4[!is.na(as.numeric(as.character(MP4$Mark4_Value))),]

MP1$MP1Att<-1
MP2$MP2Att<-1
MP3$MP3Att<-1
MP4$MP4Att<-1

MP1$MP1Pass<-ifelse(as.numeric(MP1$Mark1_Value)>64,1,0)
MP2$MP2Pass<-ifelse(as.numeric(MP2$Mark2_Value)>64,1,0)
MP3$MP3Pass<-ifelse(as.numeric(MP3$Mark3_Value)>64,1,0)
MP4$MP4Pass<-ifelse(as.numeric(MP4$Mark4_Value)>64,1,0)

MP1Attempts<-aggregate(MP1Att ~ IEP +ELL+SCHOOL_YEAR+Ethnicity,  data=MP1, FUN=sum)
MP1Earned<-aggregate(MP1Pass ~ IEP +ELL+SCHOOL_YEAR+Ethnicity,  data=MP1, FUN=sum)

MP2Attempts<-aggregate(MP2Att ~ IEP +ELL+SCHOOL_YEAR+Ethnicity,  data=MP2, FUN=sum)
MP2Earned<-aggregate(MP2Pass ~ IEP +ELL+SCHOOL_YEAR+Ethnicity,  data=MP2, FUN=sum)

MP3Attempts<-aggregate(MP3Att ~ IEP +ELL+SCHOOL_YEAR+Ethnicity,  data=MP3, FUN=sum)
MP3Earned<-aggregate(MP3Pass ~ IEP +ELL+SCHOOL_YEAR+Ethnicity,  data=MP3, FUN=sum)

MP4Attempts<-aggregate(MP4Att ~ IEP +ELL+SCHOOL_YEAR+Ethnicity,  data=MP4, FUN=sum)
MP4Earned<-aggregate(MP4Pass ~ IEP +ELL+SCHOOL_YEAR+Ethnicity,  data=MP4, FUN=sum)

QuarterCredits<-merge(MP1Attempts,MP1Earned,by=c("IEP","SCHOOL_YEAR","ELL","Ethnicity"))
QuarterCredits<-merge(QuarterCredits,MP2Earned,by=c("IEP","SCHOOL_YEAR","ELL","Ethnicity"))
QuarterCredits<-merge(QuarterCredits,MP3Earned,by=c("IEP","SCHOOL_YEAR","ELL","Ethnicity"))
QuarterCredits<-merge(QuarterCredits,MP4Earned,by=c("IEP","SCHOOL_YEAR","ELL","Ethnicity"))
QuarterCredits<-merge(QuarterCredits,MP2Attempts,by=c("IEP","SCHOOL_YEAR","ELL","Ethnicity"))
QuarterCredits<-merge(QuarterCredits,MP3Attempts,by=c("IEP","SCHOOL_YEAR","ELL","Ethnicity"))
QuarterCredits<-merge(QuarterCredits,MP4Attempts,by=c("IEP","SCHOOL_YEAR","ELL","Ethnicity"))

QuarterCredits<-QuarterCredits %>%
  mutate(MP1PassRate=MP1Pass/MP1Att)%>%
  mutate(MP2PassRate=MP2Pass/MP2Att)%>%
  mutate(MP3PassRate=MP3Pass/MP3Att)%>%
  mutate(MP4PassRate=MP4Pass/MP4Att)

write.csv(QuarterCredits, "C:/Users/drobil66/Desktop/RFiles/R Reports/EngagingSchoolsCredits.csv") 



FinalCredits<-merge(AllCredits,QuarterCredits,by=c("CourseSchoolName","SCHOOL_YEAR","SubjArea_CourseCatalog","CohortYear"))
FinalCredits<-select(FinalCredits,CourseSchoolName,SCHOOL_YEAR,CohortYear,SubjArea_CourseCatalog,ATT_CREDIT,EARN_CREDIT,FinalPassingRate,MP1Pass,MP1Att,MP1PassRate,MP2Pass,MP2Att,MP2PassRate,MP3Pass,MP3Att,MP3PassRate,MP4Pass,MP4Att,MP4PassRate)

FinalCredits$MP1PassRate <- paste(round((FinalCredits$MP1PassRate)*100,digits=1),"%",sep="")
FinalCredits$MP2PassRate <- paste(round((FinalCredits$MP2PassRate)*100,digits=1),"%",sep="")
FinalCredits$MP3PassRate <- paste(round((FinalCredits$MP3PassRate)*100,digits=1),"%",sep="")
FinalCredits$MP4PassRate <- paste(round((FinalCredits$MP4PassRate)*100,digits=1),"%",sep="")
FinalCredits$FinalPassingRate <- paste(round((FinalCredits$FinalPassingRate)*100,digits=1),"%",sep="")

###################################################################################
###################################################################################
###################################################################################



################################################################################################################################
################################################################################################################################
################################################################################################################################
#First semester dropout rates for the high school 9-11th grade
enrollment<-read.csv("enrollmentallyears.csv")
enrollment$Student<-1
totalstudents<-aggregate(Student~EnrollmentSchoolName,data=enrollment, FUN=sum)
totaldropouts<-aggregate(Dropout~EnrollmentSchoolName,data=enrollment,FUN=sum)
Dropout<-merge(totalstudents,totaldropouts,by="EnrollmentSchoolName")
Dropout$DropoutRate<-(Dropout$Dropout/Dropout$Student)
Dropout<-filter(Dropout,EnrollmentSchoolName=="Nottingham High School"|EnrollmentSchoolName=="Henninger High School"|
                  EnrollmentSchoolName=="Institute of Technology at Syracuse Central"|EnrollmentSchoolName=="Public Service Leadership Academy at Fowler"|
                  EnrollmentSchoolName=="Roberts K-8 School"|EnrollmentSchoolName=="Edward Smith K-8 School"|
                  EnrollmentSchoolName=="Fowler High School"|EnrollmentSchoolName=="Twilight Academy @ Nottingham"|
                  EnrollmentSchoolName=="Twilight Academy @ PSLA "|EnrollmentSchoolName=="Twilight Academy @ Henninger"|
                  EnrollmentSchoolName=="Twilight Academy @ Corcoran"|EnrollmentSchoolName=="Twilight Academy @ Fowler"|
                  EnrollmentSchoolName=="P-Tech")

Dropout$DropoutRate <- paste(round((Dropout$DropoutRate)*100,digits=1),"%",sep="")

################################################################################################################################
###############Referrals, ISS OSS by Quarter 2015-2017###########################################################################################
################################################################################################################################
ehout<-read.csv("ehout2017.csv")


###parse dates and create time intervals
ehout$date <- mdy(ehout$IncidentDate) # puts into date format
ehout$year<-year(ehout$date)
ehout$month<-month(ehout$date)
ehout$month<-sprintf("%02d",ehout$month)
ehout$day<-day(ehout$date)
ehout$day<-sprintf("%02d",ehout$day)

ehout$date.num <- as.numeric(paste0(ehout$year, ehout$month, ehout$day)) 

ehout$MP<-ifelse(ehout$date.num>20160815 & ehout$date.num<20161127, 1, 
                 ifelse(ehout$date.num>20161128 & ehout$date.num<20170127, 2, 
                        ifelse(ehout$date.num>20170128 & ehout$date.num<20170407, 3, 
                               ifelse(ehout$date.num>20170408 & ehout$date.num<20170622, 4, 0)
                        )
                 )
)

# cleaning up the rest
#Cleaning Up EHOUT for Usage

#only columns from ehout that are necessary
ehout<-ehout[c("OffenderID","OSS","ISS","MP")]
#only referrals from ehout

#create a counter for every incident (will use to sum by ID)
ehout$inccount = 1

#Subset by MP
ehoutMP1<-subset(ehout,MP=="1")
ehoutMP2<-subset(ehout,MP=="2")
ehoutMP3<-subset(ehout,MP=="3")
ehoutMP4<-subset(ehout,MP=="4")


#sum referrals,OSS,OSS Days by MP

MP1RefOSS<-aggregate(cbind(OSS,ISS,inccount)~OffenderID,data=ehoutMP1,FUN=sum)
MP2RefOSS<-aggregate(cbind(OSS,ISS,inccount)~OffenderID,data=ehoutMP2,FUN=sum)
MP3RefOSS<-aggregate(cbind(OSS,ISS,inccount)~OffenderID,data=ehoutMP3,FUN=sum)
MP4RefOSS<-aggregate(cbind(OSS,ISS,inccount)~OffenderID,data=ehoutMP4,FUN=sum)


#change all column names
colnames(MP1RefOSS)[which(names(MP1RefOSS)=="inccount")]<-"2017MP1 Referrals"
colnames(MP1RefOSS)[which(names(MP1RefOSS)=="OSS")]<-"2017MP1OSS"
colnames(MP1RefOSS)[which(names(MP1RefOSS)=="ISS")]<-"2017MP1ISS"

colnames(MP2RefOSS)[which(names(MP2RefOSS)=="inccount")]<-"2017MP2Referrals"
colnames(MP2RefOSS)[which(names(MP2RefOSS)=="OSS")]<-"2017MP2OSS"
colnames(MP2RefOSS)[which(names(MP2RefOSS)=="ISS")]<-"2017MP2ISS"

colnames(MP3RefOSS)[which(names(MP3RefOSS)=="inccount")]<-"2017MP3Referrals"
colnames(MP3RefOSS)[which(names(MP3RefOSS)=="OSS")]<-"2017MP3OSS"
colnames(MP3RefOSS)[which(names(MP3RefOSS)=="ISS")]<-"2017MP3ISS"

colnames(MP4RefOSS)[which(names(MP4RefOSS)=="inccount")]<-"2017MP4Referrals"
colnames(MP4RefOSS)[which(names(MP4RefOSS)=="OSS")]<-"2017MP4OSS"
colnames(MP4RefOSS)[which(names(MP4RefOSS)=="ISS")]<-"2017MP4ISS"

#merge all together
ehoutbyMP<-merge(MP1RefOSS,MP2RefOSS,"OffenderID",all=TRUE)
ehoutbyMP<-merge(ehoutbyMP,MP3RefOSS,"OffenderID",all=TRUE)
ehoutbyMP<-merge(ehoutbyMP,MP4RefOSS,"OffenderID",all=TRUE)


colnames(ehoutbyMP)[which(names(ehoutbyMP)=="OffenderID")]<-"STUDENT_ID"

#######same but for 2016


ehout2<-read.csv("ehout2016.csv")


###parse dates and create time intervals
ehout2$date <- mdy(ehout2$IncidentDate) # puts into date format
ehout2$year<-year(ehout2$date)
ehout2$month<-month(ehout2$date)
ehout2$month<-sprintf("%02d",ehout2$month)
ehout2$day<-day(ehout2$date)
ehout2$day<-sprintf("%02d",ehout2$day)

ehout2$date.num <- as.numeric(paste0(ehout2$year, ehout2$month, ehout2$day)) 

ehout2$MP<-ifelse(ehout2$date.num>20150815 & ehout2$date.num<20151127, 1, 
                 ifelse(ehout2$date.num>20151128 & ehout2$date.num<20160127, 2, 
                        ifelse(ehout2$date.num>20160128 & ehout2$date.num<20160407, 3, 
                               ifelse(ehout2$date.num>20160408 & ehout2$date.num<20160623, 4, 0)
                        )
                 )
)

# cleaning up the rest
#Cleaning Up EHOUT for Usage

#only columns from ehout that are necessary
ehout2<-ehout2[c("OffenderID","OSS","ISS","MP")]
#only referrals from ehout

#create a counter for every incident (will use to sum by ID)
ehout2$inccount = 1

#Subset by MP
ehout2MP1<-subset(ehout2,MP=="1")
ehout2MP2<-subset(ehout2,MP=="2")
ehout2MP3<-subset(ehout2,MP=="3")
ehout2MP4<-subset(ehout2,MP=="4")


#sum referrals,OSS,OSS Days by MP

MP1RefOSS<-aggregate(cbind(OSS,ISS,inccount)~OffenderID,data=ehout2MP1,FUN=sum)
MP2RefOSS<-aggregate(cbind(OSS,ISS,inccount)~OffenderID,data=ehout2MP2,FUN=sum)
MP3RefOSS<-aggregate(cbind(OSS,ISS,inccount)~OffenderID,data=ehout2MP3,FUN=sum)
MP4RefOSS<-aggregate(cbind(OSS,ISS,inccount)~OffenderID,data=ehout2MP4,FUN=sum)


#change all column names
colnames(MP1RefOSS)[which(names(MP1RefOSS)=="inccount")]<-"2016MP1 Referrals"
colnames(MP1RefOSS)[which(names(MP1RefOSS)=="OSS")]<-"2016MP1OSS"
colnames(MP1RefOSS)[which(names(MP1RefOSS)=="ISS")]<-"2016MP1ISS"

colnames(MP2RefOSS)[which(names(MP2RefOSS)=="inccount")]<-"2016MP2Referrals"
colnames(MP2RefOSS)[which(names(MP2RefOSS)=="OSS")]<-"2016MP2OSS"
colnames(MP2RefOSS)[which(names(MP2RefOSS)=="ISS")]<-"2016MP2ISS"

colnames(MP3RefOSS)[which(names(MP3RefOSS)=="inccount")]<-"2016MP3Referrals"
colnames(MP3RefOSS)[which(names(MP3RefOSS)=="OSS")]<-"2016MP3OSS"
colnames(MP3RefOSS)[which(names(MP3RefOSS)=="ISS")]<-"2016MP3ISS"

colnames(MP4RefOSS)[which(names(MP4RefOSS)=="inccount")]<-"2016MP4Referrals"
colnames(MP4RefOSS)[which(names(MP4RefOSS)=="OSS")]<-"2016MP4OSS"
colnames(MP4RefOSS)[which(names(MP4RefOSS)=="ISS")]<-"2016MP4ISS"

#merge all together
ehout2byMP<-merge(MP1RefOSS,MP2RefOSS,"OffenderID",all=TRUE)
ehout2byMP<-merge(ehout2byMP,MP3RefOSS,"OffenderID",all=TRUE)
ehout2byMP<-merge(ehout2byMP,MP4RefOSS,"OffenderID",all=TRUE)


colnames(ehout2byMP)[which(names(ehout2byMP)=="OffenderID")]<-"STUDENT_ID"

ehoutbothyears<-merge(ehoutbyMP,ehout2byMP,"STUDENT_ID",all=TRUE)

###################################################################################################################################################
###MATH/ELA GRADES BY MP########################
################################################################################################################################
Credits<-read.csv("ESmodcourses.csv")

Credits$SubjArea_CourseCatalog<-revalue(Credits$SubjArea_CourseCatalog, c(
  "English"="ELA",
  "Physical Education"="Special",
  "Elective Science"="Elective",
  "Health"="Special",
  "Elective"="Elective",
  "CTE"="CTE",
  "Language"="Foreign Language",
  "Art"="Special",
  "Music"="Special",
  "Participation in Government"="Social Studies",
  "Physical Science"="Science",
  "Economics"="Social Studies",
  "Mathematics"="Math",
  "Life Science"="Science",
  "History"="Social Studies",
  "Electives / Sequences"="Electives",
  "English Language Arts"="ELA",
  "Lab"="Science",
  "Languages other than English"="Foreign Language"
))



Credits$SubjArea_CourseCatalog <- sub("^$", "Other", Credits$SubjArea_CourseCatalog)

Credits$Mark1_Value<-as.numeric(as.character(Credits$Mark1_Value))
Credits$Mark2_Value<-as.numeric(as.character(Credits$Mark2_Value))
Credits$Mark3_Value<-as.numeric(as.character(Credits$Mark3_Value))
Credits$Mark4_Value<-as.numeric(as.character(Credits$Mark4_Value))

Credits[is.na(Credits)] <- " "

Credits2<-select(Credits,CourseSchoolName,SCHOOL_YEAR,STUDENT_ID,Mark1_Value,Mark2_Value,Mark3_Value,Mark4_Value,SubjArea_CourseCatalog)
Credits2<-filter(Credits2,SubjArea_CourseCatalog==c("Math","ELA"))
Credits2$M1Attempt<-ifelse(Credits2$Mark1_Value>=0,1,0)
Credits2$M2Attempt<-ifelse(Credits2$Mark2_Value>=0,1,0)
Credits2$M3Attempt<-ifelse(Credits2$Mark3_Value>=0,1,0)
Credits2$M4Attempt<-ifelse(Credits2$Mark4_Value>=0,1,0)
Credits2$M1Pass<-ifelse(Credits2$Mark1_Value>=65,1,0)
Credits2$M2Pass<-ifelse(Credits2$Mark2_Value>=65,1,0)
Credits2$M3Pass<-ifelse(Credits2$Mark3_Value>=65,1,0)
Credits2$M4Pass<-ifelse(Credits2$Mark4_Value>=65,1,0)

Credits2<-select(Credits2,CourseSchoolName,SCHOOL_YEAR,STUDENT_ID,SubjArea_CourseCatalog,Mark1_Value,M1Attempt,M1Pass,Mark2_Value,M2Attempt,
                 M2Pass,Mark3_Value,M3Attempt,M3Pass,Mark4_Value,M4Attempt,M4Pass)

MP1<-select(Credits3,STUDENT_ID,SCHOOL_YEAR,CourseSchoolName,SubjArea_CourseCatalog,Mark1_Value,M1Pass)
MP2<-select(Credits3,STUDENT_ID,SCHOOL_YEAR,CourseSchoolName,SubjArea_CourseCatalog,Mark2_Value,M2Pass)
MP3<-select(Credits3,STUDENT_ID,SCHOOL_YEAR,CourseSchoolName,SubjArea_CourseCatalog,Mark3_Value,M3Pass)
MP4<-select(Credits3,STUDENT_ID,SCHOOL_YEAR,CourseSchoolName,SubjArea_CourseCatalog,Mark4_Value,M4Pass)

MP1<-rename(MP1,Passed=M1Pass)
MP2<-rename(MP2,Passed=M2Pass)
MP3<-rename(MP3,Passed=M3Pass)
MP4<-rename(MP4,Passed=M4Pass)
MP1<-rename(MP1,GradeValue=Mark1_Value)
MP2<-rename(MP2,GradeValue=Mark2_Value)
MP3<-rename(MP3,GradeValue=Mark3_Value)
MP4<-rename(MP4,GradeValue=Mark4_Value)


MP1$MarkingPeriod<-1
MP2$MarkingPeriod<-2
MP3$MarkingPeriod<-3
MP4$MarkingPeriod<-4

AllCourses<-do.call("rbind",list(MP1,MP2,MP3,MP4))
AllCourses$Attempt<-ifelse(AllCourses$Grade>=0,1,0)

demo<-read.csv("enrollmentallyears.csv")
demo<-rename(demo,SCHOOL_YEAR=ï..SCHOOL_YEAR)
demo<-select(demo,SCHOOL_YEAR,STUDENT_ID,Ethnicity,IEP,ELL,Grade)

AllCourses<-merge(AllCourses,demo,c("STUDENT_ID","SCHOOL_YEAR"))

####################MERGING THEM ALL TOGETHER###############

WorryListData<-merge(,ehoutbothyears,"STUDENT_ID",all=TRUE)