###parse dates and create time intervals
library(lubridate)
ehout<-read.csv("ehout all years.csv")
ehout$date <- mdy(ehout$IncidentDate) # puts into date format
ehout$year<-year(ehout$date) # parse year
ehout$month<-month(ehout$date) # parse month
ehout$month<-sprintf("%02d",ehout$month) # create 2 digit month
ehout$day<-day(ehout$date) # parse day
ehout$day<-sprintf("%02d",ehout$day) # create 2 digit day

ehout$date.num <- as.numeric(paste0(ehout$year, ehout$month, ehout$day)) # concatenate

#ytd calculations
ehout$ytd<-ifelse(ehout$date.num>20130901 & ehout$date.num<20140519, 1, 
                  ifelse(ehout$date.num>20140901 & ehout$date.num<20150519, 1, 
                         ifelse(ehout$date.num>20150901 & ehout$date.num<20160519, 1, 
                                ifelse(ehout$date.num>20160821 & ehout$date.num<20170519, 1, 0)
                         )
                  )
)

#####filter to proper data set
library(plyr)
library(dplyr)
ehout3<-filter(ehout, ytd==1)
ehout3<-filter(ehout3,Grade!=15)

####data analysis for all incidents

ehout3$inccount=1

ehout3$raceethnicity<-revalue(ehout3$raceethnicity, c("A" = "O", "I" = "O",
                                                  "M" = "O", "P" = "O"))

total.oss<-as.data.frame(aggregate(OSS ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL + FRPL + Grade, data=ehout3, FUN=sum))
total.ossdays<-as.data.frame(aggregate(OSS.Days ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL + FRPL + Grade, data=ehout3, FUN=sum))
total.iss<-as.data.frame(aggregate(ISS ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL + FRPL + Grade,  data=ehout3, FUN=sum))
total.issdays<-as.data.frame(aggregate(ISS.Days ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL + FRPL + Grade,  data=ehout3, FUN=sum))
total.susp<-as.data.frame(aggregate(Total.Suspension ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL + FRPL + Grade,  data=ehout3, FUN=sum))
total.suspdays<-as.data.frame(aggregate(Total.Susp.Duration ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL + FRPL + Grade,  data=ehout3, FUN=sum))
total.referral<-as.data.frame(aggregate(inccount~SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL + FRPL + Grade,  data=ehout3, FUN=sum))



all.susp.categories<-merge(total.oss, total.ossdays)
all.susp.categories<-merge(all.susp.categories, total.iss)
all.susp.categories<-merge(all.susp.categories, total.issdays)
all.susp.categories<-merge(all.susp.categories, total.susp)
all.susp.categories<-merge(all.susp.categories, total.suspdays)
all.susp.categories<-merge(all.susp.categories,total.referral)

####data analysis for unique students
unique <- select(ehout3, SchoolYear, SchoolName, 
                 OffenderID, IEP, ELL, 
                 RaceEthnicity, FRPL, Gender, Grade, OSS,ISS,inccount)

final.unique <-unique %>%
  group_by(SchoolYear,SchoolName,OffenderID,IEP,ELL, 
           RaceEthnicity, FRPL, Gender, Grade, OSS, ISS, inccount)%>%
      summarise_each(funs(sum),iss.count=ISS,oss.count=OSS,referrals=inccount)

final.unique$OSS.count<-ifelse(final.unique$oss.count>=1, 1, 0)
final.unique$ISS.count<-ifelse(final.unique$iss.count>=1, 1, 0)
final.unique$referrals.count<-ifelse(final.unique$referrals>=1,1,0)

colnames(final.unique)[colnames(final.unique)=="OSS.count"]<-"OSS.flag"
colnames(final.unique)[colnames(final.unique)=="ISS.count"]<-"ISS.flag"
colnames(final.unique)[colnames(final.unique)=="longoss.count"]<-"LongOSS.flag"
colnames(final.unique)[colnames(final.unique)=="shortoss.count"]<-"ShortOSS.flag"
colnames(final.unique)[colnames(final.unique)=="referrals.count"]<-"Referrals.flag"


final.unique.OSS<-aggregate(OSS.flag ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL + FRPL + Grade, data=final.unique, FUN=sum)
final.unique.ISS<-aggregate(ISS.flag ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL + FRPL + Grade, data=final.unique, FUN=sum)
final.unique.longoss<-aggregate(LongOSS.flag ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL + FRPL + Grade, data=final.unique, FUN=sum)
final.unique.shortoss<-aggregate(ShortOSS.flag ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL + FRPL + Grade, data=final.unique, FUN=sum)
final.unique.referrals<-aggregate(Referrals.flag ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL + FRPL + Grade, data=final.unique, FUN=sum)



all.susp.categories<-merge(all.susp.categories, final.unique.OSS, all=TRUE)
all.susp.categories<-merge(all.susp.categories, final.unique.ISS, all=TRUE)
all.susp.categories<-merge(all.susp.categories, final.unique.referrals, all=TRUE)

colnames(all.susp.categories)[colnames(all.susp.categories)=="ISS.flag"]<-"ISS.unique.students"
colnames(all.susp.categories)[colnames(all.susp.categories)=="OSS.flag"]<-"OSS.unique.students"
colnames(all.susp.categories)[colnames(all.susp.categories)=="ShortOSS.flag"]<-"ShortOSS.unique.students"
colnames(all.susp.categories)[colnames(all.susp.categories)=="LongOSS.flag"]<-"LongOSS.unique.students"
colnames(all.susp.categories)[colnames(all.susp.categories)=="Referrals.flag"]<-"Referrals.unique.students"


###########################enrollment##################################
#2017
beds2017<-read.csv("2017 BEDS.csv")
beds2017$POVERTY<-as.numeric(beds2017$POVERTY)
beds2017$POVERTY[beds2017$POVERTY==1]<-0
beds2017$POVERTY[beds2017$POVERTY==2]<-1
beds2017$LEP_ELIGIBILITY<-as.numeric(beds2017$LEP_ELIGIBILITY)
beds2017$LEP_ELIGIBILITY[beds2017$LEP_ELIGIBILITY==1]<-0
beds2017$LEP_ELIGIBILITY[beds2017$LEP_ELIGIBILITY==2]<-1
beds2017$ETHNIC_DESC<-revalue(beds2017$ETHNIC_DESC, c("Black or African American"="B", "White"="W", 
                                                      "American Indian or Alaska Native" = "O", "Hispanic or Latino" = "H",
                                                      "Multiracial" = "O", "Asian or Pacific Islander" = "O"))
beds2017$STUDENT_GENDER<-revalue(beds2017$STUDENT_GENDER, c("Male" = "M", "Female" = "F"))
beds2017$sped <- trimws(beds2017$CHALLENGE_TYPE)
beds2017$sped <- sub(" ", "")
beds2017.filter <- filter(beds2017, sped != "")
beds2017.filter$iep <- 1
beds2017.filter <- select(beds2017.filter, STUDENT_ID, iep)
beds2017b<-merge(beds2017, beds2017.filter, all=TRUE)
beds2017b$iep[is.na(beds2017b$iep)]<-0
beds2017b$schoolyear<-2017
beds2017b$CURR_GRADE_LVL<-as.character(beds2017$CURR_GRADE_LVL)
beds2017$CURR_GRADE_LVL<-(revalue(beds2017b$CURR_GRADE_LVL, c("PKF" = "PK", "PKH" = "PK")))
colnames(beds2017b)[colnames(beds2017b)=="CURR_GRADE_LVL"]<-"grade"
colnames(beds2017b)[colnames(beds2017b)=="LOCATION_NAME"]<-"schoolname"
colnames(beds2017b)[colnames(beds2017b)=="POVERTY"]<-"frpl"
colnames(beds2017b)[colnames(beds2017b)=="ETHNIC_DESC"]<-"raceethnicity"
colnames(beds2017b)[colnames(beds2017b)=="STUDENT_GENDER"]<-"gender"
colnames(beds2017b)[colnames(beds2017b)=="LEP_ELIGIBILITY"]<-"ell"
colnames(beds2017b)[colnames(beds2017b)=="STUDENT_ID"]<-"offenderid"
beds2017.clean<-select(beds2017b, schoolyear, schoolname, offenderid, grade, raceethnicity, gender,
                       iep, ell, frpl)

#2016
beds2016<-read.csv("2016 BEDS.csv")
beds2016$POVERTY<-as.numeric(beds2016$POVERTY)
beds2016$POVERTY[beds2016$POVERTY==1]<-0
beds2016$POVERTY[beds2016$POVERTY==2]<-1
beds2016$LEP_ELIGIBILITY<-as.numeric(beds2016$LEP_ELIGIBILITY)
beds2016$LEP_ELIGIBILITY[beds2016$LEP_ELIGIBILITY==1]<-0
beds2016$LEP_ELIGIBILITY[beds2016$LEP_ELIGIBILITY==2]<-1
beds2016$ETHNIC_DESC<-revalue(beds2016$ETHNIC_DESC, c("Black or African American"="B", "White"="W", 
                                                      "American Indian or Alaska Native" = "O", "Hispanic or Latino" = "H",
                                                      "Multiracial" = "O", "Asian or Pacific Islander" = "O"))
beds2016$STUDENT_GENDER<-revalue(beds2016$STUDENT_GENDER, c("Male" = "M", "Female" = "F"))
beds2016$sped <- trimws(beds2016$CHALLENGE_TYPE)
beds2016$sped <- sub(" ", "")
beds2016.filter <- filter(beds2016, sped != "")
beds2016.filter$iep <- 1
beds2016.filter <- select(beds2016.filter, STUDENT_ID, iep)
beds2016b<-merge(beds2016, beds2016.filter, all=TRUE)
beds2016b$iep[is.na(beds2016b$iep)]<-0
beds2016b$schoolyear<-2016
beds2016b$CURR_GRADE_LVL<-revalue(beds2016b$CURR_GRADE_LVL, c("PKF" = "PK", "PKH" = "PK"))
colnames(beds2016b)[colnames(beds2016b)=="CURR_GRADE_LVL"]<-"grade"
colnames(beds2016b)[colnames(beds2016b)=="LOCATION_NAME"]<-"schoolname"
colnames(beds2016b)[colnames(beds2016b)=="POVERTY"]<-"frpl"
colnames(beds2016b)[colnames(beds2016b)=="ETHNIC_DESC"]<-"raceethnicity"
colnames(beds2016b)[colnames(beds2016b)=="STUDENT_GENDER"]<-"gender"
colnames(beds2016b)[colnames(beds2016b)=="LEP_ELIGIBILITY"]<-"ell"
colnames(beds2016b)[colnames(beds2016b)=="STUDENT_ID"]<-"offenderid"
beds2016.clean<-select(beds2016b, schoolyear, schoolname, offenderid, grade, raceethnicity, gender,
                       iep, ell, frpl)

#2015
beds2015<-read.csv("2015 BEDS.csv")
beds2015$POVERTY<-as.numeric(beds2015$POVERTY)
beds2015$POVERTY[beds2015$POVERTY==1]<-0
beds2015$POVERTY[beds2015$POVERTY==2]<-1
beds2015$LEP_ELIGIBILITY<-as.numeric(beds2015$LEP_ELIGIBILITY)
beds2015$LEP_ELIGIBILITY[beds2015$LEP_ELIGIBILITY==1]<-0
beds2015$LEP_ELIGIBILITY[beds2015$LEP_ELIGIBILITY==2]<-1
beds2015$ETHNIC_DESC<-revalue(beds2015$ETHNIC_DESC, c("Black or African American"="B", "White"="W", 
                                                      "American Indian or Alaska Native" = "O", "Hispanic or Latino" = "H",
                                                      "Multiracial" = "O", "Asian or Pacific Islander" = "O"))
beds2015$STUDENT_GENDER<-revalue(beds2015$STUDENT_GENDER, c("Male" = "M", "Female" = "F"))
beds2015$sped <- trimws(beds2015$CHALLENGE_TYPE)
beds2015$sped <- sub(" ", "")
beds2015.filter <- filter(beds2015, sped != "")
beds2015.filter$iep <- 1
beds2015.filter <- select(beds2015.filter, STUDENT_ID, iep)
beds2015b<-merge(beds2015, beds2015.filter, all=TRUE)
beds2015b$iep[is.na(beds2015b$iep)]<-0
beds2015b$schoolyear<-2015
beds2015b$CURR_GRADE_LVL<-revalue(beds2015b$CURR_GRADE_LVL, c("PKF" = "PK", "PKH" = "PK"))
colnames(beds2015b)[colnames(beds2015b)=="CURR_GRADE_LVL"]<-"grade"
colnames(beds2015b)[colnames(beds2015b)=="LOCATION_NAME"]<-"schoolname"
colnames(beds2015b)[colnames(beds2015b)=="POVERTY"]<-"frpl"
colnames(beds2015b)[colnames(beds2015b)=="ETHNIC_DESC"]<-"raceethnicity"
colnames(beds2015b)[colnames(beds2015b)=="STUDENT_GENDER"]<-"gender"
colnames(beds2015b)[colnames(beds2015b)=="LEP_ELIGIBILITY"]<-"ell"
colnames(beds2015b)[colnames(beds2015b)=="STUDENT_ID"]<-"offenderid"
beds2015.clean<-select(beds2015b, schoolyear, schoolname, offenderid, grade, raceethnicity, gender,
                       iep, ell, frpl)

#2014
beds2014<-read.csv("2014 BEDS.csv")
beds2014$POVERTY<-as.numeric(beds2014$POVERTY)
beds2014$POVERTY[beds2014$POVERTY==1]<-0
beds2014$POVERTY[beds2014$POVERTY==2]<-1
beds2014$LEP_ELIGIBILITY<-as.numeric(beds2014$LEP_ELIGIBILITY)
beds2014$LEP_ELIGIBILITY[beds2014$LEP_ELIGIBILITY==1]<-0
beds2014$LEP_ELIGIBILITY[beds2014$LEP_ELIGIBILITY==2]<-1
beds2014$ETHNIC_DESC<-revalue(beds2014$ETHNIC_DESC, c("Black or African American"="B", "White"="W", 
                                                      "American Indian or Alaska Native" = "O", "Hispanic or Latino" = "H",
                                                      "Multiracial" = "O", "Asian or Pacific Islander" = "O"))
beds2014$STUDENT_GENDER<-revalue(beds2014$STUDENT_GENDER, c("Male" = "M", "Female" = "F"))
beds2014$sped <- trimws(beds2014$CHALLENGE_TYPE)
beds2014$sped <- sub(" ", "")
beds2014.filter <- filter(beds2014, sped != "")
beds2014.filter$iep <- 1
beds2014.filter <- select(beds2014.filter, STUDENT_ID, iep)
beds2014b<-merge(beds2014, beds2014.filter, all=TRUE)
beds2014b$iep[is.na(beds2014b$iep)]<-0
beds2014b$schoolyear<-2014
beds2014b$CURR_GRADE_LVL<-revalue(beds2014b$CURR_GRADE_LVL, c("PKF" = "PK", "PKH" = "PK"))
colnames(beds2014b)[colnames(beds2014b)=="CURR_GRADE_LVL"]<-"grade"
colnames(beds2014b)[colnames(beds2014b)=="LOCATION_NAME"]<-"schoolname"
colnames(beds2014b)[colnames(beds2014b)=="POVERTY"]<-"frpl"
colnames(beds2014b)[colnames(beds2014b)=="ETHNIC_DESC"]<-"raceethnicity"
colnames(beds2014b)[colnames(beds2014b)=="STUDENT_GENDER"]<-"gender"
colnames(beds2014b)[colnames(beds2014b)=="LEP_ELIGIBILITY"]<-"ell"
colnames(beds2014b)[colnames(beds2014b)=="STUDENT_ID"]<-"offenderid"
beds2014.clean<-select(beds2014b, schoolyear, schoolname, offenderid, grade, raceethnicity, gender,
                       iep, ell, frpl)

#combine cleaned files
beds.all<-rbind(beds2017.clean, beds2016.clean, beds2015.clean, beds2014.clean)

beds.all$schoolname<-revalue(beds.all$schoolname, c(
  "DR KING ELEMENTARY SCHOOL"="Dr. King Elementary School",
  "DR WEEKS ELEMENTARY SCHOOL"="Dr. Weeks Elementary School", 
  "EDWARD SMITH K-8 SCHOOL" = "Edward Smith K-8",
  "EXPEDITIONARY LEARNING MIDDLE SCH" = "Expeditionary Learning MIddle School",
  "FRAZER K-8 SCHOOL" = "Frazer PK-8",
  "HUNTINGTON K-8 SCHOOL" = "Huntington PK-8",
  "HURLBUT W SMITH K-8 SCHOOL" = "H.W. Smith PK-8",
  "INSTITUTE OF TECH AT SYRACUSE CENTRA" = "Institute of Technology at Syracuse Central",
  "MCKINLEY-BRIGHTON ELEMENTARY" = "McKinley-Brighton Elementary School",
  "PUBLIC SERVICE LEADERSHIP ACA-FOWLER" = "Public Service Leadership Academy at Fowler",
  "ROBERTS K-8 SCHOOL" = "Roberts PK-8",
  "SYRACUSE LATIN SCHOOL" = "Syracuse Latin School",
  "Syracuse Latin School " = "Syracuse Latin School"))

###############enrollment distributions
beds.all$offenderid<-as.factor(beds.all$offenderid)
enroll.dist<-aggregate(offenderid ~ schoolyear + schoolname + gender + raceethnicity + iep + ell + 
                         frpl + grade, data=beds.all, FUN=length)
colnames(enroll.dist)[colnames(enroll.dist)=="offenderid"]<-"Enrollment"

all.susp.categories<-merge(all.susp.categories, enroll.dist, all=TRUE)

####replace NA with 0
all.susp.categories[is.na(all.susp.categories)]<-0

write.csv(all.susp.categories, "IM File 5.22.2017.csv")

##################Alternative Program Enrollment###############################
alt<-read.csv("Alt Enroll.csv")
library(lubridate)

alt$date <- mdy(alt$EntryDate) # puts into date format
alt$year<-year(alt$date)
alt$month<-month(alt$date)
alt$month<-sprintf("%02d",alt$month)
alt$day<-day(alt$date)
alt$day<-sprintf("%02d",alt$day)

alt$date.num <- as.numeric(paste0(alt$year, alt$month, alt$day)) 

alt$ytd<-ifelse(alt$date.num>20130901 & alt$date.num<20140201, 1, 
                ifelse(alt$date.num>20140901 & alt$date.num<20150201, 1, 
                       ifelse(alt$date.num>20150901 & alt$date.num<20160201, 1, 
                              ifelse(alt$date.num>20160821 & alt$date.num<20170201, 1, 0)
                       )
                )
)

#####filter to proper data set
library(plyr)
library(dplyr)
alt2<-filter(alt, ytd==1)
colnames(alt2)[colnames(alt2)=="SY"]<-"SchoolYear"


####aggregate by subgroup
alt3<-select(alt2, SchoolYear, StudentID, SchoolName, Gender, RaceEthnicity, IEP, ELL, FRPL, Grade )
alt.demo<-aggregate(StudentID ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL + FRPL + Grade, data=alt3, FUN=length)


#####merge with enrollment factor
colnames(alt.demo)[colnames(alt.demo)=="StudentID"]<-"OffenderID"
enroll.dist<-aggregate(OffenderID ~ SchoolYear + Gender + RaceEthnicity + IEP + ELL + 
                         FRPL + Grade, data=beds.all, FUN=length)
colnames(enroll.dist)[colnames(enroll.dist)=="OffenderID"]<-"Enrollment"

alt.demo$RaceEthnicity<-trimws(alt.demo$RaceEthnicity)
alt.demo$RaceEthnicity<-as.factor(alt.demo$RaceEthnicity)
alt.demo$SchoolYear<-as.factor(alt.demo$SchoolYear)
enroll.dist$SchoolYear<-as.factor(enroll.dist$SchoolYear)

alt.demo$RaceEthnicity<-revalue(alt.demo$RaceEthnicity, c("P"="A"))

alt.demo.enroll<-merge(alt.demo, enroll.dist, c("SchoolYear", "Gender", "RaceEthnicity", "IEP", "FRPL", "ELL", "Grade"))

#####################Creation of Tables
race.gender<-as.data.frame(aggregate(OSS ~ SchoolYear + RaceEthnicity + Gender, data = all.susp.categories, FUN = sum))
race.grade<-as.data.frame(aggregate(OSS ~ SchoolYear + RaceEthnicity + Grade, data = all.susp.categories, FUN = sum))
race.iep<-as.data.frame(aggregate(OSS ~ SchoolYear + RaceEthnicity + IEP, data = all.susp.categories, FUN = sum))
race.ELL<-as.data.frame(aggregate(OSS ~ SchoolYear + RaceEthnicity + ELL, data = all.susp.categories, FUN = sum))
race.FRPL<-as.data.frame(aggregate(OSS ~ SchoolYear + RaceEthnicity + FRPL, data = all.susp.categories, FUN = sum))
