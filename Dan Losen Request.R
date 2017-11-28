ehout<-read.csv("ehoutallyears.csv",strip.white = TRUE)
library(lubridate)
library(plyr)
library(dplyr)

ehout<-rename(ehout,Total.Suspension=Total.S12pension)
ehout<-rename(ehout,Total.Suspension.Duration=Total.S12p.Duration)


#cleaning up school names
ehout$SchoolName<-revalue(ehout$SchoolName, c(
  "Edward Smith K-8 School" = "Edward Smith K-8",
  "Frazer PK-8" = "Frazer PK-8 School",
  "Frazer K-8 School" = "Frazer PK-8 School",
  "Guided Prevention Services (GPS)" = "CORE",
  "Hurlbut W. Smith K-8"= "H.W. Smith PK-8",
  "Hughes K-8"="Hughes Elementary School",
  "Huntington K-8 School" = "Huntington PK-8",
  "McKinley - Brighton Elementary School" = "McKinley-Brighton Elementary School",
  "Middle School Alternative Program" = "CORE",
  "Roberts K-8 School" = "Roberts PK-8",
  "Syracuse Latin School " = "Syracuse Latin School"
))


ehout$Classroom<-ifelse(ehout$Location=="Classroom",1,0)


###parse dates and create time intervals
library(lubridate)
ehout$date <- mdy(ehout$IncidentDate) # puts into date format
ehout$year<-year(ehout$date) # parse year
ehout$month<-month(ehout$date) # parse month
ehout$month<-sprintf("%02d",ehout$month) # create 2 digit month
ehout$day<-day(ehout$date) # parse day
ehout$day<-sprintf("%02d",ehout$day) # create 2 digit day

ehout$date.num <- as.numeric(paste0(ehout$year, ehout$month, ehout$day)) # concatenate

#ytd calculations
ehout$Semester<-ifelse(ehout$date.num>=20130901 & ehout$date.num<=20140131, 1, 
                       ifelse(ehout$date.num>=20140201 & ehout$date.num<=20150630, 2, 
                              ifelse(ehout$date.num>=20140901 & ehout$date.num<=20150131, 1, 
                                     ifelse(ehout$date.num>=2015201 & ehout$date.num<=20150630, 2,
                                            ifelse(ehout$date.num>=20150901 & ehout$date.num<=20160131, 1, 
                                                   ifelse(ehout$date.num>=20160201 & ehout$date.num<=20160630, 2, 
                                                          ifelse(ehout$date.num>=20160822 & ehout$date.num<=20170131, 1, 
                                                                 ifelse(ehout$date.num>=20170201 & ehout$date.num<=20170630, 2,0))))))))


ehout3<-filter(ehout, Grade!=15)

####data analysis for all incidents

ehout3$inccount=1

ehout3$RaceEthnicity<-revalue(ehout3$RaceEthnicity, c("P" = "API","A"="API"))

total.OSS<-as.data.frame(aggregate(OSS ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom, data=ehout3, FUN=sum))
total.OSSdays<-as.data.frame(aggregate(OSS.Days ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom, data=ehout3, FUN=sum))
total.iss<-as.data.frame(aggregate(ISS ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,  data=ehout3, FUN=sum))
total.issdays<-as.data.frame(aggregate(ISS.Days ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,  data=ehout3, FUN=sum))
total.susp<-as.data.frame(aggregate(Total.Suspension ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,  data=ehout3, FUN=sum))
total.suspdays<-as.data.frame(aggregate(Total.Suspension.Duration ~ SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,  data=ehout3, FUN=sum))
total.referral<-as.data.frame(aggregate(inccount~SchoolYear + SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,  data=ehout3, FUN=sum))

all.susp.categories<-merge(total.OSS, total.OSSdays)
all.susp.categories<-merge(all.susp.categories, total.iss)
all.susp.categories<-merge(all.susp.categories, total.issdays)
all.susp.categories<-merge(all.susp.categories, total.susp)
all.susp.categories<-merge(all.susp.categories, total.suspdays)
all.susp.categories<-merge(all.susp.categories,total.referral)

####data analysis for unique students
OSSbyYear<-aggregate(OSS~OffenderID+SchoolYear+SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,data=ehout3,FUN=sum)
ISSbyYear<-aggregate(ISS~OffenderID+SchoolYear+ SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,data=ehout3,FUN=sum)
RefbyYear<-aggregate(inccount~OffenderID+SchoolYear+ SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,data=ehout3,FUN=sum)
TotSuspbyYear<-aggregate(Total.Suspension~OffenderID+SchoolYear+ SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,data=ehout3,FUN=sum)
OSSbyYear$RepeatOSS<-ifelse(OSSbyYear$OSS>=2, 1,0)
OSSbyYear$OneOSS<-ifelse(OSSbyYear$OSS>=1, 1,0)
ISSbyYear$RepeatISS<-ifelse(ISSbyYear$ISS>=2, 1,0)
ISSbyYear$OneISS<-ifelse(ISSbyYear$ISS>=1, 1,0)
RefbyYear$RepeatRef<-ifelse(RefbyYear$inccount>=2, 1,0)
RefbyYear$OneRef<-ifelse(RefbyYear$inccount>=1, 1,0)
TotSuspbyYear$OneSusp<-ifelse(TotSuspbyYear$Total.Suspension>=1,1,0)
TotSuspbyYear$RepeatSusp<-ifelse(TotSuspbyYear$Total.Suspension>=2,1,0)

RepeatoSS<-aggregate(RepeatOSS~SchoolYear+SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,data=OSSbyYear,FUN=sum)
OneoSS<-aggregate(OneOSS~SchoolYear+SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,data=OSSbyYear,FUN=sum)
RepeatISS<-aggregate(RepeatISS~SchoolYear+SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,data=ISSbyYear,FUN=sum)
OneISS<-aggregate(OneISS~SchoolYear+SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,data=ISSbyYear,FUN=sum)
RepeatRef<-aggregate(RepeatRef~SchoolYear+SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,data=RefbyYear,FUN=sum)
OneRef<-aggregate(OneRef~SchoolYear+SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,data=RefbyYear,FUN=sum)
OneSusp<-aggregate(OneSusp~SchoolYear+SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,data=TotSuspbyYear,FUN=sum)
RepeatSusp<-aggregate(RepeatSusp~SchoolYear+SchoolName + Gender + RaceEthnicity + IEP + ELL+Classroom,data=TotSuspbyYear,FUN=sum)



total.repeats<-merge(RepeatoSS,OneoSS)
total.repeats<-merge(total.repeats,RepeatISS)
total.repeats<-merge(total.repeats,OneISS)
total.repeats<-merge(total.repeats,RepeatRef)
total.repeats<-merge(total.repeats,OneRef)
total.repeats<-merge(total.repeats,OneSusp)
total.repeats<-merge(total.repeats,RepeatSusp)


AllDiscipline<-merge(total.repeats,all.susp.categories,by=c("SchoolYear","SchoolName","Gender","RaceEthnicity","IEP","ELL","Classroom"),all=TRUE)

write.csv(AllDiscipline, "C:/Users/drobil66/Desktop/RFiles/R Reports/LosenDiscipline.csv")

