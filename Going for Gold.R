library(stringi)
library(stringr)
library(tidyverse)
library(lubridate)
library(reshape2)


#####enrollment##################################
Beds<-read.csv("BEDSAllYears.csv",strip.white = TRUE)
Beds1<-Beds %>%
  filter(SY %in% c("2017","2018"))%>%
  mutate(ETHNIC_DESC=str_replace_all(ETHNIC_DESC,c("Black or African American"="B", "White"="O", 
                                                 "American Indian or Alaska Native" = "O", "Hispanic or Latino" = "O",
                                                 "Multiracial" = "O", "Asian or Pacific Islander" = "O")))%>%
  mutate(CHALLENGE_TYPE=sub("^$","0",CHALLENGE_TYPE))%>%
  mutate(CHALLENGE_TYPE=ifelse(!CHALLENGE_TYPE==0,1,0))%>%
  rename("IEP"="CHALLENGE_TYPE","RaceEthnicity"="ETHNIC_DESC","SchoolName"="LOCATION_NAME","SchoolYear"="SY")%>%
  mutate(Student=1)%>%
  mutate(SchoolName=str_replace_all(SchoolName,c("BELLEVUE ELEMENTARY SCHOOL"="Bellevue Elementary School",
                                                 "CLARY MIDDLE SCHOOL"="Clary Middle School",
                                                 "CORCORAN HIGH SCHOOL" = "Corcoran High School",
                                                 "DANFORTH MIDDLE SCHOOL" = "Danforth Middle School",
                                                 "DELAWARE ACADEMY" = "Delaware Academy - Primary",
                                                 "DELAWARE PRIMARY SCHOOL" = "Delaware Academy - Primary",
                                                 "DR WEEKS ELEMENTARY SCHOOL" ="Dr. Weeks Elementary School", 
                                                 "DR KING ELEMENTARY SCHOOL" = "Dr. King Elementary School",
                                                 "EDWARD SMITH K-8 SCHOOL" = "Edward Smith PK-8",
                                                 "EXPEDITIONARY LEARNING MIDDLE SCH"="Expeditionary Learning Middle School",
                                                 "FOWLER HIGH SCHOOL"="Public Service Leadership Academy at Fowler", 
                                                 "FRANKLIN ELEMENTARY SCHOOL" = "Franklin Elementary School",
                                                 "FRAZER K-8 SCHOOL" = "Frazer PK-8",
                                                 "GRANT MIDDLE SCHOOL" = "Grant Middle School",
                                                 "HENNINGER HIGH SCHOOL" = "Henninger High School",
                                                 "HUGHES ELEMENTARY SCHOOL"="Syracuse Latin - Hughes",
                                                 "HUNTINGTON K-8 SCHOOL"="Huntington PK-8",
                                                 "HURLBUT W SMITH K-8 SCHOOL" = "H.W. Smith PK-8",
                                                 "INSTITUTE OF TECH AT SYRACUSE CENTRA" = "Institute of Technology at Syracuse Central",
                                                 "LEMOYNE ELEMENTARY SCHOOL" = "Lemoyne Elementary School",
                                                 "LINCOLN MIDDLE SCHOOL" = "Lincoln Middle School",
                                                 "MCKINLEY-BRIGHTON ELEMENTARY"="McKinley-Brighton Elementary School",
                                                 "MEACHEM ELEMENTARY SCHOOL"="Meachem Elementary School", 
                                                 "NOTTINGHAM HIGH SCHOOL" = "Nottingham High School",
                                                 "PORTER ELEMENTARY SCHOOL" = "Porter Elementary School",
                                                 "PUBLIC SERVICE LEADERSHIP ACA-FOWLER" = "Public Service Leadership Academy at Fowler",
                                                 "ROBERTS K-8 SCHOOL" = "Roberts PK-8",
                                                 "SALEM HYDE ELEMENTARY SCHOOL"="Salem Hyde Elementary School",
                                                 "SEYMOUR DUAL LANGUAGE ACADEMY"="Seymour Dual Language Academy",
                                                 "SYRACUSE LATIN SCHOOL" = "Syracuse Latin - Hughes",
                                                 "VAN DUYN ELEMENTARY SCHOOL" = "Van Duyn Elementary School",
                                                 "WEBSTER ELEMENTARY SCHOOL" = "Webster Elementary School",
                                                 "WESTSIDE ACADEMY AT BLODGETT" = "Westside Academy at Blodgett",
                                                 "PK - SYRACUSE CITY SD"="UPK")))%>%
  select("SchoolYear","SchoolName","RaceEthnicity","IEP","Student")%>%
  group_by(SchoolYear,SchoolName,RaceEthnicity,IEP)%>%
  summarise(TotalStudents=sum(Student))

###DISCIPLINE####
ehoutallyears<-read.csv("ehoutallyears.csv")
ehoutallyears1<-ehoutallyears %>%
  mutate(date=mdy(IncidentDate))%>%
  mutate(year=year(date))%>%
  mutate(month=month(date))%>%
  mutate(month=sprintf("%02d",month))%>%
  mutate(day=day(date))%>%
  mutate(day=sprintf("%02d",day))%>%
  mutate(date.num=as.numeric(paste0(year,month,day)))%>%
  mutate(SchoolName=str_replace_all(SchoolName,c("P-Tech"="Institute of Technology at Syracuse Central",
                                             "Hughes Elementary School"="Syracuse Latin - Hughes",
                                             "Syracuse Latin School"="Syracuse Latin - Hughes",
                                             "Montessori School @ Lemoyne"="Lemoyne Elementary School",
                                             "GPS Elementary Program"="MSAP - CORE",
                                             "Delaware Academy"="Delaware Academy - Primary",
                                             "Delaware Primary School"="Delaware Academy - Primary",
                                             "Twilight Academy @ Corcoran"="Corcoran High School",
                                             "Twilight Academy @ Nottingham" = "Nottingham High School",
                                             "Twilight Academy @ PSLA" = "Public Service Leadership Academy at Fowler",
                                             "Twilight Academy @ Henninger" = "Henninger High School",
                                             "Johnson Center Epic Program"="Johnson Center",
                                             "Johnson Center Transition Program"="Johnson Center",
                                             "Fowler High School"="Public Service Leadership Academy at Fowler")))%>%
  mutate(RaceEthnicity=str_replace_all(RaceEthnicity,c("I"="O","M"="O","P"="O","A"="O","W"="O","H"="O")))%>%
  mutate(ytd=ifelse(date.num>20160821 &date.num<20161117, 1,
                    ifelse(date.num>20170821 & date.num<20171117,1,0)))%>%
  filter(ytd==1)%>%
  mutate(Ref=1)%>%
  select(SchoolYear,SchoolName,Ref,OSS,RaceEthnicity,IEP)%>%
  group_by(SchoolYear,SchoolName,RaceEthnicity,IEP)%>%
  summarise(Referrals=sum(Ref),OSS=sum(OSS))

alldata<-merge(ehoutallyears1,Beds1,by=c("SchoolName","RaceEthnicity","IEP","SchoolYear"),all=TRUE)


alldata[is.na(alldata)]<-0
RaceData<-alldata %>%
select(SchoolYear,SchoolName,RaceEthnicity,Referrals,OSS,TotalStudents)%>%
group_by(SchoolYear,SchoolName,RaceEthnicity)%>%
summarise(OSS=sum(OSS),Referrals=sum(Referrals),TotalStudents=sum(TotalStudents))%>%
melt(id.vars=c("SchoolYear","SchoolName","RaceEthnicity"))%>%
  dcast(SchoolName~RaceEthnicity+variable+SchoolYear)%>%
mutate(TotalOSS2017=(B_OSS_2017+O_OSS_2017))%>%
mutate(TotalStudents2017=(B_TotalStudents_2017+O_TotalStudents_2017))%>%
mutate(TotalReferrals2017=(B_Referrals_2017+O_Referrals_2017))%>%
mutate(TotalOSS2018=(B_OSS_2018+O_OSS_2018))%>%
mutate(TotalStudents2018=(B_TotalStudents_2018+O_TotalStudents_2018))%>%
mutate(TotalReferrals2018=(B_Referrals_2018+O_Referrals_2018))%>%
mutate(OSSRate2017=TotalOSS2017/TotalStudents2017)%>%
mutate(OSSRate2018=TotalOSS2018/TotalStudents2018)%>%
mutate(OSSDisprop2017=(((B_OSS_2017/TotalOSS2017))-(B_TotalStudents_2017/TotalStudents2017)))%>%
mutate(RefDisprop2017=(((B_Referrals_2017/TotalReferrals2017))-(B_TotalStudents_2017/TotalStudents2017)))%>%
mutate(OSSDisprop2018=(((B_OSS_2018/TotalOSS2018))-(B_TotalStudents_2018/TotalStudents2018)))%>%
mutate(RefDisprop2018=(((B_Referrals_2018/TotalReferrals2018))-(B_TotalStudents_2018/TotalStudents2018)))%>%
mutate(RaceOSSRateChange=OSSRate2018-OSSRate2017)%>%
mutate(RaceOSSDispropChange=OSSDisprop2018-OSSDisprop2017)%>%
mutate(RaceRefDispropChange=RefDisprop2018-RefDisprop2017)%>%
select(SchoolName,RaceOSSRateChange,RaceOSSDispropChange,RaceRefDispropChange)

IEPData<-alldata %>%
  select(SchoolYear,SchoolName,IEP,Referrals,OSS,TotalStudents)%>%
  group_by(SchoolYear,SchoolName,IEP)%>%
  summarise(OSS=sum(OSS),Referrals=sum(Referrals),TotalStudents=sum(TotalStudents))%>%
  melt(id.vars=c("SchoolYear","SchoolName","IEP"))%>%
  dcast(SchoolName~IEP+variable+SchoolYear)%>%
  mutate(TotalOSS2017=(`0_OSS_2017`+`1_OSS_2017`))%>%
  mutate(TotalStudents2017=(`1_TotalStudents_2017`+`0_TotalStudents_2017`))%>%
  mutate(TotalReferrals2017=(`1_Referrals_2017`+`0_Referrals_2017`))%>%
  mutate(TotalOSS2018=(`1_OSS_2018`+`0_OSS_2018`))%>%
  mutate(TotalStudents2018=(`1_TotalStudents_2018`+`0_TotalStudents_2018`))%>%
  mutate(TotalReferrals2018=(`1_Referrals_2018`+`0_Referrals_2018`))%>%
  mutate(OSSRate2017=TotalOSS2017/TotalStudents2017)%>%
  mutate(OSSRate2018=TotalOSS2018/TotalStudents2018)%>%
  mutate(OSSDisprop2017=(((`1_OSS_2017`/TotalOSS2017))-(`1_TotalStudents_2017`/TotalStudents2017)))%>%
  mutate(RefDisprop2017=(((`1_Referrals_2017`/TotalReferrals2017))-(`1_TotalStudents_2017`/TotalStudents2017)))%>%
  mutate(OSSDisprop2018=(((`1_OSS_2018`/TotalOSS2018))-(`1_TotalStudents_2018`/TotalStudents2018)))%>%
  mutate(RefDisprop2018=(((`1_Referrals_2018`/TotalReferrals2018))-(`1_TotalStudents_2018`/TotalStudents2018)))%>%
  mutate(IEPOSSRateChange=OSSRate2018-OSSRate2017)%>%
  mutate(IEPOSSDispropChange=OSSDisprop2018-OSSDisprop2017)%>%
  mutate(IEPRefDispropChange=RefDisprop2018-RefDisprop2017)%>%
  select(SchoolName,IEPOSSRateChange,IEPOSSDispropChange,IEPRefDispropChange)

Total<-merge(IEPData,RaceData,by="SchoolName",all=TRUE)
Total$SchoolBand<-revalue(Total$SchoolName,c(
    "Public Service Leadership Academy at Fowler"="HS","Johnson Center"="ALT","Nottingham High School"="HS",
    "Corcoran High School"="HS","McCarthy at Beard School"="ALT","Henninger High School"="HS",                 
    "Institute of Technology at Syracuse Central"="HS","Clary Middle School"="MS","Elmcrest Children's Center School"="ALT",
    "Edward Smith PK-8"="K8","Westside Academy at Blodgett"="MS" ,"Grant Middle School"="MS",
    "Frazer PK-8"="K8","Huntington PK-8"="K8","Lincoln Middle School"="MS",
    "Expeditionary Learning Middle School"="MS","H.W. Smith PK-8"="K8","Danforth Middle School"="MS",
    "Roberts PK-8"="K8","CORE"="ALT","Salem Hyde Elementary School" ="E",
    "Porter Elementary School"="E","Bellevue Elementary School"="E" ,
    "Dr. King Elementary School"="E","Dr. Weeks Elementary School"="E" ,"Meachem Elementary School"="E",
    "Seymour Dual Language Academy"="E" ,"McKinley-Brighton Elementary School"="E", "Webster Elementary School"="E" ,
    "Van Duyn Elementary School" ="E","Franklin Elementary School"="E","Delaware Academy - Primary"="E",
    "Lemoyne Elementary School"="E","Syracuse Latin - Hughes"="E"))

write.csv(Total, "C:/Users/drobil66/Desktop/RFiles/R Reports/GoingForGold.csv")


