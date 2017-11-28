library(lubridate)
library(tidyverse)
library(stringi)
library(stringr)
attendance<-read.csv("attendance2018.csv")
rawehout<-read.csv("ehout2018.csv",strip.white = TRUE)
rawattendance<-attendance

rawattendance$Rptg.Race.Ethnicity.Desc<-str_replace_all(rawattendance$Rptg.Race.Ethnicity.Desc,c("Black or African American"="B","Asian"="AO",
                                                           "White"="W","Hispanic"="H","Multiracial"="AO",
                                                           "Native Hawaiian / Other Pacific Islander"="AO",
                                                           "American Indian or Alaska native"="AO"))
rawattendance$Male<-as.character(rawattendance$Male)
rawattendance$Male<-str_replace_all(rawattendance$Male,c("0"="F","1"="M"))
rawattendance$IEP<-as.character(rawattendance$Has.Iep)
rawattendance$IEP<-str_replace_all(rawattendance$IEP,c("Y"="1","N"="0"))
rawattendance$Attendance.Location.Name<-str_replace_all(rawattendance$Attendance.Location.Name,
              c("Edward Smith Middle"="Edward Smith K-8","Edward Smith Elementary School"="Edward Smith K-8",
                "GPS Elementary Program"="MSAP/CORE","Frazer Elementary School" = "Frazer PK-8",
                "Frazer Middle School"="Frazer PK-8","Huntington Elementary School"="Huntington PK-8",
                "Huntington Middle School"="Huntington PK-8","Hurlbut W. Smith Elementary School"="H.W. Smith PK-8",
                "Hurlbut W. Smith Middle School"="H.W. Smith PK-8","Johnson Center Transition Program"="Johnson Center",
                "JVC EPIC Program"="Johnson Center","Montessori School @ Lemoyne"="Lemoyne Elementary School",
                "McKinley - Brighton Elementary School"="McKinley-Brighton Elementary School",
                "P-Tech"="Institute of Technology at Syracuse Central", 
                "Roberts Elementary School"="Roberts PK-8","Roberts Middle School"="Roberts PK-8",
                "Twilight Academy @ Corcoran"="Corcoran High School", "Twilight Academy @ Henninger"="Henninger High School",
                "Twilight Academy @ Nottingham"="Nottingham High SChool","Twilight Academy @ PSLA"="Public Service Leadership Academy at Fowler"
                  ))


buildingattendance <- rawattendance %>%
  mutate(Day=1)%>%
  mutate(Pres=ifelse(Attendance.Desc=="Present",1,0))%>%
  mutate(Abs=ifelse(Pres==0,1,0))%>%
  group_by(Student.Id,Rptg.Race.Ethnicity.Desc,IEP,Male,ENL,Attendance.Location.Name)%>%
  summarise(TotalEnrolled=sum(Day),DaysPresent=sum(Pres),DaysAbsent=sum(Abs))%>%
  mutate(AttendancePercentage=DaysPresent/TotalEnrolled)%>%
  rename(Race=Rptg.Race.Ethnicity.Desc,Gender=Male,EnrollmentBuilding=Attendance.Location.Name)%>%
  select(Student.Id,EnrollmentBuilding,Race,Gender,IEP,ENL,AttendancePercentage)%>%
  mutate(ChronicStatus=ifelse(AttendancePercentage>=.95,1,ifelse(AttendancePercentage>=.9,2,
                                                                 ifelse(AttendancePercentage>=.8,3,4))))%>%
  mutate(Chronic=ifelse(AttendancePercentage>=.9,0,1))%>%
  mutate(NotChronic=ifelse(Chronic<1,1,0))%>%
  mutate(AttendancePercentage=(paste(round((AttendancePercentage)*100,digits=1),"%",sep="")))%>%
  mutate(SeverelyChronic=ifelse(ChronicStatus==4,1,0))%>%
  mutate(ModeratelyChronic=ifelse(ChronicStatus==3,1,0))%>%
  mutate(MildlyChronic=ifelse(ChronicStatus==2,1,0))%>%
  mutate(Satisfactory=ifelse(ChronicStatus==1,1,0))%>%
  group_by(EnrollmentBuilding,Race,ENL,IEP,Gender)%>%
  summarise(TotalChronic=sum(Chronic),TotalNotChronic=sum(NotChronic),SeverelyChronic=sum(SeverelyChronic),
            ModeratelyChronic=sum(ModeratelyChronic),MildlyChronic=sum(MildlyChronic),Satisfactory=sum(Satisfactory))%>%
  mutate(TotalStudents=TotalNotChronic+TotalChronic)


buildingattendance$SchoolType<-ifelse(buildingattendance$EnrollmentBuilding=="Bellevue Elementary School"|
                                        buildingattendance$EnrollmentBuilding=="Delaware Primary School"|
                                        buildingattendance$EnrollmentBuilding=="Delaware Academy"|
                                        buildingattendance$EnrollmentBuilding=="Delaware Primary School"|
                                        buildingattendance$EnrollmentBuilding=="Dr. King Elementary School"|
                                        buildingattendance$EnrollmentBuilding=="Dr. Weeks Elementary School"|
                                        buildingattendance$EnrollmentBuilding=="Franklin Elementary School"|
                                        buildingattendance$EnrollmentBuilding=="Hughes Elementary School"|
                                        buildingattendance$EnrollmentBuilding=="Lemoyne Elementary School"|
                                        buildingattendance$EnrollmentBuilding=="McKinley-Brighton Elementary School"|
                                        buildingattendance$EnrollmentBuilding=="Meachem Elementary School"|
                                        buildingattendance$EnrollmentBuilding=="Porter Elementary School"|
                                        buildingattendance$EnrollmentBuilding=="Salem Hyde Elementary School"|
                                        buildingattendance$EnrollmentBuilding=="Seymour Dual Language Academy"|
                                        buildingattendance$EnrollmentBuilding=="Syracuse Latin School"|
                                        buildingattendance$EnrollmentBuilding=="Van Duyn Elementary School"|
                                        buildingattendance$EnrollmentBuilding=="Webster Elementary School",
                                       "Elementary Schools",ifelse(buildingattendance$EnrollmentBuilding=="Clary Middle School"|
                                        buildingattendance$EnrollmentBuilding=="Danforth Middle School"|
                                        buildingattendance$EnrollmentBuilding=="Grant Middle School"|
                                        buildingattendance$EnrollmentBuilding=="Lincoln Middle School"|
                                        buildingattendance$EnrollmentBuilding=="Westside Academy at Blodgett"|
                                        buildingattendance$EnrollmentBuilding=="Expeditionary Learning Middle School",
                                        "Middle Schools",ifelse(buildingattendance$EnrollmentBuilding=="Edward Smith K-8"|
                                        buildingattendance$EnrollmentBuilding=="Frazer PK-8"|
                                        buildingattendance$EnrollmentBuilding=="Huntington PK-8"|
                                          buildingattendance$EnrollmentBuilding=="Roberts PK-8"|
                                          buildingattendance$EnrollmentBuilding=="H.W. Smith PK-8",
                                        "K-8 Schools",ifelse(buildingattendance$EnrollmentBuilding=="Corcoran High School"|
                                        buildingattendance$EnrollmentBuilding=="Henninger High School"|
                                          buildingattendance$EnrollmentBuilding=="Institute of Technology at Syracuse Central"|
                                          buildingattendance$EnrollmentBuilding=="Public Service Leadership Academy at Fowler"|
                                          buildingattendance$EnrollmentBuilding=="Nottingham High School",
                                        "High Schools",ifelse(buildingattendance$EnrollmentBuilding=="Elmcrest Children's Center School"|
                                        buildingattendance$EnrollmentBuilding=="Johnson Center"|
                                        buildingattendance$EnrollmentBuilding=="McCarthy at Beard School"|
                                        buildingattendance$EnrollmentBuilding=="MSAP/CORE",
                                        "Alternative Programs","NA")))))


rawehout$RaceEthnicity<-str_replace_all(rawehout$RaceEthnicity,c("A"="AO","M"="AO","P"="AO","I"="AO"))
rawehout$SchoolName<-str_replace_all(rawehout$SchoolName,c("CORE"="MSAP/CORE","Johnson Center Transition Program"="Johnson Center",
                                                   "Johnson Center Epic Program"="Johnson Center", "Montessori at LeMoyne"="Lemoyne Elementary School",
                                                   "Syracuse Latin School "="Syracuse Latin School","Edward Smith PK-8"="Edward Smith K-8"))

rawehout$SchoolType<-ifelse(rawehout$SchoolName=="Bellevue Elementary School"|
                                        rawehout$SchoolName=="Delaware Primary School"|
                                        rawehout$SchoolName=="Delaware Academy"|
                                        rawehout$SchoolName=="Delaware Primary School"|
                                        rawehout$SchoolName=="Dr. King Elementary School"|
                                        rawehout$SchoolName=="Dr. Weeks Elementary School"|
                                        rawehout$SchoolName=="Franklin Elementary School"|
                                        rawehout$SchoolName=="Hughes Elementary School"|
                                        rawehout$SchoolName=="Lemoyne Elementary School"|
                                        rawehout$SchoolName=="McKinley-Brighton Elementary School"|
                                        rawehout$SchoolName=="Meachem Elementary School"|
                                        rawehout$SchoolName=="Porter Elementary School"|
                                        rawehout$SchoolName=="Salem Hyde Elementary School"|
                                        rawehout$SchoolName=="Seymour Dual Language Academy"|
                                        rawehout$SchoolName=="Syracuse Latin School"|
                                        rawehout$SchoolName=="Van Duyn Elementary School"|
                                        rawehout$SchoolName=="Webster Elementary School",
                                      "Elementary Schools",ifelse(rawehout$SchoolName=="Clary Middle School"|
                                                   rawehout$SchoolName=="Danforth Middle School"|
                                                   rawehout$SchoolName=="Grant Middle School"|
                                                   rawehout$SchoolName=="Lincoln Middle School"|
                                                   rawehout$SchoolName=="Westside Academy at Blodgett"|
                                                   rawehout$SchoolName=="Expeditionary Learning Middle School",
                                                 "Middle Schools",ifelse(rawehout$SchoolName=="Edward Smith K-8"|
                                                              rawehout$SchoolName=="Frazer PK-8"|
                                                                rawehout$SchoolName=="Roberts PK-8"|
                                                              rawehout$SchoolName=="Huntington PK-8"|
                                                              rawehout$SchoolName=="H.W. Smith PK-8",
                                                            "K-8 Schools",ifelse(rawehout$SchoolName=="Corcoran High School"|
                                                                          rawehout$SchoolName=="Henninger High School"|
                                                                          rawehout$SchoolName=="Institute of Technology at Syracuse Central"|
                                                                          rawehout$SchoolName=="Public Service Leadership Academy at Fowler"|
                                                                          rawehout$SchoolName=="Nottingham High School",
                                                                        "High Schools",ifelse(rawehout$SchoolName=="Elmcrest Children's Center School"|
                                                                                     rawehout$SchoolName=="Johnson Center"|
                                                                                     rawehout$SchoolName=="McCarthy at Beard School"|
                                                                                     rawehout$SchoolName=="MSAP/CORE",
                                                                                   "Alternative Schools","NA")))))



studentehout<-rawehout %>%
  filter(IncidentType=="referral")%>%
  mutate(Refcount=1)%>%
  group_by(OffenderID,SchoolName,ELL,IEP,Gender,RaceEthnicity)%>%
  summarise(TotalOSS=sum(OSS),TotalReferrals=sum(Refcount),TotalOSSDays=sum(OSS.Days)) %>%
  mutate(FivePlus=ifelse(TotalReferrals>=5,1,0))%>%
  mutate(TwotoFive=ifelse(TotalReferrals<5 & TotalReferrals>=2,1,0))%>%
  mutate(ZeroOne=ifelse(TotalReferrals<=1,1,0))%>%
  mutate(RecidOSS=ifelse(TotalOSS>1,1,0))%>%
  mutate(OneOSS=ifelse(TotalOSS>0,1,0))
  

buildingehout<-studentehout%>%
  group_by(SchoolName,ELL,IEP,Gender,RaceEthnicity)%>%
  summarise(TotalOSS=sum(TotalOSS),TotalReferrals=sum(TotalReferrals),TotalOSSDays=sum(TotalOSSDays),
            TotalUniqueOSS=sum(OneOSS),TotalRecidOSS=sum(RecidOSS),TotalFivePlus=sum(FivePlus),
            TotalTwotoFive=sum(TwotoFive),TotalZeroOne=sum(ZeroOne))%>%
  rename(EnrollmentBuilding=SchoolName,ENL=ELL,Race=RaceEthnicity)

PSPData=merge(buildingehout,buildingattendance,by=c("EnrollmentBuilding","ENL","Race","IEP","Gender"),all=TRUE)
PSPData[is.na(PSPData)]<-0


write.csv(PSPData,"C:/Users/drobil66/Desktop/RFiles/R Reports/PSPData.csv")

