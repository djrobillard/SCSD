#1) 

#PSP Data for AOD Report
library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)

ehout2017<-read.csv("ehout2017.csv",strip.white = TRUE)
BEDS2017<-read.csv("2017 BEDS.csv",strip.white = TRUE)


###############DISTRICT LEVEL REPORT###############################
#Pulling in BEDS 2017
BEDS2017$StudentCount<-1
Racetable<-aggregate(StudentCount~ETHNIC_DESC,data=BEDS2017,FUN = sum)

BEDS2017$CHALLENGE_TYPE<-sub("^$","0",BEDS2017$CHALLENGE_TYPE)
BEDS2017$CHALLENGE_TYPE<-revalue(BEDS2017$CHALLENGE_TYPE, c("Learning Disability"="1", "Other Health Impairment"="1", 
                                                      "Intellectual Disability" = "1", "Autism" = "1",
                                                      "Multiple Disabilities" = "1", "Emotional Disturbance" = "1",
                                                      "Speech or Language Impairment"="1", "Traumatic Brain Injury"="1", 
                                                      "Orthopedic Impairment" = "1", "Pre-School Student with a Disability" = "1",
                                                      "Visual Impairment (includes Blindness)" = "1", "Hearing Impairment" = "1"))

BEDS2017$LOCATION_NAME<-revalue(BEDS2017$LOCATION_NAME, c(
  "BELLEVUE ELEMENTARY SCHOOL"="Bellevue Elementary School",
  "CLARY MIDDLE SCHOOL"="Clary Middle School",
  "CORCORAN HIGH SCHOOL" = "Corcoran High School",
  "DANFORTH MIDDLE SCHOOL" = "Danforth Middle School",
  "DELAWARE ACADEMY" = "Delaware Academy",
  "DELAWARE PRIMARY SCHOOL" = "Delaware Primary School",
  "DR WEEKS ELEMENTARY SCHOOL" ="Dr. Weeks Elementary School", 
  "DR KING ELEMENTARY SCHOOL" = "Dr. King Elementary School",
  "EDWARD SMITH K-8 SCHOOL" = "Edward Smith K-8",
  "EXPEDITIONARY LEARNING MIDDLE SCH"="Expeditionary Learning Middle School",
  "FOWLER HIGH SCHOOL"="Fowler High School", 
  "FRANKLIN ELEMENTARY SCHOOL" = "Franklin Elementary School",
  "FRAZER K-8 SCHOOL" = "Frazer PK-8",
  "GRANT MIDDLE SCHOOL" = "Grant Middle School",
  "HENNINGER HIGH SCHOOL" = "Henninger High School",
  "HUGHES ELEMENTARY SCHOOL"="Hughes Elementary School",
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
  "SYRACUSE LATIN SCHOOL" = "Syracuse Latin School",
  "VAN DUYN ELEMENTARY SCHOOL" = "Van Duyn Elementary School",
  "WEBSTER ELEMENTARY SCHOOL" = "Webster Elementary School",
  "WESTSIDE ACADEMY AT BLODGETT" = "Westside Academy at Blodgett",
  "PK - SYRACUSE CITY SD"="UPK"
  ))

BEDS2017$LOCATION_NAME<-trimws(BEDS2017$LOCATION_NAME)

BEDS2017$GradeBand<-revalue(BEDS2017$CURR_GRADE_LVL,c("KF"="PK-2","PKH"="PK-2","PKF"="PK-2","1"="PK-2","2"="PK-2","U2"="PK-2",
                                                 "3"="3-5","4"="3-5","5"="3-5","U4"="3-5","U3"="3-5",
                                                 "6"="6-8","7"="6-8","8"="6-8","U6"="6-8","U7"="6-8","U8"="6-8",
                                                 "9"="9-12","10"="9-12","11"="9-12","12"="9-12","T+"="9-12","US"="9-12","14"="9-12","13"="3-5"))

BEDS2017$ETHNIC_DESC<-revalue(BEDS2017$ETHNIC_DESC, c("Black or African American"="B", "White"="W", 
                                                                "American Indian or Alaska Native" = "I", "Hispanic or Latino" = "H",
                                                                "Multiracial" = "M", "Asian or Pacific Islander" = "A"))

Enrolltable<-aggregate(StudentCount~ETHNIC_DESC+CHALLENGE_TYPE+LOCATION_NAME+GradeBand,data=BEDS2017,FUN = sum)
Enrolltable<-rename(Enrolltable,IEP=CHALLENGE_TYPE)
Enrolltable<-rename(Enrolltable,RaceEthnicity=ETHNIC_DESC)
Enrolltable<-rename(Enrolltable,SchoolName=LOCATION_NAME)
Enrolltable$IEP<-as.integer(Enrolltable$IEP)

schoolracepop<-Enrolltable %>%
  select(SchoolName,IEP,RaceEthnicity,StudentCount)%>%
  group_by(SchoolName,RaceEthnicity) %>%
  summarise(RaceStudentCount=sum(StudentCount))
  
schooldispropracepop<-Enrolltable %>%
  select(SchoolName,IEP,RaceEthnicity,StudentCount)%>%
  group_by(SchoolName,RaceEthnicity) %>%
  summarise(StudentCount=sum(StudentCount))%>%
  melt(id.vars=c("SchoolName","RaceEthnicity"))%>%
  dcast(SchoolName~RaceEthnicity)%>%
  mutate(TotalPop=I+A+B+H+M+W)%>%
  rename(IPop=I,APop=A,BPop=B,HPop=H,MPop=M,WPop=W)

schoolIEPpop<-Enrolltable %>%
  select(SchoolName,IEP,RaceEthnicity,StudentCount)%>%
  group_by(SchoolName,IEP) %>%
  summarise(IEPStudentCount=sum(StudentCount))
  
schooldispropIEPpop<-Enrolltable %>%
  select(SchoolName,IEP,RaceEthnicity,StudentCount)%>%
  group_by(SchoolName,IEP) %>%
  summarise(StudentCount=sum(StudentCount))%>%
  melt(id.vars=c("SchoolName","IEP"))%>%
  dcast(SchoolName~IEP)%>%
  mutate(TotalPop=`1`+`0`)%>%
  rename(GenEdPop=`0`,IEPPop=`1`)
  
districtracepop<-Enrolltable %>%
  select(RaceEthnicity,StudentCount,GradeBand)%>%
  group_by(RaceEthnicity) %>%
  summarise(RaceStudentCount=sum(StudentCount))%>%
  mutate(District="District")%>%
  melt(id.vars=c("RaceEthnicity","District"))%>%
  dcast(District~RaceEthnicity)%>%
  mutate(TotalPop=I+A+B+H+M+W)%>%
  rename(IPop=I,APop=A,BPop=B,HPop=H,MPop=M,WPop=W)

districtracepoprate<-Enrolltable %>%
  select(RaceEthnicity,StudentCount,GradeBand)%>%
  group_by(RaceEthnicity,GradeBand) %>%
  summarise(RaceStudentCount=sum(StudentCount))
  

districtracepopoverallrate<-Enrolltable %>%
  select(RaceEthnicity,StudentCount)%>%
  mutate(GradeBand="All")%>%
  group_by(RaceEthnicity,GradeBand) %>%
  summarise(RaceStudentCount=sum(StudentCount))
 
  
districtracepoprate<-rbind(districtracepoprate,districtracepopoverallrate)


districtIEPpop<-Enrolltable %>%
  select(IEP,RaceEthnicity,StudentCount,GradeBand)%>%
  group_by(IEP) %>%
  summarise(IEPStudentCount=sum(StudentCount))%>%
  mutate(District="District")%>%
  melt(id.vars=c("IEP","District"))%>%
  dcast(District~IEP)%>%
  mutate(TotalPop=`1`+ `0`)%>%
  rename(GenEdPop=`0`,IEPPop=`1`)


districtIEPpoprate<-Enrolltable %>%
  select(SchoolName,IEP,RaceEthnicity,GradeBand,StudentCount)%>%
  group_by(IEP,GradeBand) %>%
  summarise(IEPStudentCount=sum(StudentCount))%>%
  select(GradeBand,IEP,IEPStudentCount)
  
districtIEPpopOverallrate<-Enrolltable %>%
  select(SchoolName,IEP,RaceEthnicity,StudentCount)%>%
  mutate(GradeBand="All")%>%
  group_by(IEP,GradeBand) %>%
  summarise(IEPStudentCount=sum(StudentCount))%>%
  select(GradeBand,IEP,IEPStudentCount)

districtIEPpoprate<-rbind(districtIEPpoprate,districtIEPpopOverallrate)

##############################################################################################################################
#Cleaning Up EHOUT for Usage

#only columns from ehout that are necessary
ehout2017$GradeBand<-revalue(ehout2017$Grade,c("KF"="PK-2","PKH"="PK-2","PKF"="PK-2","1"="PK-2","2"="PK-2","U2"="PK-2",
                                                 "3"="3-5","4"="3-5","5"="3-5","U4"="3-5","U3"="3-5",
                                                 "6"="6-8","7"="6-8","8"="6-8","U6"="6-8","U7"="6-8","U8"="6-8",
                                                 "9"="9-12","10"="9-12","11"="9-12","12"="9-12","T+"="9-12","US"="9-12"))

ehout2017$RaceEthnicity<-revalue(ehout2017$RaceEthnicity,c("P"="A"))
ehout2017$SchoolName<-revalue(ehout2017$SchoolName, c(
  "Twilight Academy @ Fowler"="Fowler High School",
  "Twilight Academy @ Corcoran"="Corcoran High School",
  "Twilight Academy @ Nottingham" = "Nottingham High School",
  "Twilight Academy @ PSLA" = "Public Service Leadership Academy at Fowler",
  "Twilight Academy @ Henninger" = "Henninger High School",
  "Syracuse Latin School " = "Syracuse Latin School"))


########SCHOOL-BASED INFORMATION#####################################################################
IndDiscRace<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,SchoolName,OffenderID,RaceEthnicity)%>%
  group_by(SchoolName,OffenderID,RaceEthnicity) %>%
  summarise(OSS=sum(OSS),ISS=sum(ISS),Ref=sum(Ref)) %>%
  replace(.,is.na(.),0) %>%
  mutate(OneOSS=ifelse(OSS>0,1,0))%>%
  mutate(MultOSS=ifelse(OSS>1,1,0))%>%
  mutate(OneISS=ifelse(ISS>0,1,0))%>%
  mutate(MultISS=ifelse(ISS>1,1,0))%>%
  group_by(SchoolName,RaceEthnicity)%>%
  summarise(RaceOneOSS=sum(OneOSS),RaceMultOSS=sum(MultOSS),RaceOneISS=sum(OneISS),RaceMultISS=sum(MultISS))
  

IndDiscIEP<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,SchoolName,OffenderID,IEP)%>%
  group_by(SchoolName,OffenderID,IEP) %>%
  summarise(OSS=sum(OSS),ISS=sum(ISS),Ref=sum(Ref)) %>%
  mutate(OneOSS=ifelse(OSS>0,1,0))%>%
  mutate(MultOSS=ifelse(OSS>1,1,0))%>%
  mutate(OneISS=ifelse(ISS>0,1,0))%>%
  mutate(MultISS=ifelse(ISS>1,1,0))%>%
  group_by(SchoolName,IEP)%>%
  summarise(IEPOneOSS=sum(OneOSS),IEPMultOSS=sum(MultOSS),IEPOneISS=sum(OneISS),IEPMultISS=sum(MultISS))
  replace(.,is.na(.),0) %>%
  
IndDispropDiscRace<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,SchoolName,OffenderID,RaceEthnicity)%>%
  group_by(SchoolName,OffenderID,RaceEthnicity) %>%
  summarise(OSS=sum(OSS),ISS=sum(ISS),Ref=sum(Ref)) %>%
  mutate(OneOSS=ifelse(OSS>0,1,0))%>%
  mutate(MultOSS=ifelse(OSS>1,1,0))%>%
  mutate(OneISS=ifelse(ISS>0,1,0))%>%
  mutate(MultISS=ifelse(ISS>1,1,0))%>%
  group_by(SchoolName,RaceEthnicity)%>%
  summarise(OneOSS=sum(OneOSS),MultOSS=sum(MultOSS),OneISS=sum(OneISS),MultISS=sum(MultISS))%>%
  melt(id.vars=c("SchoolName","RaceEthnicity"))%>%
  dcast(SchoolName+variable~RaceEthnicity)%>%
  replace(.,is.na(.),0) %>%
  mutate(TotalDisc=A+B+H+W+I+M)%>%
  rename(ADisc=A,BDisc=B,HDisc=H,WDisc=W,IDisc=I,MDisc=M)

  
  
IndDispropDiscIEP<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,SchoolName,OffenderID,IEP)%>%
  group_by(SchoolName,OffenderID,IEP) %>%
  summarise(OSS=sum(OSS),ISS=sum(ISS),Ref=sum(Ref)) %>%
  mutate(OneOSS=ifelse(OSS>0,1,0))%>%
  mutate(MultOSS=ifelse(OSS>1,1,0))%>%
  mutate(OneISS=ifelse(ISS>0,1,0))%>%
  mutate(MultISS=ifelse(ISS>1,1,0))%>%
  group_by(SchoolName,IEP)%>%
  summarise(OneOSS=sum(OneOSS),MultOSS=sum(MultOSS),OneISS=sum(OneISS),MultISS=sum(MultISS))%>%
  melt(id.vars=c("SchoolName","IEP"))%>%
  dcast(SchoolName+variable~IEP)%>%
  replace(.,is.na(.),0) %>%
  mutate(TotalDisc=`1`+`0`)%>%
  rename(IEPDisc=`1`,GenEdDisc=`0`)

SchoolDiscRace<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,SchoolName,IEP,RaceEthnicity)%>%
  group_by(SchoolName,RaceEthnicity) %>%
  summarise(RaceOSSIncidents=sum(OSS),RaceISSIncidents=sum(ISS),RaceRefIncidents=sum(Ref))%>%
  replace(.,is.na(.),0)
  

SchoolDiscIEP<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,SchoolName,IEP,RaceEthnicity)%>%
  group_by(SchoolName,IEP) %>%
  summarise(IEPOSSIncidents=sum(OSS),IEPISSIncidents=sum(ISS),IEPRefIncidents=sum(Ref))%>%
  replace(.,is.na(.),0)

SchoolDispropDiscRace<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,SchoolName,IEP,RaceEthnicity)%>%
  group_by(SchoolName,RaceEthnicity) %>%
  summarise(OSSIncidents=sum(OSS),ISSIncidents=sum(ISS),RefIncidents=sum(Ref))%>%
  melt(id.vars=c("SchoolName","RaceEthnicity"))%>%
  dcast(SchoolName+variable~RaceEthnicity)%>%
  replace(.,is.na(.),0) %>%
  rename(ADisc=A,BDisc=B,HDisc=H,WDisc=W,IDisc=I,MDisc=M)%>%
  mutate(TotalDisc=ADisc+BDisc+HDisc+WDisc+IDisc+MDisc)
  
SchoolDispropDiscIEP<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,SchoolName,IEP,RaceEthnicity)%>%
  group_by(SchoolName,IEP) %>%
  summarise(OSSIncidents=sum(OSS),ISSIncidents=sum(ISS),RefIncidents=sum(Ref))%>%
  melt(id.vars=c("SchoolName","IEP"))%>%
  dcast(SchoolName+variable~IEP)%>%
  replace(.,is.na(.),0) %>%
  mutate(TotalDisc=`1`+`0`)%>%
  rename(IEPDisc=`1`,GenEdDisc=`0`)

SchoolRateIEP<-inner_join(SchoolDiscIEP,schoolIEPpop,by=c("SchoolName","IEP")) %>%
  inner_join(., IndDiscIEP,by=c("SchoolName","IEP"))
SchoolRateRace<-inner_join(SchoolDiscRace,schoolracepop,by=c("SchoolName","RaceEthnicity")) %>%
  inner_join(., IndDiscRace,by=c("SchoolName","RaceEthnicity"))

SchoolDispropIEP<-rbind(SchoolDispropDiscIEP,IndDispropDiscIEP)
SchoolDispropIEP<-merge(SchoolDispropIEP,schooldispropIEPpop,by="SchoolName")

SchoolDispropRace<-rbind(SchoolDispropDiscRace,IndDispropDiscRace)
SchoolDispropRace<-merge(SchoolDispropRace,schooldispropracepop,by="SchoolName")

SchoolOverallRates<-SchoolRateIEP%>%
  group_by(SchoolName)%>%
  summarise(OSSIncidents=sum(IEPOSSIncidents),ISSIncidents=sum(IEPISSIncidents),Referrals=sum(IEPRefIncidents),
            Population=sum(IEPStudentCount),OneOSS=sum(IEPOneOSS),OneISS=sum(IEPOneISS))%>%
  mutate(OSSIncidentRate=OSSIncidents/Population)%>%
  mutate(ISSIncidentRate=ISSIncidents/Population)%>%
  mutate(OSSUniqueRate=OneOSS/Population)%>%
  mutate(ISSUniqueRate=OneISS/Population)%>%
  mutate(ReferralRate=Referrals/Population)%>%
  select(SchoolName,OSSIncidentRate,ISSIncidentRate,OSSUniqueRate,ISSUniqueRate,ReferralRate)

SchoolRateRace<-SchoolRateRace%>%
  mutate(RaceOSSIncRate=RaceOSSIncidents/RaceStudentCount)%>%
  mutate(RaceOSSUniqueRate=RaceOneOSS/RaceStudentCount)%>%
  mutate(RaceISSIncRate=RaceISSIncidents/RaceStudentCount)%>%
  mutate(RaceISSUniqueRate=RaceOneISS/RaceStudentCount)%>%
  mutate(RaceRefRate=RaceRefIncidents/RaceStudentCount)

SchoolIEPRate<-SchoolRateIEP%>%
  mutate(IEPOSSIncRate=IEPOSSIncidents/IEPStudentCount)%>%
  mutate(IEPOSSUniqueRate=IEPOneOSS/IEPStudentCount)%>%
  mutate(IEPISSIncRate=IEPISSIncidents/IEPStudentCount)%>%
  mutate(IEPISSUniqueRate=IEPOneISS/IEPStudentCount)%>%
  mutate(IEPRefRate=IEPRefIncidents/IEPStudentCount)
 

SchoolDispropRace[is.na(SchoolDispropRace)]<-0
SchoolIEPRate[is.na(SchoolIEPRate)]<-0
SchoolRaceRate[is.na(SchoolRaceRate)]<-0
SchoolDispropIEP[is.na(SchoolDispropIEP)]<-0

SchoolDispropRaceFinal<-SchoolDispropRace%>%
  mutate(BDisprop=(BDisc/TotalDisc)-(BPop/TotalPop))%>%
  mutate(ADisprop=(ADisc/TotalDisc)-(APop/TotalPop))%>%
  mutate(HDisprop=(HDisc/TotalDisc)-(HPop/TotalPop))%>%
  mutate(IDisprop=(IDisc/TotalDisc)-(IPop/TotalPop))%>%
  mutate(MDisprop=(MDisc/TotalDisc)-(MPop/TotalPop))%>%
  mutate(WDisprop=(WDisc/TotalDisc)-(WPop/TotalPop))%>%
  mutate(BRiskRatio=(BDisc/BPop)/((TotalDisc-BDisc)/(TotalPop-BPop)))%>%
  mutate(ARiskRatio=(ADisc/BPop)/((TotalDisc-ADisc)/(TotalPop-APop)))%>%
  mutate(HRiskRatio=(HDisc/HPop)/((TotalDisc-HDisc)/(TotalPop-HPop)))%>%
  mutate(IRiskRatio=(IDisc/IPop)/((TotalDisc-IDisc)/(TotalPop-IPop)))%>%
  mutate(MRiskRatio=(MDisc/MPop)/((TotalDisc-MDisc)/(TotalPop-MPop)))%>%
  mutate(WRiskRatio=(WDisc/WPop)/((TotalDisc-WDisc)/(TotalPop-WPop)))%>%
  select(SchoolName,variable,ADisprop,BDisprop,HDisprop,IDisprop,MDisprop,WDisprop,ARiskRatio,BRiskRatio,
         HRiskRatio,IRiskRatio,MRiskRatio,WRiskRatio)

SchoolDispropIEPFinal<-SchoolDispropIEP%>%
  mutate(IEPDisprop=(IEPDisc/TotalDisc)-(IEPPop/TotalPop))%>%
  mutate(GenEdDisprop=(GenEdDisc/TotalDisc)-(GenEdPop/TotalPop))%>%
  mutate(IEPRiskRatio=(IEPDisc/IEPPop)/((TotalDisc-IEPDisc)/(TotalPop-IEPPop)))%>%
  mutate(GenEdRiskRatio=(GenEdDisc/GenEdPop)/((TotalDisc-GenEdDisc)/(TotalPop-GenEdPop)))%>%
  select(SchoolName,variable,GenEdDisprop,IEPDisprop,GenEdRiskRatio,IEPRiskRatio)


SchoolRateRaceFinal<-SchoolRateRace %>%
  select(SchoolName,RaceEthnicity,RaceOSSUniqueRate,RaceOSSIncRate,RaceISSIncRate,RaceISSUniqueRate,RaceRefRate)%>%
  melt(id.vars=c("SchoolName","RaceEthnicity"))%>%
  dcast(SchoolName+variable~RaceEthnicity)

SchoolRateIEPFinal<-SchoolIEPRate%>%
  select(SchoolName,IEP,IEPOSSUniqueRate,IEPOSSIncRate,IEPISSIncRate,IEPISSUniqueRate,IEPRefRate)%>%
  melt(id.vars=c("SchoolName","IEP"))%>%
  dcast(SchoolName+variable~IEP)




write.csv(SchoolDispropIEPFinal, "C:/Users/drobil66/Desktop/RFiles/R Reports/SchoolDispropIEPFinal.csv")
write.csv(SchoolDispropRaceFinal, "C:/Users/drobil66/Desktop/RFiles/R Reports/SchoolDispropRaceFinal.csv")
write.csv(SchoolRateRaceFinal,"C:/Users/drobil66/Desktop/RFiles/R Reports/SchoolRateRaceFinal.csv")
write.csv(SchoolRateIEPFinal,"C:/Users/drobil66/Desktop/RFiles/R Reports/SchoolRateIEPFinal.csv")
write.csv(SchoolOverallRates,"C:/Users/drobil66/Desktop/RFiles/R Reports/SchoolOverallRates.csv")




############################################################################################################
############################################################################################################
###DISTRICT INFORMATION###############
######RATES######################
DistrictIndDiscRace<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,OffenderID,RaceEthnicity,GradeBand)%>%
  group_by(GradeBand,OffenderID,RaceEthnicity) %>%
  summarise(OSS=sum(OSS),ISS=sum(ISS),Ref=sum(Ref)) %>%
  mutate(OneOSS=ifelse(OSS>0,1,0))%>%
  mutate(MultOSS=ifelse(OSS>1,1,0))%>%
  mutate(OneISS=ifelse(ISS>0,1,0))%>%
  mutate(MultISS=ifelse(ISS>1,1,0))%>%
  group_by(GradeBand,RaceEthnicity)%>%
  summarise(RaceOneOSS=sum(OneOSS),RaceMultOSS=sum(MultOSS),RaceOneISS=sum(OneISS),RaceMultISS=sum(MultISS))

DistrictIndDiscRaceOverall<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  mutate(GradeBand="All")%>%
  select(OSS,ISS,Ref,OffenderID,RaceEthnicity,GradeBand)%>%
  group_by(OffenderID,RaceEthnicity,GradeBand) %>%
  summarise(OSS=sum(OSS),ISS=sum(ISS),Ref=sum(Ref)) %>%
  mutate(OneOSS=ifelse(OSS>0,1,0))%>%
  mutate(MultOSS=ifelse(OSS>1,1,0))%>%
  mutate(OneISS=ifelse(ISS>0,1,0))%>%
  mutate(MultISS=ifelse(ISS>1,1,0))%>%
  group_by(RaceEthnicity,GradeBand)%>%
  summarise(RaceOneOSS=sum(OneOSS),RaceMultOSS=sum(MultOSS),RaceOneISS=sum(OneISS),RaceMultISS=sum(MultISS))%>%
  select(GradeBand,RaceEthnicity,RaceOneOSS,RaceMultOSS,RaceOneISS,RaceMultISS)

DistrictIndDiscRace<-rbind(DistrictIndDiscRace,DistrictIndDiscRaceOverall)

DistrictIndDiscIEP<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,OffenderID,IEP,GradeBand)%>%
  group_by(GradeBand,OffenderID,IEP) %>%
  summarise(IEPOSS=sum(OSS),IEPISS=sum(ISS),IEPRef=sum(Ref)) %>%
  mutate(IEPOneOSS=ifelse(IEPOSS>0,1,0))%>%
  mutate(IEPMultOSS=ifelse(IEPOSS>1,1,0))%>%
  mutate(IEPOneISS=ifelse(IEPISS>0,1,0))%>%
  mutate(IEPMultISS=ifelse(IEPISS>1,1,0))%>%
  group_by(GradeBand,IEP)%>%
  summarise(IEPOneOSS=sum(IEPOneOSS),IEPMultOSS=sum(IEPMultOSS),IEPOneISS=sum(IEPOneISS),IEPMultISS=sum(IEPMultISS))

DistrictIndDiscIEPOverall<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  mutate(GradeBand="All") %>%
  select(OSS,ISS,Ref,OffenderID,IEP,GradeBand)%>%
  group_by(OffenderID,IEP,GradeBand) %>%
  summarise(IEPOSS=sum(OSS),IEPISS=sum(ISS),IEPRef=sum(Ref)) %>%
  mutate(IEPOneOSS=ifelse(IEPOSS>0,1,0))%>%
  mutate(IEPMultOSS=ifelse(IEPOSS>1,1,0))%>%
  mutate(IEPOneISS=ifelse(IEPISS>0,1,0))%>%
  mutate(IEPMultISS=ifelse(IEPISS>1,1,0))%>%
  group_by(IEP,GradeBand)%>%
  summarise(IEPOneOSS=sum(IEPOneOSS),IEPMultOSS=sum(IEPMultOSS),IEPOneISS=sum(IEPOneISS),IEPMultISS=sum(IEPMultISS))

DistrictIndDiscIEP<-rbind(DistrictIndDiscIEP,DistrictIndDiscIEPOverall)


DistrictDiscRace<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,IEP,GradeBand,RaceEthnicity)%>%
  group_by(GradeBand,RaceEthnicity) %>%
  summarise(RaceOSSIncidents=sum(OSS),RaceISSIncidents=sum(ISS),RaceRefIncidents=sum(Ref))

DistrictDiscRaceOverall<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,IEP,RaceEthnicity)%>%
  mutate(GradeBand="All")%>%
  group_by(RaceEthnicity,GradeBand) %>%
  summarise(RaceOSSIncidents=sum(OSS),RaceISSIncidents=sum(ISS),RaceRefIncidents=sum(Ref))

DistrictDiscRace<-rbind(DistrictDiscRaceOverall,DistrictDiscRace)

DistrictDiscIEP<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,IEP,GradeBand)%>%
  group_by(GradeBand,IEP) %>%
  summarise(IEPOSSIncidents=sum(OSS),IEPISSIncidents=sum(ISS),IEPRefIncidents=sum(Ref))

DistrictDiscIEPOverall<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,IEP)%>%
  mutate(GradeBand="All")%>%
  group_by(IEP,GradeBand) %>%
  summarise(IEPOSSIncidents=sum(OSS),IEPISSIncidents=sum(ISS),IEPRefIncidents=sum(Ref))
  
DistrictDiscIEP<-rbind(DistrictDiscIEP,DistrictDiscIEPOverall)

DistrictIEP<-inner_join(DistrictDiscIEP,districtIEPpoprate,by=c("IEP","GradeBand")) %>%
  inner_join(., DistrictIndDiscIEP,by=c("GradeBand","IEP"))
 

DistrictRace<-inner_join(DistrictDiscRace,districtracepoprate,by=c("RaceEthnicity","GradeBand")) %>%
  inner_join(., DistrictIndDiscRace,by=c("RaceEthnicity","GradeBand"))

FDistRateOverall<-DistrictRace%>%
  group_by(GradeBand)%>%
  summarise(OSSIncidents=sum(RaceOSSIncidents),ISSIncidents=sum(RaceISSIncidents),Referrals=sum(RaceRefIncidents),Population=sum(RaceStudentCount),
            UniqueOSS=sum(RaceOneOSS),UniqueISS=sum(RaceOneISS))%>%
  mutate(OSSIncidentRate=OSSIncidents/Population)%>%
  mutate(ISSIncidentRate=ISSIncidents/Population)%>%
  mutate(OSSUniqueRate=UniqueOSS/Population)%>%
  mutate(ISSUniqueRate=UniqueISS/Population)%>%
  mutate(ReferralRate=Referrals/Population)%>%
  select(GradeBand,OSSIncidentRate,OSSUniqueRate,ISSIncidentRate,ISSUniqueRate,ReferralRate)

DistrictRaceRate<-DistrictRace%>%
  mutate(RaceOSSIncRate=RaceOSSIncidents/RaceStudentCount)%>%
  mutate(RaceOSSUniqueRate=RaceOneOSS/RaceStudentCount)%>%
  mutate(RaceISSIncRate=RaceISSIncidents/RaceStudentCount)%>%
  mutate(RaceISSUniqueRate=RaceOneISS/RaceStudentCount)%>%
  mutate(RaceRefRate=RaceRefIncidents/RaceStudentCount)

DistrictIEPRate<-DistrictIEP%>%
  mutate(IEPOSSIncRate=IEPOSSIncidents/IEPStudentCount)%>%
  mutate(IEPOSSUniqueRate=IEPOneOSS/IEPStudentCount)%>%
  mutate(IEPISSIncRate=IEPISSIncidents/IEPStudentCount)%>%
  mutate(IEPISSUniqueRate=IEPOneISS/IEPStudentCount)%>%
  mutate(IEPRefRate=IEPRefIncidents/IEPStudentCount)

FDistRateRace<-DistrictRaceRate %>%
  select(GradeBand,RaceEthnicity,RaceOSSIncRate,RaceOSSUniqueRate,RaceISSIncRate,RaceISSUniqueRate,RaceRefRate)%>%
  melt(id.vars=c("GradeBand","RaceEthnicity"))%>%
  dcast(GradeBand+variable~RaceEthnicity)
  
FDistRateIEP<-DistrictIEPRate %>%
  select(GradeBand,IEP,IEPOSSIncRate,IEPOSSUniqueRate,IEPISSIncRate,IEPISSUniqueRate,IEPRefRate)%>%
  melt(id.vars=c("GradeBand","IEP"))%>%
  dcast(GradeBand+variable~IEP)

##############DISPROP###############################
DistrictDispropDiscRace<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,OffenderID,RaceEthnicity)%>%
  group_by(OffenderID,RaceEthnicity) %>%
  summarise(RaceOSS=sum(OSS),RaceISS=sum(ISS),RaceRef=sum(Ref)) %>%
  mutate(RaceOneOSS=ifelse(RaceOSS>0,1,0))%>%
  mutate(RaceMultOSS=ifelse(RaceOSS>1,1,0))%>%
  mutate(RaceOneISS=ifelse(RaceISS>0,1,0))%>%
  mutate(RaceMultISS=ifelse(RaceISS>1,1,0))%>%
  group_by(RaceEthnicity)%>%
  summarise(RaceOneOSS=sum(RaceOneOSS),RaceMultOSS=sum(RaceMultOSS),RaceOneISS=sum(RaceOneISS),RaceMultISS=sum(RaceMultISS),AllOSS=sum(RaceOSS),AllISS=sum(RaceISS))%>%
  melt(id.vars=c("RaceEthnicity"))%>%
  dcast(variable~RaceEthnicity)%>%
  mutate(TotalDisc=A+B+H+W+I+M)%>%
  rename(ADisc=A,BDisc=B,HDisc=H,WDisc=W,IDisc=I,MDisc=M)%>%
  mutate(District="District")

DistrictDispropRace<-merge(DistrictDispropDiscRace,districtracepop,by="District")
FDistDispRace<-DistrictDispropRace%>%
  mutate(BDisprop=(BDisc/TotalDisc)-(BPop/TotalPop))%>%
  mutate(ADisprop=(ADisc/TotalDisc)-(APop/TotalPop))%>%
  mutate(HDisprop=(HDisc/TotalDisc)-(HPop/TotalPop))%>%
  mutate(IDisprop=(IDisc/TotalDisc)-(IPop/TotalPop))%>%
  mutate(MDisprop=(MDisc/TotalDisc)-(MPop/TotalPop))%>%
  mutate(WDisprop=(WDisc/TotalDisc)-(WPop/TotalPop))%>%
  mutate(BRiskRatio=(BDisc/BPop)/((TotalDisc-BDisc)/(TotalPop-BPop)))%>%
  mutate(ARiskRatio=(ADisc/BPop)/((TotalDisc-ADisc)/(TotalPop-APop)))%>%
  mutate(HRiskRatio=(HDisc/HPop)/((TotalDisc-HDisc)/(TotalPop-HPop)))%>%
  mutate(IRiskRatio=(IDisc/IPop)/((TotalDisc-IDisc)/(TotalPop-IPop)))%>%
  mutate(MRiskRatio=(MDisc/MPop)/((TotalDisc-MDisc)/(TotalPop-MPop)))%>%
  mutate(WRiskRatio=(WDisc/WPop)/((TotalDisc-WDisc)/(TotalPop-WPop)))%>%
  select(District,variable,BDisprop,ADisprop,HDisprop,IDisprop,MDisprop,WDisprop,BRiskRatio,
         ARiskRatio,HRiskRatio,IRiskRatio,MRiskRatio,WRiskRatio)

DistrictDispropDiscIEP<-ehout2017 %>%
  filter(IncidentType=="referral") %>%
  mutate(Ref=1)%>%
  select(OSS,ISS,Ref,GradeBand,OffenderID,IEP)%>%
  group_by(OffenderID,IEP) %>%
  summarise(IEPOSS=sum(OSS),IEPISS=sum(ISS),IEPRef=sum(Ref)) %>%
  mutate(IEPOneOSS=ifelse(IEPOSS>0,1,0))%>%
  mutate(IEPMultOSS=ifelse(IEPOSS>1,1,0))%>%
  mutate(IEPOneISS=ifelse(IEPISS>0,1,0))%>%
  mutate(IEPMultISS=ifelse(IEPISS>1,1,0))%>%
  group_by(IEP)%>%
  summarise(IEPOneOSS=sum(IEPOneOSS),IEPMultOSS=sum(IEPMultOSS),IEPOneISS=sum(IEPOneISS),IEPMultISS=sum(IEPMultISS),AllOSS=sum(IEPOSS),AllISS=sum(IEPISS))%>%
  melt(id.vars=c("IEP"))%>%
  dcast(variable~IEP)%>%
  replace(.,is.na(.),0) %>%
  mutate(TotalDisc=`1`+`0`)%>%
  rename(IEPDisc=`1`,GenEdDisc=`0`)%>%
  mutate(District="District")

  DistrictDispropIEP<-merge(DistrictDispropDiscIEP,districtIEPpop,by="District")
  
  FDistDispIEP<-DistrictDispropIEP %>%
    mutate(IEPDisprop=(IEPDisc/TotalDisc)-(IEPPop/TotalPop))%>%
    mutate(GenEdDisprop=(GenEdDisc/TotalDisc)-(GenEdPop/TotalPop))%>%
    mutate(IEPRiskRatio=(IEPDisc/IEPPop)/((TotalDisc-IEPDisc)/(TotalPop-IEPPop)))%>%
    mutate(GenEdRiskRatio=(GenEdDisc/GenEdPop)/((TotalDisc-GenEdDisc)/(TotalPop-GenEdPop)))%>%
    select(District,variable,IEPDisprop,GenEdDisprop,IEPRiskRatio,GenEdRiskRatio)
  
write.csv(FDistDispIEP, "C:/Users/drobil66/Desktop/RFiles/R Reports/FDistDispIEP.csv")
write.csv(FDistDispRace, "C:/Users/drobil66/Desktop/RFiles/R Reports/FDistDispRace.csv")
write.csv(FDistRateIEP, "C:/Users/drobil66/Desktop/RFiles/R Reports/FDistRateIEP.csv")
write.csv(FDistRateRace, "C:/Users/drobil66/Desktop/RFiles/R Reports/FDistRateRace.csv")
write.csv(FDistRateOverall, "C:/Users/drobil66/Desktop/RFiles/R Reports/FDistRateOverall.csv")



