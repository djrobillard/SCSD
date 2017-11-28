####grades by quarter 2016, 2017

Credits1<-read.csv("StudentCredits.csv",strip.white = TRUE)
Credits1<-rename(Credits1,SCHOOL_YEAR=ï..SCHOOL_YEAR)
Credits<-select(Credits1,SCHOOL_YEAR,SUMMER_SCHOOL,DateEnteredGrade9,CourseSchoolName,CourseName_CourseCatalog,SubjArea_CourseCatalog,ATT_CREDIT,EARN_CREDIT,STUDENT_ID,Mark1_Value,Mark2_Value,Mark3_Value,Mark4_Value,
                Mark1_Passing,Mark2_Passing,Mark3_Passing,Mark4_Passing)
Credits<-filter(Credits,SCHOOL_YEAR==2014|SCHOOL_YEAR==2015|SCHOOL_YEAR==2016|SCHOOL_YEAR==2017)
Credits<-filter(Credits,SUMMER_SCHOOL=="N")
Credits<-filter(Credits,CourseSchoolName=="Nottingham High School"|CourseSchoolName=="Henninger High School"|
                  CourseSchoolName=="Institute of Technology at Syracuse Central"|
                  CourseSchoolName=="Roberts K-8 School"|CourseSchoolName=="Edward Smith K-8 School"|
                  CourseSchoolName=="Lincoln Middle School"|                  CourseSchoolName=="P-Tech")




MP1<-aggregate(M1Pass ~ CourseSchoolName +SubjArea_CourseCatalog+SCHOOL_YEAR+Ethnicity+IEP+ELL,  data=Credits2, FUN=sum)
MP2<-aggregate(M2Pass ~ CourseSchoolName +SubjArea_CourseCatalog+SCHOOL_YEAR+Ethnicity+IEP+ELL,  data=Credits2, FUN=sum)
MP3<-aggregate(M3Pass ~ CourseSchoolName +SubjArea_CourseCatalog+SCHOOL_YEAR+Ethnicity+IEP+ELL,  data=Credits2, FUN=sum)
MP4<-aggregate(M4Pass ~ CourseSchoolName +SubjArea_CourseCatalog+SCHOOL_YEAR+Ethnicity+IEP+ELL,  data=Credits2, FUN=sum)

MP1A<-aggregate(M1Attempt ~ CourseSchoolName +SubjArea_CourseCatalog+SCHOOL_YEAR+Ethnicity+IEP+ELL,  data=Credits2, FUN=sum)
MP2A<-aggregate(M2Attempt ~ CourseSchoolName +SubjArea_CourseCatalog+SCHOOL_YEAR+Ethnicity+IEP+ELL,  data=Credits2, FUN=sum)
MP3A<-aggregate(M3Attempt ~ CourseSchoolName +SubjArea_CourseCatalog+SCHOOL_YEAR+Ethnicity+IEP+ELL,  data=Credits2, FUN=sum)
MP4A<-aggregate(M4Attempt ~ CourseSchoolName +SubjArea_CourseCatalog+SCHOOL_YEAR+Ethnicity+IEP+ELL,  data=Credits2, FUN=sum)

AllClasses<-merge(MP1,MP1A,c("CourseSchoolName","SubjArea_CourseCatalog","SCHOOL_YEAR","Ethnicity","IEP","ELL"),all=TRUE)
AllClasses<-merge(AllClasses,MP2,c("CourseSchoolName","SubjArea_CourseCatalog","SCHOOL_YEAR","Ethnicity","IEP","ELL"),all=TRUE)
AllClasses<-merge(AllClasses,MP2A,c("CourseSchoolName","SubjArea_CourseCatalog","SCHOOL_YEAR","Ethnicity","IEP","ELL"),all=TRUE)
AllClasses<-merge(AllClasses,MP3,c("CourseSchoolName","SubjArea_CourseCatalog","SCHOOL_YEAR","Ethnicity","IEP","ELL"),all=TRUE)
AllClasses<-merge(AllClasses,MP3A,c("CourseSchoolName","SubjArea_CourseCatalog","SCHOOL_YEAR","Ethnicity","IEP","ELL"),all=TRUE)
AllClasses<-merge(AllClasses,MP4,c("CourseSchoolName","SubjArea_CourseCatalog","SCHOOL_YEAR","Ethnicity","IEP","ELL"),all=TRUE)
AllClasses<-merge(AllClasses,MP4A,c("CourseSchoolName","SubjArea_CourseCatalog","SCHOOL_YEAR","Ethnicity","IEP","ELL"),all=TRUE)


AllClasses[is.na(AllClasses)]<-0

write.csv(AllClasses,"C:/Users/drobil66/Desktop/RFiles/R Reports/ESAllCourses.csv")


##########################################################################################

GradesbyMP<-GradesbyMP[!is.na(as.numeric(as.character(GradesbyMP))),]


MP1$MP1Att<-1
MP2$MP2Att<-1
MP3$MP3Att<-1
MP4$MP4Att<-1

MP1$MP1Pass<-ifelse(as.numeric(MP1$Mark1_Value)>64,1,0)
MP2$MP2Pass<-ifelse(as.numeric(MP2$Mark2_Value)>64,1,0)
MP3$MP3Pass<-ifelse(as.numeric(MP3$Mark3_Value)>64,1,0)
MP4$MP4Pass<-ifelse(as.numeric(MP4$Mark4_Value)>64,1,0)

MP1Attempts<-aggregate(MP1Att ~ CourseSchoolName +SubjArea_CourseCatalog+SCHOOL_YEAR+CohortYear,  data=MP1, FUN=sum)
MP1Earned<-aggregate(MP1Pass ~ CourseSchoolName +SubjArea_CourseCatalog+ SCHOOL_YEAR+CohortYear,  data=MP1, FUN=sum)

MP2Attempts<-aggregate(MP2Att ~ CourseSchoolName +SubjArea_CourseCatalog+SCHOOL_YEAR+CohortYear,  data=MP2, FUN=sum)
MP2Earned<-aggregate(MP2Pass ~ CourseSchoolName +SubjArea_CourseCatalog+ SCHOOL_YEAR+CohortYear,  data=MP2, FUN=sum)

MP3Attempts<-aggregate(MP3Att ~ CourseSchoolName +SubjArea_CourseCatalog+SCHOOL_YEAR+CohortYear,  data=MP3, FUN=sum)
MP3Earned<-aggregate(MP3Pass ~ CourseSchoolName +SubjArea_CourseCatalog+ SCHOOL_YEAR+CohortYear,  data=MP3, FUN=sum)

MP4Attempts<-aggregate(MP4Att ~ CourseSchoolName +SubjArea_CourseCatalog+SCHOOL_YEAR+CohortYear,  data=MP4, FUN=sum)
MP4Earned<-aggregate(MP4Pass ~ CourseSchoolName +SubjArea_CourseCatalog+ SCHOOL_YEAR+CohortYear,  data=MP4, FUN=sum)

QuarterCredits<-merge(MP1Attempts,MP1Earned,by=c("CourseSchoolName","SCHOOL_YEAR","SubjArea_CourseCatalog","CohortYear"))
QuarterCredits<-merge(QuarterCredits,MP2Earned,by=c("CourseSchoolName","SCHOOL_YEAR","SubjArea_CourseCatalog","CohortYear"))
QuarterCredits<-merge(QuarterCredits,MP3Earned,by=c("CourseSchoolName","SCHOOL_YEAR","SubjArea_CourseCatalog","CohortYear"))
QuarterCredits<-merge(QuarterCredits,MP4Earned,by=c("CourseSchoolName","SCHOOL_YEAR","SubjArea_CourseCatalog","CohortYear"))
QuarterCredits<-merge(QuarterCredits,MP2Attempts,by=c("CourseSchoolName","SCHOOL_YEAR","SubjArea_CourseCatalog","CohortYear"))
QuarterCredits<-merge(QuarterCredits,MP3Attempts,by=c("CourseSchoolName","SCHOOL_YEAR","SubjArea_CourseCatalog","CohortYear"))
QuarterCredits<-merge(QuarterCredits,MP4Attempts,by=c("CourseSchoolName","SCHOOL_YEAR","SubjArea_CourseCatalog","CohortYear"))

QuarterCredits<-QuarterCredits %>%
  mutate(MP1PassRate=MP1Pass/MP1Att)%>%
  mutate(MP2PassRate=MP2Pass/MP2Att)%>%
  mutate(MP3PassRate=MP3Pass/MP3Att)%>%
  mutate(MP4PassRate=MP4Pass/MP4Att)
