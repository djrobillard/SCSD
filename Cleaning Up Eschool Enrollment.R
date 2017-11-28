library(plyr)
library(dplyr)
library(scales)
library(xlsx)
library(lubridate)
library(eeptools)


#
enrollment<-read.csv("eschool2017.csv",strip.white = TRUE)

#only select columns
cleanenrollment<-select(enrollment,Year,Student.Id,Gender,Building.Name...E.W,ESL,Has.IEP,Ethnicity)

cleanenrollment<-rename(cleanenrollment,Building=Building.Name...E.W)

cleanenrollment<-filter(cleanenrollment,Building %in% c("Lincoln Middle School", "Institute of Technology at Syracuse Central",
                     "Grant Middle School", "Seymour Dual Language Academy", "Nottingham High School",
                     "Delaware Primary School", "Delaware Academy","Corcoran High School", "Westside Academy at Blodgett",
                     "Fowler High School","Frazer Middle School","Frazer Elementary School","Huntington Elementary School","Huntington Middle School",
                     "Public Service Leadership Academy at Fowler", "Hughes Elementary School",
                     "Dr. Weeks Elementary School", "Dr. King Elementary School",       
                     "Franklin Elementary School", "Hurlbut W. Smith Elementary School","Hurlbut W. Smith Middle School", "Webster Elementary School",
                     "Danforth Middle School", "Roberts Elementary School","Roberts Middle School", "Henninger High School",
                     "Salem Hyde Elementary School", "Lemoyne Elementary School", "Porter Elementary School",
                     "Edward Smith Elementary School","Edward Smith Middle", "JVC EPIC Program", "Syracuse Latin School", "Bellevue Elementary School",
                     "Clary Middle School", "P-Tech", "Van Duyn Elementary School", "MSAP/CORE", "McCarthy at Beard School",
                     "Meachem Elementary School", "McKinley - Brighton Elementary School", "Elmcrest Children's Center School",
                     "Expeditionary Learning Middle School","Twilight Academy @ Corcoran",
                     "Twilight Academy @ Fowler", "Twilight Academy @ Henninger", 
                     "Twilight Academy @ Nottingham",
                     "Twilight Academy @ PSLA", "Johnson Center Transition Program","Johnson Center Alternative","JVC EPIC Program","GPS Elementary Program","Montessori School @ Lemoyne","P-Tech"))


cleanenrollment$Building<-revalue(cleanenrollment$Building, c(
  "MSAP/CORE" = "CORE",
  "Edward Smith Elementary School" = "Edward Smith K-8",
  "Edward Smith Middle" = "Edward Smith K-8",
  "GPS Elementary Program" = "CORE",
  "Frazer Elementary School"="Frazer PK-8 School",
  "Frazer Middle School"="Frazer PK-8 School",
  "Huntington Elementary School"="Huntington PK-8",
  "Huntington Middle School"="Huntington PK-8",
  "McKinley - Brighton Elementary School" = "McKinley-Brighton Elementary School",
  "Roberts Elementary School" = "Roberts PK-8",
  "Roberts Middle School" = "Roberts PK-8",
  "Hurlbut W. Smith Elementary School" = "H.W. Smith PK-8",
  "Hurlbut W. Smith Middle School" = "H.W. Smith PK-8",
  "P-Tech" = "Institute of Technology at Syracuse Central",
  "JVC EPIC Program" = "Johnson Center Epic Program",
  "Johnson Center Alternative" = "Johnson Center Epic Program"
  ))

cleanenrollment<-select(cleanenrollment,Year,Building,Student.Id,Gender,Ethnicity,Has.IEP,ESL)
cleanenrollment$count<-1
#enrollment for pivot comparison

aggenroll<-aggregate(count~Year+Building+Gender+Ethnicity+Has.IEP+ESL,data=cleanenrollment,FUN=sum)
#changing column names
aggenroll<-rename(aggenroll,SchoolYear=Year)
aggenroll<-rename(aggenroll,SchoolName=Building)
aggenroll<-rename(aggenroll,RaceEthnicity=Ethnicity)
aggenroll<-rename(aggenroll,IEP=Has.IEP)
aggenroll<-rename(aggenroll,ELL=ESL)

#cleaning up values


aggenroll$RaceEthnicity<-revalue(aggenroll$RaceEthnicity,c(
  "A"="API",
  "P"="API",
  "w"="W",
  "b"="B"
))


aggenroll$IEP<-revalue(aggenroll$IEP, c(
  N = 0,
  Y = 1
  ))

aggenroll$ELL<-revalue(aggenroll$ELL, c(
  N = 0,
  Y = 1
))
aggenroll<-rename(aggenroll,StudentCount=count)
#select
aggenroll<-select(aggenroll,SchoolYear,SchoolName,Gender,RaceEthnicity,IEP,ELL,StudentCount)
AllDiscipline<-select(AllDiscipline,SchoolYear,SchoolName,Gender,RaceEthnicity,IEP,ELL,Classroom,RepeatOSS,OneOSS,RepeatISS,OneISS,RepeatRef,OneRef,OSS,OSS.Days,ISS,ISS.Days,OneSusp,RepeatSusp,Total.Suspension,Total.Suspension.Duration,inccount)
#mergewithdiscipline
everything<-merge(AllDiscipline,aggenroll,by=c("SchoolYear","SchoolName","Gender","RaceEthnicity","IEP","ELL"),all=TRUE)
everything[is.na(everything)]<-0


write.csv(everything, "C:/Users/drobil66/Desktop/RFiles/R Reports/ClassroomEverything.csv") 