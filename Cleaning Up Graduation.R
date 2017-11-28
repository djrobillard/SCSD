library(plyr)
library(dplyr)
library(scales)
library(xlsx)
library(lubridate)
library(eeptools)


#
grad<-read.csv("GradInformation20102012.csv")

grad$Graduated<-ifelse(grad$Enrollment.Exit.Reason=="Graduated (earned a Regents or local diploma)",1,
                       ifelse(grad$Enrollment.Exit.Reason=="Graduated (earned a Regents or local dip",1,0))

#cleaning up fields

grad$Ethnicity<-revalue(grad$Ethnicity, c("Black or African American"="B", "White"="W", 
                                                      "American Indian or Alaska Native" = "I", "Hispanic or Latino" = "H",
                                                      "Multiracial" = "M", "Asian or Pacific Islander" = "API"))
grad$Gender<-revalue(grad$Gender, c("Male" = "M", "Female" = "F"))
grad$Economically.disadvantaged<-revalue(grad$Economically.disadvantaged, c("YES" = "1"))
grad$Economically.disadvantaged <- sub("^$", "0", grad$Economically.disadvantaged)
grad$ELL.eligibile<-revalue(grad$ELL.eligibile, c("YES" = "1"))
grad$ELL.eligibile <- sub("^$", "0", grad$ELL.eligibile)
grad$Disability<-revalue(grad$Disability, c("Multiple Disabilities" = "1",
                                            "Autism"="1",
                                            "Emotional Disturbance"="1",
                                            "Intellectual Disability"="1",
                                            "Learning Disability"="1",
                                            "Other Health Impairment"="1",
                                            "Deafness"="1",
                                            "Visual Impairment (includes Blindness)"="1",
                                            "Orthopedic Impairment" = "1",
                                            "Traumatic Brain Injury" = "1",
                                            "Hearing Impairment" = "1",                    
                                            "Mental Retardation"="1"))
grad$Disability <- sub("^$", "0", grad$Disability)

cleangrad<-select(grad,Cohort.Year,Student.ID,Graduated,Gender,Ethnicity,Grade,Economically.disadvantaged,ELL.eligibile,Disability,Enrollment.Exit.Reason)

cleangrad<-rename(cleangrad,STUDENT_ID=Student.ID)

###match w/most recent building

enroll<-read.csv("enrollmentallyears.csv")

enroll$date <- mdy(enroll$WITHDRAWAL_DATE) # puts into date format

enroll2<- enroll %>%
  select(STUDENT_ID, date) %>%
  group_by(STUDENT_ID) %>%
  summarise(date.num=max(date))

enroll2<-rename(enroll2,WITHDRAWAL_DATE=date.num)
enroll<-rename(enroll,WD=WITHDRAWAL_DATE)
enroll<-rename(enroll,WITHDRAWAL_DATE=date)


enrollfinal<-merge(enroll,enroll2,by=c("STUDENT_ID","WITHDRAWAL_DATE"))

enrollfinal<-select(enrollfinal,STUDENT_ID,AccountabilitySchoolName)

cleangrad<-merge(cleangrad,enrollfinal)
write.csv(cleangrad, "C:/Users/drobil66/Desktop/RFiles/R Reports/LosenGradbyCohort.csv") 