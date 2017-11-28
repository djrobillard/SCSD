library(tidyverse)
library(lubridate)
library(readxl)

#Contents of Raw Data:
#All employees on payroll as of April 13, 2017
#All absences of those employees, along with info on their Unit, Job Code, and Location as of that date
#Each absence is a new row. There can be multiple absences on the same day for the same employee 
#Negative values for "hours_taken" represent corrections to erroniously submitted absences
#Employees with 0 absences, who were active on June 1, have missing data for absence date 
#Columns entitled april_* reflect data for that employee as of April 13, 2017

staffatt17 <- read_excel("//osa/accountabilityshare/Data_Repository/Attendance/Teacher/Absence Data 4_13_2017 v2.xlsx",
                         1, col_names=TRUE)

colnames(staffatt17)<-c("emplid", "april_union_code", "april_job_code", "april_empl_status", 
                        "april_location_code", "april_std_hours", "city_resident", "longevity_date",
                        "absence_date", "job_start_date", "absence_code", "hours_taken", "union_code",
                        "job_code", "empl_status", "location_code", "std_hours") 


staffatt17$hours_taken[is.na(staffatt17$hours_taken)] <- 0
staffatt17$union_code[is.na(staffatt17$union_code)] <- 
  staffatt17$april_union_code[is.na(staffatt17$union_code)]
staffatt17$location_code[is.na(staffatt17$location_code)] <- 
  staffatt17$april_location_code[is.na(staffatt17$location_code)]
staffatt17$job_code[is.na(staffatt17$job_code)] <- 
  staffatt17$april_job_code[is.na(staffatt17$job_code)]

staffatt17 <- staffatt17 %>%
  mutate(longevity_date=mdy(longevity_date),
         absence_date=mdy(absence_date),
         job_start_date=mdy(job_start_date),
         weekday=wday(absence_date, label=TRUE, abbr=TRUE)) %>%
  filter(as.numeric(union_code) %in% c(1,2,3,5,6,7,8,9,10,11),
         empl_status=="A", 
         wday(absence_date) %in% c(2,3,4,5,6) | is.na(absence_date)==TRUE)

by_emplid_date17 <- staffatt17 %>%
  group_by(emplid, absence_date) %>%
  mutate(tot_hours_taken=sum(hours_taken),
         std_hours=max(std_hours),
         date_days_abs=5*tot_hours_taken/std_hours) %>%
  select(emplid, april_union_code, april_job_code, april_location_code, longevity_date, absence_date,
         job_start_date, date_days_abs, std_hours, weekday)

by_emplid17 <- by_emplid_date17 %>%
  group_by(emplid) %>%
  filter(absence_date==(max(absence_date))) %>% 

  mutate(experience=if_else(!is.na(longevity_date),
                            (mdy("06/30/2017")-longevity_date)/365.25,
                            (mdy("06/30/2017")-job_start_date)/365.25),
         exp_cat=if_else(experience<3,"Less than 3 years",
                         if_else(experience>=3&experience<8,"3-7 years",
                                 if_else(experience>=8&experience<15,"8-14 years",
                                         if_else(experience>=15&experience<20,"15-19 years",
                                                 if_else(experience>=20,"20 years","")))), missing=NULL),
         total_absences=sum(date_days_abs),
         benchmark=if_else(total_absences<=3,"Exemplary",
                           if_else(total_absences>3&total_absences<=7,"Good",
                                   if_else(total_absences>7&total_absences<=8,"Satisfactory",
                                           if_else(total_absences>8,"Needs Improvement","")))),
         index=row_number()) %>%
  filter(index==1) %>%
  select(-date_days_abs -index)

staffattend<-aggregate(date_days_abs~emplid+april_union_code+april_location_code,data=by_emplid_date17,FUN = sum)

write.csv(staffattend, "C:/Users/drobil66/Desktop/RFiles/R Reports/StaffAttend.csv") 