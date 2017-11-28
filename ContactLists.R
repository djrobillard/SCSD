library(tidyverse)
Enroll<-read.csv("CurrentEnrollment.csv")
ContactList<-Enroll %>%
  filter(Student.Building=="Bellevue Elementary School")%>%
  select(Student.Name,Curr.Grade.Lvl,Guardian.Nm,Guardian.Phone,Student.Id)

write.csv(ContactList, "C:/Users/drobil66/Desktop/RFiles/R Reports/ContactList.csv")