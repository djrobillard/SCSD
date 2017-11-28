library(plyr)
library(dplyr)


Credits<-read.csv("StudentCreditsbyBuilding.csv")

Credits$SchoolType<-mapvalues(Credits$CourseSchoolID, from = c(607,603,606,434,605,433,436,437,602,604,
                                                               27,48,54,406,407,426,427,608,646,670,
                                                               944,945,1,2,3,4,6,7,8,9,
                                                               11,13,15,16,17,18,22,24,29,30,
                                                               33,36,40,45,49,101,303,360,361,363,
                                                               364,401,402,403,404,405,411,414,415,417,
                                                               419,425,501,503,504,506,507,651,999), 
                                                      to=c("Twilight","Twilight","Twilight","Twilight","Twilight","Twilight","Twilight","Twilight","Twilight","Twilight",
                                                           "Alt","Alt","Alt","Alt","Alt","Alt","Alt","Alt","Alt","Alt","Alt","Alt",
                                                           "Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt",
                                                           "Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt",
                                                           "Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt",
                                                           "Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt",
                                                           "Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt","Non-Alt"))

total.earned.building<-as.data.frame(aggregate(EARN_CREDIT ~ STUDENT_ID + SchoolType,data=Credits, FUN=sum))
total.attempted.building<-as.data.frame(aggregate(ATT_CREDIT ~ STUDENT_ID + SchoolType,data=Credits, FUN=sum))

total<-merge(total.attempted.building,total.earned.building)

TotalCredits<-reshape(total,idvar = "STUDENT_ID",timevar = "SchoolType",direction = "wide")

TotalCredits[is.na(TotalCredits)]<-0
colnames(TotalCredits)[colnames(TotalCredits)=="STUDENT_ID"]<-"StudentID"
TotalCredits$TotalEarned<-TotalCredits$EARN_CREDIT.Twilight+TotalCredits$EARN_CREDIT.Alt+TotalCredits$`EARN_CREDIT.Non-Alt`
TotalCredits$TotalAttempted<-TotalCredits$`ATT_CREDIT.Non-Alt`+TotalCredits$ATT_CREDIT.Alt+TotalCredits$ATT_CREDIT.Twilight

AltEnroll<-read.csv("AltEnroll2022017.csv")

AltEnrollwCredit<-merge(TotalCredits,AltEnroll)

write.csv(AltEnrollwCredit,"C:/Users/drobil66/Desktop/RFiles/R Reports/AltEnrollwCredit.csv")
