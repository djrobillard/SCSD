Credits<-read.csv("Credits3212017.csv")
Credits2017<-Credits %>%
  filter(SCHOOL_YEAR %in% c(2017)) %>%
  filter(SUMMER_SCHOOL %in% c("N")) %>%
  mutate(Creditbearing=(ifelse(ATT_CREDIT>0,1,0)))%>%
  mutate(CourseTaken=1)%>%
  select(STUDENT_ID,CourseTaken,Creditbearing)
  
Credits2017A<-aggregate(cbind(CourseTaken,Creditbearing)~STUDENT_ID,data=Credits2017,FUN=sum)


Credits2017A<-mutate(Credits2017A,Percent=(Creditbearing/CourseTaken))
Enroll<-enroll %>%
  filter(SCHOOL_YEAR %in% c(2017))%>%
  select(STUDENT_ID,IEP,Grade)
TotalIEP<-merge(Credits2017A,Enroll,by="STUDENT_ID")
TotalIEP<-unique(TotalIEP)
write.csv(TotalIEP, "C:/Users/drobil66/Desktop/RFiles/R Reports/IEP2.csv")

Credits2017A<-summarise(Credits2017,TotalAtt=sum(CourseTaken),TotalCreditBearing=sum(Creditbearing))
Credits2017A<-mutate(Credits2017A,Percent=(TotalCreditBearing/TotalAtt))
Credits2017A<-select(Credits2017A,STUDENT_ID,TotalCreditBearing,TotalAtt,Percent)