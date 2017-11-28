ehout<-ehout_all_years
library(lubridate)

###parse dates and create time intervals
ehout$date <- mdy(ehout$incidentdate) # puts into date format
ehout$year<-year(ehout$date)
ehout$month<-month(ehout$date)
ehout$month<-sprintf("%02d",ehout$month)
ehout$day<-day(ehout$date)
ehout$day<-sprintf("%02d",ehout$day)

ehout$date.num <- as.numeric(paste0(ehout$year, ehout$month, ehout$day)) 

ehout$ytd<-ifelse(ehout$date.num>20130901 & ehout$date.num<20140201, 1, 
                  ifelse(ehout$date.num>20140901 & ehout$date.num<20150201, 1, 
                         ifelse(ehout$date.num>20150901 & ehout$date.num<20160201, 1, 
                                ifelse(ehout$date.num>20160821 & ehout$date.num<20170201, 1, 0)
                         )
                  )
)


#####filter to proper data set
library(plyr)
library(dplyr)
ehout2<-filter(ehout, incidenttype=="referral")
ehout3<-filter(ehout2, ytd==1)
ehout3<-filter(ehout3, grade!=15)

####data analysis for all incidents

ehout3$raceethnicity<-revalue(ehout3$raceethnicity, c("A" = "O", "I" = "O",
                                                      "M" = "O", "P" = "O"))

total.oss<-as.data.frame(aggregate(oss ~ schoolyear + schoolname + gender + raceethnicity + iep + ell + frpl + grade, data=ehout3, FUN=sum))
total.ossdays<-as.data.frame(aggregate(ossdays ~ schoolyear+ schoolname + gender + raceethnicity + iep + ell + frpl + grade, data=ehout3, FUN=sum))
total.iss<-as.data.frame(aggregate(iss ~ schoolyear+ schoolname + gender + raceethnicity + iep + ell + frpl + grade, data=ehout3, FUN=sum))
total.issdays<-as.data.frame(aggregate(issdays ~ schoolyear+ schoolname + gender + raceethnicity + iep + ell + frpl + grade, data=ehout3, FUN=sum))
total.susp<-as.data.frame(aggregate(totalsuspension ~ schoolyear+ schoolname + gender + raceethnicity + iep + ell + frpl + grade, data=ehout3, FUN=sum))
total.suspdays<-as.data.frame(aggregate(totalsuspduration ~ schoolyear+ schoolname + gender + raceethnicity + iep + ell + frpl + grade, data=ehout3, FUN=sum))

all.susp.categories<-merge(total.oss, total.ossdays)
all.susp.categories<-merge(all.susp.categories, total.iss)
all.susp.categories<-merge(all.susp.categories, total.issdays)
all.susp.categories<-merge(all.susp.categories, total.susp)
all.susp.categories<-merge(all.susp.categories, total.suspdays)

#subset for grades
all.susp.categories1<-subset(all.susp.categories,grade=="KF")
all.susp.categories2<-subset(all.susp.categories,grade=="1")
all.susp.categories3<-subset(all.susp.categories,grade=="2")
all.susp.categories4<-subset(all.susp.categories,grade=="3")
all.susp.categories5<-subset(all.susp.categories,grade=="4")
all.susp.categories6<-subset(all.susp.categories,grade=="5")
all.susp.categories7<-subset(all.susp.categories,grade=="13")
# merge for grades
k5suspytd<-rbind(all.susp.categories1,all.susp.categories2)
k5suspytd<-rbind(k5suspytd,all.susp.categories3)
k5suspytd<-rbind(k5suspytd,all.susp.categories4)
k5suspytd<-rbind(k5suspytd,all.susp.categories5)
k5suspytd<-rbind(k5suspytd,all.susp.categories6)
k5suspytd<-rbind(k5suspytd,all.susp.categories7)

k5suspytd$K2<-ifelse(k5suspytd$grade<3,1,0)

#write CSV
write.csv(k5suspytd, "C:/Users/drobil66/Desktop/RFiles/k5suspytd.csv") 
