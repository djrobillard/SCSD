library(lubridate)
library(dplyr)




rawattendance<-read.csv("attendancedaily.csv")
attdemo<-read.csv("attendancedaily.csv")

rawattendance$Absent<-as.numeric(rawattendance$'Period.Absence..'>.5)
rawattendance$daycount = 1
rawattendance$Month<-month(rawattendance$cal_date)

rawattendance$Month<-month.abb[rawattendance$Month]




totaldays<-as.data.frame(aggregate(daycount ~ name + Month,  data=rawattendance, FUN=sum))
totalabsent<-as.data.frame(aggregate(Absent ~ name + Month,  data=rawattendance, FUN=sum))
together<-merge(totaldays,totalabsent, by=c("name","Month"),all=TRUE)
together$Percentage<-((together$daycount-together$Absent)/(together$daycount))

together$Percentage <- paste(round((together$Percentage)*100,digits=1),"%",sep="")

together<-together[,c("Month","name","Percentage")]

togetherfinal<-reshape(together, idvar="name", timevar="Month", direction="wide")

togetherfinal<-rename(togetherfinal,Dec=Percentage.Dec,
                   Feb=Percentage.Feb,
                   Jan=Percentage.Jan,
                  Nov=Percentage.Nov,
                  Oct=Percentage.Oct,
                  Sep=Percentage.Sep,
                  Apr=Percentage.Apr,
                  Mar=Percentage.Mar)

rawattendance<-togetherfinal[,c("name","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr")]
attdemo<-select(attdemo,STUDENT_ID,grade,name)
rawattendance<-merge(rawattendance,attdemo,by="STUDENT_ID")
rawattendance<-rawattendance[,c("name","grade","STUDENT_ID","YTDAtt","SepAtt","OctAtt","NovAtt","DecAtt","JanAtt","FebAtt","MarAtt","AprAtt")]

Westside<-subset(rawattendance,rawattendance$name=="Westside Academy at Blodgett")
Huntington<-subset(rawattendance,rawattendance$name=="Huntington Middle School")
ELMS<-subset(rawattendance,rawattendance$name=="Expeditionary Learning Middle School")
HWSMITH<-subset(rawattendance,rawattendance$name=="Hurlbut W. Smith Middle School")
EDSMITH<-subset(rawattendance,rawattendance$name=="Edward Smith Middle")
Frazer<-subset(rawattendance,rawattendance$name=="Frazer Middle School")
Roberts<-subset(rawattendance,rawattendance$name=="Roberts Middle School")
Grant<-subset(rawattendance,rawattendance$name=="Grant Middle School")
Danforth<-subset(rawattendance,rawattendance$name=="Danforth Middle School")
MSAP<-subset(rawattendance,rawattendance$name=="MSAP")
Clary<-subset(rawattendance,rawattendance$name=="Clary Middle School")
Lincoln<-subset(rawattendance,rawattendance$name=="Lincoln Middle School")

msattendance1<-rbind(Westside, Huntington)
msattendance2<-rbind(msattendance1, ELMS)
msattendance3<-rbind(msattendance2, EDSMITH)
msattendance4<-rbind(msattendance3, HWSMITH)
msattendance5<-rbind(msattendance4, Frazer)
msattendance6<-rbind(msattendance5, Roberts)
msattendance7<-rbind(msattendance6, Grant)
msattendance8<-rbind(msattendance7, Danforth)
msattendance9<-rbind(msattendance8, MSAP)
msattendance10<-rbind(msattendance9, Clary)
msattendance11<-rbind(msattendance10, Lincoln)

write.csv(msattendance11,"C:/Users/drobil66/Desktop/RFiles/R Reports/Msattendance.csv")


