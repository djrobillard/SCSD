ehout<-read.csv("ehout all years.csv")
library(lubridate)

###parse dates and create time intervals

ehout$date <- as.Date(ehout$IncidentDate,format="%m/%d/%Y") # puts into date format
ehout$date<-ymd(ehout$date)
ehout$year<-year(ehout$date) # parse year
ehout$month<-month(ehout$date) # parse month
ehout$month<-sprintf("%02d",ehout$month) # create 2 digit month
ehout$day<-day(ehout$date) # parse day
ehout$day<-sprintf("%02d",ehout$day) # create 2 digit day

ehout$date.num <- as.numeric(paste0(ehout$year, ehout$month, ehout$day)) # concatenate

#ytd calculations
ehout$ytd<-ifelse(ehout$date.num>20130901 & ehout$date.num<=20140531, 1, 
                  ifelse(ehout$date.num>20140901 & ehout$date.num<=20150531, 1, 
                         ifelse(ehout$date.num>20150901 & ehout$date.num<=20160531, 1, 
                                ifelse(ehout$date.num>20160821 & ehout$date.num<=20170531, 1, 0)
                         )
                  )
)
ehout<-filter(ehout,ytd==1)
write.csv(ehout, "C:/Users/drobil66/Desktop/RFiles/R Reports/EhoutYTD.csv") 
