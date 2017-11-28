View(everything)

#creating tables

#by Race
Renroll<-aggregate(StudentCount~SchoolYear+SchoolName+RaceEthnicity,data=everything,FUN=sum)
ROSSUnique<-aggregate(OneOSS~SchoolYear+SchoolName+RaceEthnicity,data=everything,FUN=sum)
ROSSMO<-aggregate(RepeatOSS~SchoolYear+SchoolName+RaceEthnicity,data=everything,FUN=sum)
ROSSDuration<-aggregate(OSS.Days~SchoolYear+SchoolName+RaceEthnicity,data=everything,FUN=sum)
RISSUnique<-aggregate(OneISS~SchoolYear+SchoolName+RaceEthnicity,data=everything,FUN=sum)
RISSMO<-aggregate(RepeatISS~SchoolYear+SchoolName+RaceEthnicity,data=everything,FUN=sum)
RISSDuration<-aggregate(ISS.Days~SchoolYear+SchoolName+RaceEthnicity,data=everything,FUN=sum)
RRefUnique<-aggregate(OneRef~SchoolYear+SchoolName+RaceEthnicity,data=everything,FUN=sum)
RRefMO<-aggregate(RepeatRef~SchoolYear+SchoolName+RaceEthnicity,data=everything,FUN=sum)
RSuspUnique<-aggregate(OneSusp~SchoolYear+SchoolName+RaceEthnicity,data=everything,FUN=sum)
RSuspMO<-aggregate(RepeatSusp~SchoolYear+SchoolName+RaceEthnicity,data=everything,FUN=sum)
RTotalSusp<-aggregate(Total.Suspension~SchoolYear+SchoolName+RaceEthnicity,data=everything,FUN=sum)
RTotalSuspDuration<-aggregate(Total.Suspension.Duration~SchoolYear+SchoolName+RaceEthnicity,data=everything,FUN=sum)

ByR.Count<-merge(Renroll,ROSSUnique,by=c("SchoolYear","SchoolName","RaceEthnicity"),all=TRUE)
ByR.Count<-merge(ByR.Count,ROSSMO,by=c("SchoolYear","SchoolName","RaceEthnicity"),all=TRUE)
ByR.Count<-merge(ByR.Count,ROSSDuration,by=c("SchoolYear","SchoolName","RaceEthnicity"),all=TRUE)
ByR.Count<-merge(ByR.Count,RISSUnique,by=c("SchoolYear","SchoolName","RaceEthnicity"),all=TRUE)
ByR.Count<-merge(ByR.Count,RISSMO,by=c("SchoolYear","SchoolName","RaceEthnicity"),all=TRUE)
ByR.Count<-merge(ByR.Count,RISSDuration,by=c("SchoolYear","SchoolName","RaceEthnicity"),all=TRUE)
ByR.Count<-merge(ByR.Count,RRefUnique,by=c("SchoolYear","SchoolName","RaceEthnicity"),all=TRUE)
ByR.Count<-merge(ByR.Count,RRefMO,by=c("SchoolYear","SchoolName","RaceEthnicity"),all=TRUE)
ByR.Count<-merge(ByR.Count,RSuspUnique,by=c("SchoolYear","SchoolName","RaceEthnicity"),all=TRUE)
ByR.Count<-merge(ByR.Count,RSuspMO,by=c("SchoolYear","SchoolName","RaceEthnicity"),all=TRUE)
ByR.Count<-merge(ByR.Count,RTotalSusp,by=c("SchoolYear","SchoolName","RaceEthnicity"),all=TRUE)
ByR.Count<-merge(ByR.Count,RTotalSuspDuration,by=c("SchoolYear","SchoolName","RaceEthnicity"),all=TRUE)

ByR.Rate<-ByR.Count%>%
  mutate(UniqueOSSRate=(OneOSS/StudentCount),
         PlusOSSRate=(RepeatOSS/StudentCount),
         OSSDurRate=(OSS.Days/StudentCount),
         UniqueISSRate=(OneISS/StudentCount),
         PlusISSRate=(RepeatISS/StudentCount),
         ISSDurRate=(ISS.Days/StudentCount),
         UniqueRefRate=(OneRef/StudentCount),
         PlusRefRate=(RepeatRef/StudentCount),
         AnySuspRate=(OneSusp/StudentCount),
         PlusAnySuspRate=(RepeatSusp/StudentCount),
         TotalSuspRate=(Total.Suspension/StudentCount),
         TotalDurRate=(Total.Suspension.Duration/StudentCount))%>%
  select(SchoolYear,SchoolName,RaceEthnicity,StudentCount,UniqueOSSRate,PlusOSSRate,OSSDurRate,UniqueISSRate,
         PlusISSRate,ISSDurRate,UniqueRefRate,PlusRefRate,AnySuspRate,PlusAnySuspRate,TotalSuspRate,
         TotalDurRate)

ByR.Count<-select(ByR.Count,SchoolYear,SchoolName,RaceEthnicity,StudentCount,OneOSS,RepeatOSS,OSS.Days,OneISS,RepeatISS,
                  ISS.Days,OneRef,RepeatRef,OneSusp,RepeatSusp,Total.Suspension,Total.Suspension.Duration)

ByR.Rate$UniqueOSSRate<-round(ByR.Rate$UniqueOSSRate,digits=2)
ByR.Rate$PlusOSSRate<-round(ByR.Rate$PlusOSSRate,digits=2)
ByR.Rate$OSSDurRate<-round(ByR.Rate$OSSDurRate,digits=2)
ByR.Rate$UniqueISSRate<-round(ByR.Rate$UniqueISSRate,digits=2)
ByR.Rate$PlusISSRate<-round(ByR.Rate$PlusISSRate,digits=2)
ByR.Rate$ISSDurRate<-round(ByR.Rate$ISSDurRate,digits=2)
ByR.Rate$UniqueRefRate<-round(ByR.Rate$UniqueRefRate,digits=2)
ByR.Rate$PlusRefRate<-round(ByR.Rate$PlusRefRate,digits=2)
ByR.Rate$AnySuspRate<-round(ByR.Rate$AnySuspRate,digits=2)
ByR.Rate$PlusAnySuspRate<-round(ByR.Rate$PlusAnySuspRate,digits=2)
ByR.Rate$TotalSuspRate<-round(ByR.Rate$TotalSuspRate,digits=2)
ByR.Rate$TotalDurRate<-round(ByR.Rate$TotalDurRate,digits=2)

#By Race, Gender
RGenroll<-aggregate(StudentCount~SchoolYear+SchoolName+RaceEthnicity+Gender,data=everything,FUN=sum)
RGOSSUnique<-aggregate(OneOSS~SchoolYear+SchoolName+RaceEthnicity+Gender,data=everything,FUN=sum)
RGOSSMO<-aggregate(RepeatOSS~SchoolYear+SchoolName+RaceEthnicity+Gender,data=everything,FUN=sum)
RGOSSDuration<-aggregate(OSS.Days~SchoolYear+SchoolName+RaceEthnicity+Gender,data=everything,FUN=sum)
RGISSUnique<-aggregate(OneISS~SchoolYear+SchoolName+RaceEthnicity+Gender,data=everything,FUN=sum)
RGISSMO<-aggregate(RepeatISS~SchoolYear+SchoolName+RaceEthnicity+Gender,data=everything,FUN=sum)
RGISSDuration<-aggregate(ISS.Days~SchoolYear+SchoolName+RaceEthnicity+Gender,data=everything,FUN=sum)
RGRefUnique<-aggregate(OneRef~SchoolYear+SchoolName+RaceEthnicity+Gender,data=everything,FUN=sum)
RGRefMO<-aggregate(RepeatRef~SchoolYear+SchoolName+RaceEthnicity+Gender,data=everything,FUN=sum)
RGSuspUnique<-aggregate(OneSusp~SchoolYear+SchoolName+RaceEthnicity+Gender,data=everything,FUN=sum)
RGSuspMO<-aggregate(RepeatSusp~SchoolYear+SchoolName+RaceEthnicity+Gender,data=everything,FUN=sum)
RGTotalSusp<-aggregate(Total.Suspension~SchoolYear+SchoolName+RaceEthnicity+Gender,data=everything,FUN=sum)
RGTotalSuspDuration<-aggregate(Total.Suspension.Duration~SchoolYear+SchoolName+RaceEthnicity+Gender,data=everything,FUN=sum)

ByRG.Count<-merge(RGenroll,RGOSSUnique,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender"),all=TRUE)
ByRG.Count<-merge(ByRG.Count,RGOSSMO,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender"),all=TRUE)
ByRG.Count<-merge(ByRG.Count,RGOSSDuration,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender"),all=TRUE)
ByRG.Count<-merge(ByRG.Count,RGISSUnique,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender"),all=TRUE)
ByRG.Count<-merge(ByRG.Count,RGISSMO,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender"),all=TRUE)
ByRG.Count<-merge(ByRG.Count,RGISSDuration,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender"),all=TRUE)
ByRG.Count<-merge(ByRG.Count,RGRefUnique,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender"),all=TRUE)
ByRG.Count<-merge(ByRG.Count,RGRefMO,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender"),all=TRUE)
ByRG.Count<-merge(ByRG.Count,RGSuspUnique,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender"),all=TRUE)
ByRG.Count<-merge(ByRG.Count,RGSuspMO,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender"),all=TRUE)
ByRG.Count<-merge(ByRG.Count,RGTotalSusp,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender"),all=TRUE)
ByRG.Count<-merge(ByRG.Count,RGTotalSuspDuration,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender"),all=TRUE)



ByRG.Rate<-ByRG.Count%>%
  mutate(UniqueOSSRate=(OneOSS/StudentCount),
         PlusOSSRate=(RepeatOSS/StudentCount),
         OSSDurRate=(OSS.Days/StudentCount),
         UniqueISSRate=(OneISS/StudentCount),
         PlusISSRate=(RepeatISS/StudentCount),
         ISSDurRate=(ISS.Days/StudentCount),
         UniqueRefRate=(OneRef/StudentCount),
         PlusRefRate=(RepeatRef/StudentCount),
         AnySuspRate=(OneSusp/StudentCount),
         PlusAnySuspRate=(RepeatSusp/StudentCount),
         TotalSuspRate=(Total.Suspension/StudentCount),
         TotalDurRate=(Total.Suspension.Duration/StudentCount))%>%
  select(SchoolYear,SchoolName,RaceEthnicity,Gender,StudentCount,UniqueOSSRate,PlusOSSRate,OSSDurRate,UniqueISSRate,
         PlusISSRate,ISSDurRate,UniqueRefRate,PlusRefRate,AnySuspRate,PlusAnySuspRate,TotalSuspRate,
         TotalDurRate)


ByRG.Count<-select(ByRG.Count,SchoolYear,SchoolName,RaceEthnicity,Gender,StudentCount,OneOSS,RepeatOSS,OSS.Days,OneISS,RepeatISS,
                   ISS.Days,OneRef,RepeatRef,OneSusp,RepeatSusp,Total.Suspension,Total.Suspension.Duration)

ByRG.Rate$UniqueOSSRate<-round(ByRG.Rate$UniqueOSSRate,digits=2)
ByRG.Rate$PlusOSSRate<-round(ByRG.Rate$PlusOSSRate,digits=2)
ByRG.Rate$OSSDurRate<-round(ByRG.Rate$OSSDurRate,digits=2)
ByRG.Rate$UniqueISSRate<-round(ByRG.Rate$UniqueISSRate,digits=2)
ByRG.Rate$PlusISSRate<-round(ByRG.Rate$PlusISSRate,digits=2)
ByRG.Rate$ISSDurRate<-round(ByRG.Rate$ISSDurRate,digits=2)
ByRG.Rate$UniqueRefRate<-round(ByRG.Rate$UniqueRefRate,digits=2)
ByRG.Rate$PlusRefRate<-round(ByRG.Rate$PlusRefRate,digits=2)
ByRG.Rate$AnySuspRate<-round(ByRG.Rate$AnySuspRate,digits=2)
ByRG.Rate$PlusAnySuspRate<-round(ByRG.Rate$PlusAnySuspRate,digits=2)
ByRG.Rate$TotalSuspRate<-round(ByRG.Rate$TotalSuspRate,digits=2)
ByRG.Rate$TotalDurRate<-round(ByRG.Rate$TotalDurRate,digits=2)

#By IEP Status, Race, Gender
IRGenroll<-aggregate(StudentCount~SchoolYear+SchoolName+IEP+RaceEthnicity+Gender,data=everything,FUN=sum)
IRGOSSUnique<-aggregate(OneOSS~SchoolYear+SchoolName+IEP+RaceEthnicity+Gender,data=everything,FUN=sum)
IRGOSSMO<-aggregate(RepeatOSS~SchoolYear+SchoolName+IEP+RaceEthnicity+Gender,data=everything,FUN=sum)
IRGOSSDuration<-aggregate(OSS.Days~SchoolYear+SchoolName+IEP+RaceEthnicity+Gender,data=everything,FUN=sum)
IRGISSUnique<-aggregate(OneISS~SchoolYear+SchoolName+IEP+RaceEthnicity+Gender,data=everything,FUN=sum)
IRGISSMO<-aggregate(RepeatISS~SchoolYear+SchoolName+IEP+RaceEthnicity+Gender,data=everything,FUN=sum)
IRGISSDuration<-aggregate(ISS.Days~SchoolYear+SchoolName+IEP+RaceEthnicity+Gender,data=everything,FUN=sum)
IRGRefUnique<-aggregate(OneRef~SchoolYear+SchoolName+IEP+RaceEthnicity+Gender,data=everything,FUN=sum)
IRGRefMO<-aggregate(RepeatRef~SchoolYear+SchoolName+IEP+RaceEthnicity+Gender,data=everything,FUN=sum)
IRGSuspUnique<-aggregate(OneSusp~SchoolYear+SchoolName+IEP+RaceEthnicity+Gender,data=everything,FUN=sum)
IRGSuspMO<-aggregate(RepeatSusp~SchoolYear+SchoolName+IEP+RaceEthnicity+Gender,data=everything,FUN=sum)
IRGTotalSusp<-aggregate(Total.Suspension~SchoolYear+SchoolName+IEP+RaceEthnicity+Gender,data=everything,FUN=sum)
IRGTotalSuspDuration<-aggregate(Total.Suspension.Duration~SchoolYear+SchoolName+IEP+RaceEthnicity+Gender,data=everything,FUN=sum)

ByIRG.Count<-merge(IRGenroll,IRGOSSUnique,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender","IEP"),all=TRUE)
ByIRG.Count<-merge(ByIRG.Count,IRGOSSMO,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender","IEP"),all=TRUE)
ByIRG.Count<-merge(ByIRG.Count,IRGOSSDuration,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender","IEP"),all=TRUE)
ByIRG.Count<-merge(ByIRG.Count,IRGISSUnique,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender","IEP"),all=TRUE)
ByIRG.Count<-merge(ByIRG.Count,IRGISSMO,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender","IEP"),all=TRUE)
ByIRG.Count<-merge(ByIRG.Count,IRGISSDuration,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender","IEP"),all=TRUE)
ByIRG.Count<-merge(ByIRG.Count,IRGRefUnique,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender","IEP"),all=TRUE)
ByIRG.Count<-merge(ByIRG.Count,IRGRefMO,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender","IEP"),all=TRUE)
ByIRG.Count<-merge(ByIRG.Count,IRGSuspUnique,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender","IEP"),all=TRUE)
ByIRG.Count<-merge(ByIRG.Count,IRGSuspMO,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender","IEP"),all=TRUE)
ByIRG.Count<-merge(ByIRG.Count,IRGTotalSusp,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender","IEP"),all=TRUE)
ByIRG.Count<-merge(ByIRG.Count,IRGTotalSuspDuration,by=c("SchoolYear","SchoolName","RaceEthnicity","Gender","IEP"),all=TRUE)


ByIRG.Rate<-ByIRG.Count%>%
  mutate(UniqueOSSRate=(OneOSS/StudentCount),
         PlusOSSRate=(RepeatOSS/StudentCount),
         OSSDurRate=(OSS.Days/StudentCount),
         UniqueISSRate=(OneISS/StudentCount),
         PlusISSRate=(RepeatISS/StudentCount),
         ISSDurRate=(ISS.Days/StudentCount),
         UniqueRefRate=(OneRef/StudentCount),
         PlusRefRate=(RepeatRef/StudentCount),
         AnySuspRate=(OneSusp/StudentCount),
         PlusAnySuspRate=(RepeatSusp/StudentCount),
         TotalSuspRate=(Total.Suspension/StudentCount),
         TotalDurRate=(Total.Suspension.Duration/StudentCount))%>%
  select(SchoolYear,SchoolName,IEP,RaceEthnicity,Gender,StudentCount,UniqueOSSRate,PlusOSSRate,OSSDurRate,UniqueISSRate,
         PlusISSRate,ISSDurRate,UniqueRefRate,PlusRefRate,AnySuspRate,PlusAnySuspRate,TotalSuspRate,
         TotalDurRate)

ByIRG.Count<-select(ByIRG.Count,SchoolYear,SchoolName,IEP,RaceEthnicity,Gender,StudentCount,OneOSS,RepeatOSS,OSS.Days,OneISS,RepeatISS,
                    ISS.Days,OneRef,RepeatRef,OneSusp,RepeatSusp,Total.Suspension,Total.Suspension.Duration)

ByIRG.Rate$UniqueOSSRate<-round(ByIRG.Rate$UniqueOSSRate,digits=2)
ByIRG.Rate$PlusOSSRate<-round(ByIRG.Rate$PlusOSSRate,digits=2)
ByIRG.Rate$OSSDurRate<-round(ByIRG.Rate$OSSDurRate,digits=2)
ByIRG.Rate$UniqueISSRate<-round(ByIRG.Rate$UniqueISSRate,digits=2)
ByIRG.Rate$PlusISSRate<-round(ByIRG.Rate$PlusISSRate,digits=2)
ByIRG.Rate$ISSDurRate<-round(ByIRG.Rate$ISSDurRate,digits=2)
ByIRG.Rate$UniqueRefRate<-round(ByIRG.Rate$UniqueRefRate,digits=2)
ByIRG.Rate$PlusRefRate<-round(ByIRG.Rate$PlusRefRate,digits=2)
ByIRG.Rate$AnySuspRate<-round(ByIRG.Rate$AnySuspRate,digits=2)
ByIRG.Rate$PlusAnySuspRate<-round(ByIRG.Rate$PlusAnySuspRate,digits=2)
ByIRG.Rate$TotalSuspRate<-round(ByIRG.Rate$TotalSuspRate,digits=2)
ByIRG.Rate$TotalDurRate<-round(ByIRG.Rate$TotalDurRate,digits=2)

#By IEP, ENL, Gender
IEGenroll<-aggregate(StudentCount~SchoolYear+SchoolName+IEP+ELL+Gender,data=everything,FUN=sum)
IEGOSSUnique<-aggregate(OneOSS~SchoolYear+SchoolName+IEP+ELL+Gender,data=everything,FUN=sum)
IEGOSSMO<-aggregate(RepeatOSS~SchoolYear+SchoolName+IEP+ELL+Gender,data=everything,FUN=sum)
IEGOSSDuration<-aggregate(OSS.Days~SchoolYear+SchoolName+IEP+ELL+Gender,data=everything,FUN=sum)
IEGISSUnique<-aggregate(OneISS~SchoolYear+SchoolName+IEP+ELL+Gender,data=everything,FUN=sum)
IEGISSMO<-aggregate(RepeatISS~SchoolYear+SchoolName+IEP+ELL+Gender,data=everything,FUN=sum)
IEGISSDuration<-aggregate(ISS.Days~SchoolYear+SchoolName+IEP+ELL+Gender,data=everything,FUN=sum)
IEGRefUnique<-aggregate(OneRef~SchoolYear+SchoolName+IEP+ELL+Gender,data=everything,FUN=sum)
IEGRefMO<-aggregate(RepeatRef~SchoolYear+SchoolName+IEP+ELL+Gender,data=everything,FUN=sum)
IEGSuspUnique<-aggregate(OneSusp~SchoolYear+SchoolName+IEP+ELL+Gender,data=everything,FUN=sum)
IEGSuspMO<-aggregate(RepeatSusp~SchoolYear+SchoolName+IEP+ELL+Gender,data=everything,FUN=sum)
IEGTotalSusp<-aggregate(Total.Suspension~SchoolYear+SchoolName+IEP+ELL+Gender,data=everything,FUN=sum)
IEGTotalSuspDuration<-aggregate(Total.Suspension.Duration~SchoolYear+SchoolName+IEP+ELL+Gender,data=everything,FUN=sum)

ByIEG.Count<-merge(IEGenroll,IEGOSSUnique,by=c("SchoolYear","SchoolName","ELL","Gender","IEP"),all=TRUE)
ByIEG.Count<-merge(ByIEG.Count,IEGOSSMO,by=c("SchoolYear","SchoolName","ELL","Gender","IEP"),all=TRUE)
ByIEG.Count<-merge(ByIEG.Count,IEGOSSDuration,by=c("SchoolYear","SchoolName","ELL","Gender","IEP"),all=TRUE)
ByIEG.Count<-merge(ByIEG.Count,IEGISSUnique,by=c("SchoolYear","SchoolName","ELL","Gender","IEP"),all=TRUE)
ByIEG.Count<-merge(ByIEG.Count,IEGISSMO,by=c("SchoolYear","SchoolName","ELL","Gender","IEP"),all=TRUE)
ByIEG.Count<-merge(ByIEG.Count,IEGISSDuration,by=c("SchoolYear","SchoolName","ELL","Gender","IEP"),all=TRUE)
ByIEG.Count<-merge(ByIEG.Count,IEGRefUnique,by=c("SchoolYear","SchoolName","ELL","Gender","IEP"),all=TRUE)
ByIEG.Count<-merge(ByIEG.Count,IEGRefMO,by=c("SchoolYear","SchoolName","ELL","Gender","IEP"),all=TRUE)
ByIEG.Count<-merge(ByIEG.Count,IEGSuspUnique,by=c("SchoolYear","SchoolName","ELL","Gender","IEP"),all=TRUE)
ByIEG.Count<-merge(ByIEG.Count,IEGSuspMO,by=c("SchoolYear","SchoolName","ELL","Gender","IEP"),all=TRUE)
ByIEG.Count<-merge(ByIEG.Count,IEGTotalSusp,by=c("SchoolYear","SchoolName","ELL","Gender","IEP"),all=TRUE)
ByIEG.Count<-merge(ByIEG.Count,IEGTotalSuspDuration,by=c("SchoolYear","SchoolName","ELL","Gender","IEP"),all=TRUE)


ByIEG.Rate<-ByIEG.Count%>%
  mutate(UniqueOSSRate=(OneOSS/StudentCount),
        PlusOSSRate=(RepeatOSS/StudentCount),
        OSSDurRate=(OSS.Days/StudentCount),
        UniqueISSRate=(OneISS/StudentCount),
        PlusISSRate=(RepeatISS/StudentCount),
        ISSDurRate=(ISS.Days/StudentCount),
        UniqueRefRate=(OneRef/StudentCount),
        PlusRefRate=(RepeatRef/StudentCount),
        AnySuspRate=(OneSusp/StudentCount),
        PlusAnySuspRate=(RepeatSusp/StudentCount),
        TotalSuspRate=(Total.Suspension/StudentCount),
        TotalDurRate=(Total.Suspension.Duration/StudentCount))%>%
  select(SchoolYear,SchoolName,IEP,ELL,Gender,StudentCount,UniqueOSSRate,PlusOSSRate,OSSDurRate,UniqueISSRate,
         PlusISSRate,ISSDurRate,UniqueRefRate,PlusRefRate,AnySuspRate,PlusAnySuspRate,TotalSuspRate,
         TotalDurRate)

ByIEG.Count<-select(ByIEG.Count,SchoolYear,SchoolName,IEP,ELL,Gender,StudentCount,OneOSS,RepeatOSS,OSS.Days,OneISS,RepeatISS,
                    ISS.Days,OneRef,RepeatRef,OneSusp,RepeatSusp,Total.Suspension,Total.Suspension.Duration)

ByIEG.Rate$UniqueOSSRate<-round(ByIEG.Rate$UniqueOSSRate,digits=2)
ByIEG.Rate$PlusOSSRate<-round(ByIEG.Rate$PlusOSSRate,digits=2)
ByIEG.Rate$OSSDurRate<-round(ByIEG.Rate$OSSDurRate,digits=2)
ByIEG.Rate$UniqueISSRate<-round(ByIEG.Rate$UniqueISSRate,digits=2)
ByIEG.Rate$PlusISSRate<-round(ByIEG.Rate$PlusISSRate,digits=2)
ByIEG.Rate$ISSDurRate<-round(ByIEG.Rate$ISSDurRate,digits=2)
ByIEG.Rate$UniqueRefRate<-round(ByIEG.Rate$UniqueRefRate,digits=2)
ByIEG.Rate$PlusRefRate<-round(ByIEG.Rate$PlusRefRate,digits=2)
ByIEG.Rate$AnySuspRate<-round(ByIEG.Rate$AnySuspRate,digits=2)
ByIEG.Rate$PlusAnySuspRate<-round(ByIEG.Rate$PlusAnySuspRate,digits=2)
ByIEG.Rate$TotalSuspRate<-round(ByIEG.Rate$TotalSuspRate,digits=2)
ByIEG.Rate$TotalDurRate<-round(ByIEG.Rate$TotalDurRate,digits=2)

####writing to file
write.csv(ByIEG.Count, "C:/Users/drobil66/Desktop/RFiles/R Reports/ByIEG.Count.csv")
write.csv(ByIEG.Rate, "C:/Users/drobil66/Desktop/RFiles/R Reports/ByIEG.Rate.csv")
write.csv(ByR.Count, "C:/Users/drobil66/Desktop/RFiles/R Reports/ByR.Count.csv")
write.csv(ByR.Rate, "C:/Users/drobil66/Desktop/RFiles/R Reports/ByR.Rate.csv")
write.csv(ByRG.Count, "C:/Users/drobil66/Desktop/RFiles/R Reports/ByRG.Count.csv")
write.csv(ByRG.Rate, "C:/Users/drobil66/Desktop/RFiles/R Reports/ByRG.Rate.csv")
write.csv(ByIRG.Count, "C:/Users/drobil66/Desktop/RFiles/R Reports/ByIRG.Count.csv")
write.csv(ByIRG.Rate, "C:/Users/drobil66/Desktop/RFiles/R Reports/ByIRG.Rate.csv")


############################creating files by statistic

everything.Rate<-everything%>%
  mutate(UniqueOSSRate=(OneOSS/StudentCount),
         PlusOSSRate=(RepeatOSS/StudentCount),
         OSSDurRate=(OSS.Days/StudentCount),
         UniqueISSRate=(OneISS/StudentCount),
         PlusISSRate=(RepeatISS/StudentCount),
         ISSDurRate=(ISS.Days/StudentCount),
         UniqueRefRate=(OneRef/StudentCount),
         PlusRefRate=(RepeatRef/StudentCount),
         AnySuspRate=(OneSusp/StudentCount),
         PlusAnySuspRate=(RepeatSusp/StudentCount),
         TotalSuspRate=(Total.Suspension/StudentCount),
         TotalDurRate=(Total.Suspension.Duration/StudentCount))%>%
  select(SchoolYear,SchoolName,RaceEthnicity,IEP,ELL,StudentCount,UniqueOSSRate,PlusOSSRate,OSSDurRate,UniqueISSRate,
         PlusISSRate,ISSDurRate,UniqueRefRate,PlusRefRate,AnySuspRate,PlusAnySuspRate,TotalSuspRate,
         TotalDurRate)

everything.Rate$UniqueOSSRate<-round(everything.Rate$UniqueOSSRate,digits=2)
everything.Rate$PlusOSSRate<-round(everything.Rate$PlusOSSRate,digits=2)
everything.Rate$OSSDurRate<-round(everything.Rate$OSSDurRate,digits=2)
everything.Rate$UniqueISSRate<-round(everything.Rate$UniqueISSRate,digits=2)
everything.Rate$PlusISSRate<-round(everything.Rate$PlusISSRate,digits=2)
everything.Rate$ISSDurRate<-round(everything.Rate$ISSDurRate,digits=2)
everything.Rate$UniqueRefRate<-round(everything.Rate$UniqueRefRate,digits=2)
everything.Rate$PlusRefRate<-round(everything.Rate$PlusRefRate,digits=2)
everything.Rate$AnySuspRate<-round(everything.Rate$AnySuspRate,digits=2)
everything.Rate$PlusAnySuspRate<-round(everything.Rate$PlusAnySuspRate,digits=2)
everything.Rate$TotalSuspRate<-round(everything.Rate$TotalSuspRate,digits=2)
everything.Rate$TotalDurRate<-round(everything.Rate$TotalDurRate,digits=2)

everythingtogether<-merge(everything,everything.Rate,by=c("SchoolYear","SchoolName","RaceEthnicity","IEP","ELL","StudentCount"))

write.csv(everythingtogether, "C:/Users/drobil66/Desktop/RFiles/R Reports/everythingtogether1.csv")

####Rates of Each Thing


