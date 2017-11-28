library(plyr)
library("polycor")
library("psych")
library("dplyr")

TFA<-read.csv("TFA.csv",strip.white = TRUE)

TFAClean<-TFA %>%
  rename(CMID=CM.Identification,
         CMTenure=CM.Tenure,
         MainSubject=Main.Subject.Taught,
         MainGrade=Main.Grade.Taught,
         CoachTenure=Coach.Tenure,
         OverallSatisfaction=How.would.you.describe.your.overall.satisfaction.with.Teach.For.America,
         Valued=I.feel.valued.as.a.person.by.Teach.For.America.staff,
         Opinions=My.opinions.matter.at.Teach.For.America,
         Proud=I.am.proud.to.be.a.part.of.Teach.For.America.,
         TFANet=Offerings.on.TFANet.are.critical.in.my.efforts.to.lead.my.students.to.academic.achievement,
         OngoingSupport=The.ongoing.support.tools.and.learning.experiences.that.Teach.For.America.provides.play.a.critical.role.in.my.efforts.to.be.a.successful.teacher,
         CoachCritical=My.coach.is.critical.in.my.ability.to.become.a.successful.teacher,
         CoachHours=How.many.hours.a.month.do.you.spend.with.your.coach.,
         ExtraCurric=Do.you.lead.an.extracurricular.activity.at.your.school............,
         Institute=My.experiences.at.Institute.have.been.critical.in.my.efforts.to.become.a.successful.teacher.,
         Recommend=I.would.recommend.Teach.for.America.to.a.friend,
         Safe=I.feel.safe.in.my.school) %>%
  select(OverallSatisfaction,CoachTenure,Valued,Opinions,Proud,TFANet,OngoingSupport,
         CoachCritical,CoachHours,ExtraCurric,Institute,Recommend,Safe)
TFAClean$CoachTenure<-revalue(TFAClean$CoachTenure,c("3rd year"="3","2nd year"="2","1st year"="1","Missing"=""))
TFAClean$ExtraCurric<-revalue(TFAClean$ExtraCurric,c("Yes"="1","No"="0"))
TFAClean$TFANet<-as.character(as.numeric(TFAClean$TFANet))
TFAClean$TFANet<-revalue(TFAClean$TFANet,c("8"=""))
TFAClean$CoachTenure<-as.numeric(as.character(TFAClean$CoachTenure))
TFAClean$ExtraCurric<-as.numeric(as.character(TFAClean$ExtraCurric))
TFAClean$TFANet<-as.numeric(as.character(TFAClean$TFANet))
######Doing Correlations#######

summary(TFAClean)

CoachTenurecor<-cor(TFAClean$OverallSatisfaction,TFAClean$CoachTenure,use="pairwise.complete.obs",method="pearson")
Valuedcor<-cor(TFAClean$OverallSatisfaction,TFAClean$Valued,use="pairwise.complete.obs",method="pearson")
Opinionscor<-cor(TFAClean$OverallSatisfaction,TFAClean$Opinions,use="pairwise.complete.obs",method="pearson")
Proudcor<-cor(TFAClean$OverallSatisfaction,TFAClean$Proud,use="pairwise.complete.obs",method="pearson")
TFAnetcor<-cor(TFAClean$OverallSatisfaction,TFAClean$TFANet,use="pairwise.complete.obs",method="pearson")
OngoingSupportcor<-cor(TFAClean$OverallSatisfaction,TFAClean$OngoingSupport,use="pairwise.complete.obs",method="pearson")
CoachCriticalcor<-cor(TFAClean$OverallSatisfaction,TFAClean$CoachCritical,use="pairwise.complete.obs",method="pearson")
CoachHours<-cor(TFAClean$OverallSatisfaction,TFAClean$CoachHours,use="pairwise.complete.obs",method="pearson")
ExtraCurriccor<-cor(TFAClean$OverallSatisfaction,TFAClean$ExtraCurric,use="pairwise.complete.obs",method="pearson")
Institutecor<-cor(TFAClean$OverallSatisfaction,TFAClean$Institute,use="pairwise.complete.obs",method="pearson")
Recommendcor<-cor(TFAClean$OverallSatisfaction,TFAClean$Recommend,use="pairwise.complete.obs",method="pearson")
Safecor<-cor(TFAClean$OverallSatisfaction,TFAClean$Safe,use="pairwise.complete.obs",method="pearson")

###Computing T-Stats#####
CoachTenureTstat<-((CoachTenurecor*sqrt(5425-2))/(sqrt(1-(CoachTenurecor*CoachTenurecor))))
ValuedTstat<-((Valuedcor*sqrt(5425-2))/(sqrt(1-(Valuedcor*Valuedcor))))
OpinionsTstat<-((Opinionscor*sqrt(5425-2))/(sqrt(1-(Opinionscor*Opinionscor))))
ProudTstat<-((Proudcor*sqrt(5425-2))/(sqrt(1-(Proudcor*Proudcor))))
TFANetTstat<-((TFAnetcor*sqrt(5425-2))/(sqrt(1-(TFAnetcor*TFAnetcor))))
OngoingSupportTstat<-((OngoingSupportcor*sqrt(5425-2))/(sqrt(1-(OngoingSupportcor*OngoingSupportcor))))
CoachCriticalTstat<-((CoachCriticalcor*sqrt(5425-2))/(sqrt(1-(CoachCriticalcor*CoachCriticalcor))))
ExtraCurricTstat<-((ExtraCurriccor*sqrt(5425-2))/(sqrt(1-(ExtraCurriccor*ExtraCurriccor))))
InstituteTstat<-((Institutecor*sqrt(5425-2))/(sqrt(1-(Institutecor*Institutecor))))
RecommendTstat<-((Recommendcor*sqrt(5425-2))/(sqrt(1-(Recommendcor*Recommendcor))))
SafeTstat<-((Safecor*sqrt(5425-2))/(sqrt(1-(Safecor*Safecor))))

####Creating T-Stat Table######
RawTstats<-c(CoachTenureTstat,ValuedTstat,OpinionsTstat,ProudTstat,TFANetTstat,OngoingSupportTstat,
          CoachCriticalTstat,ExtraCurricTstat,InstituteTstat,RecommendTstat,SafeTstat)
Header<-c("CoachTenure","Valued","Opinions","Proud","TFANet","OngoingSupport","CoachCritical",
          "ExtraCurric","Institute","Recommend","Safe")
Tstats<-data.frame(RawTstats,Header)



