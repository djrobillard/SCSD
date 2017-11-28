attach(indexclean)
index2<-plot(X3.8.Math,pct_fam_poverty,main="High Needs Classification by Math Proficiency Level",
             xlab="High Need Yes or No",ylab="2016 3-8 Math Test Proficiency",pch=19)

library(MASS)

index4<-select(indexclean,X3.8.ELA,high_need_classification)


barplot(index4,main="3-8 Test Scores and HNC",xlab = "HNC")