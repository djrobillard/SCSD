#changing grade names from words to numerical
Enroll17BAGS$Grade[grepl (pattern="Third grade",x=Enroll17BAGS$Grade)]<-"3"
as.character(Enroll17BAGS$Grade)