#load all files
FSM<-read.csv("Fall16SM_cleaned.csv")
FSR<-read.csv("Fall16SR_cleaned.csv")
FSEL<-read.csv("Fall16SEL_cleaned.csv")
WSM<-read.csv("WinterSM_cleaned.csv")
WSR<-read.csv("WinterSR_cleaned.csv")
WSEL<-read.csv("WinterSEL_cleaned.csv")

#cleaning STAR information
FSEL<-FSEL[c("F16EL_StudentLocalID","F16EL_LiteracyClassification")]
FSM<-FSM[c("F16Math_StudentLocalID","F16Math_ScreeningCategoryGroupAdjustment")]
FSR<-FSR[c("F16Read_StudentLocalID","F16Read_ScreeningCategoryGroupAdjustment")]
WSM<-WSM[c("Win16Math_StudentLocalID","Win16Math_ScreeningCategoryGroupAdjustment")]
WSR<-WSR[c("Win16Read_StudentLocalID","Win16Read_ScreeningCategoryGroupAdjustment")]
WSEL<-WSEL[c("Win16EL_StudentLocalID","Win16EL_LiteracyClassification")]


#change column names
colnames(FSM)[which(names(FSM)=="F16Math_ScreeningCategoryGroupAdjustment")]<-"Fall Math"
colnames(FSM)[which(names(FSM)=="F16Math_StudentLocalID")]<-"Student ID"
colnames(FSR)[which(names(FSR)=="F16Read_ScreeningCategoryGroupAdjustment")]<-"Fall Reading"
colnames(FSR)[which(names(FSR)=="F16Read_StudentLocalID")]<-"Student ID"
colnames(FSEL)[which(names(FSEL)=="F16EL_StudentLocalID")]<-"Student ID"
colnames(FSEL)[which(names(FSEL)=="F16EL_LiteracyClassification")]<-"Fall EL"
colnames(WSM)[which(names(WSM)=="Win16Math_ScreeningCategoryGroupAdjustment")]<-"Winter Math"
colnames(WSM)[which(names(WSM)=="Win16Math_StudentLocalID")]<-"Student ID"
colnames(WSR)[which(names(WSR)=="Win16Read_ScreeningCategoryGroupAdjustment")]<-"Winter Reading"
colnames(WSR)[which(names(WSR)=="Win16Read_StudentLocalID")]<-"Student ID"
colnames(WSEL)[which(names(WSEL)=="Win16EL_LiteracyClassification")]<-"Winter EL"
colnames(WSEL)[which(names(WSEL)=="Win16EL_StudentLocalID")]<-"Student ID"

#merging
StarbyMP<-merge(FSM,FSR,by="Student ID", all=TRUE)
StarbyMP<-merge(StarbyMP,FSEL,by="Student ID",all=TRUE)
StarbyMP<-merge(StarbyMP,WSM,by="Student ID",all=TRUE)
StarbyMP<-merge(StarbyMP,WSR,by="Student ID",all=TRUE)
StarbyMP<-merge(StarbyMP,WSEL,by="Student ID",all=TRUE)

#change to "No Score"
is.na(StarbyMP)<-0
