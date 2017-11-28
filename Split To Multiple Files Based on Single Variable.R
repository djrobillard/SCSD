#changing column names to be correct
colnames(BSBags)[which(names(BSBags)=="Student.ID")]<-"Student ID"
colnames(BSBags)[which(names(BSBags)=="Student.Name")]<-"Student Name"

#split to multiple files
BagsSplit <- split(BSBags, BSBags$School)
lapply(names(BagsSplit), function(name) write.csv(BagsSplit[[name]], file = paste('C:/Users/drobil66/Desktop/RFiles/Before School BAGS by Building/',gsub(' ','',name),sep = ''), row.names = F))



