#source("c:/WorkHome06/StatDesign/Programs/R/FishtankR.txt",print.eval=TRUE)#
#Does anova for FishTank Data
data<-read.table("c:/WorkHome06/StatDesign/DataSets/FishTank.txt",sep = "",header=T)
Diet<-as.character(data[,1])
Tank<-as.character(data[,2])
WtGain<-data[,3]
aovdata <- data.frame(WtGain,Diet,Tank)
#--------Nested ANOVA ---------------------
#----------This gives the full anova table, wrong tests----------------
summary(aov(WtGain~Diet+Tank,data=aovdata))
#---------This gives the correct test on Diet with an error message-----------------------
summary(aov(WtGain~Diet+Error(Tank/Diet),data=aovdata))
