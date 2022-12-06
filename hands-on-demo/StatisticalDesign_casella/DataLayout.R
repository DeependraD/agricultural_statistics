#source("c:/WorkHome06/StatDesign/Programs/R/DataLayoutR.txt",print.eval=TRUE)#
#Does anova for DataLayout Data
data<-read.table("c:/WorkHome06/StatDesign/DataSets/DataLayout.txt",sep = "",header=T)
Fertilizer<-as.character(data[,1])
Hybrid<-as.character(data[,2])
Yield<-data[,3]
aovdata <- data.frame(Yield,Fertilizer,Hybrid)
#--------Twoway ANOVA ---------------------
#----------This gives the anova table----------------
summary(aov(Yield~Fertilizer*Hybrid,data=aovdata))
#---------This gives the correct test on Diet with an error message-----------------------
#summary(aov(WtGain~Diet+Error(Tank/Diet),data=aovdata))
