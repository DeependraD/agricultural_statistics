#source("c:/WorkHome06/StatDesign/Programs/R/Sulphur.R",print.eval=TRUE)#
#Does anova for Sulphur data
data<-read.table("c:/WorkHome06/StatDesign/DataSets/Sulphur.txt",sep = "",header=T)
Nitrogen<-as.character(data[,1])
Sulphur<-as.character(data[,2])
Yield<-data[,3]
aovdata <- data.frame(Nitrogen,Sulphur,Yield)
#----------This gives the twoway anova table----------------
summary(aov(Yield~Sulphur*Nitrogen,data=aovdata))
