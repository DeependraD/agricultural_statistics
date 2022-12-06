#source("c:/WorkHome06/StatDesign/Programs/R/Brownie.R",print.eval=TRUE)#
#Does anova for Brownie Data
data<-read.table("c:/WorkHome06/StatDesign/DataSets/BrownieData.txt",sep = "",header=T)
Brand<-as.character(data[,1])
Power<-as.character(data[,2])
Time<-as.character(data[,3])
Texture<-data[,4]
