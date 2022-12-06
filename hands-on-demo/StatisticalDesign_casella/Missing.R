#source("c:/WorkBeach06/StatDesign/Programs/R/Missing.R",print.eval=TRUE)#
#Does anova for missign data example
data<-read.table("c:/WorkBeach06/StatDesign/DataSets/Missing.txt",sep = "",header=T)
Trt<-as.character(data[,1])
Block<-as.character(data[,2])
Strength<-data[,3]
aovdata <- data.frame(Trt,Block,Strength)
#----------This gives the full anova table---------------------------
#----------Note that default R SS is the SEQUENTIAL SS---------------
summary(aov(Strength~Trt+Block,data=aovdata))
#----------This Fits Treatment before Blocks-------------------------
summary(aov(Strength~Block+Trt,data=aovdata))
#----------This gives both partial Sums of Squares
miss1<-aov(Strength~Block+Trt,data=aovdata)
drop1(miss1,test="F")

