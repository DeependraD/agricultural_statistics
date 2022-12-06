#source("c:/WorkBeach06/StatDesign/Programs/R/BIBD.R",print.eval=TRUE)#
#Does anova for missign data example
data<-read.table("c:/WorkBeach06/StatDesign/DataSets/BIBD.txt",sep = "",header=T)
Trt<-as.character(data[,1])
Block<-as.character(data[,2])
Y<-data[,3]
aovdata <- data.frame(Trt,Block,Y)
#----------This gives the anova table---------------------------
summary(aov(Y~Block+Trt,data=aovdata))
drop1(aov(Y~Block+Trt,data=aovdata),test="F")

