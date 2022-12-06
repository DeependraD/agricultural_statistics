#source("c:/WorkHome06/StatDesign/Programs/R/RehabTimeR.txt",print.eval=TRUE)#
#Does anova for DataLayout Data
data<-read.table("c:/WorkHome06/StatDesign/DataSets/RehabTime.txt",sep = "",header=T)
Condition<-as.character(data[,1])
ConditionCode<-as.character(data[,2])
RehabTime<-data[,3]
aovdata <- data.frame(ConditionCode,RehabTime)
#--------Oneway ANOVA ---------------------
#----------This gives the anova table----------------
summary(aov(RehabTime~ConditionCode,data=aovdata))
#-------Helmert Contrast Matrix
C<-contr.helmert(4, contrasts = TRUE)
#-------Treatment means - Be careful about the ordering
Rmean<-tapply(RehabTime, ConditionCode, mean) 
nmean<-c(6,6,6,6) #observations in each mean
#-------Contrast sums of squares
(Rmean%*%C)^2/apply(C^2/nmean,2,sum)
#-------Another contrast matrix
C2 <- matrix(c(1,-1,0,0,0,0,1,-1,1,1,-1,-1), nrow = 4, ncol=3)
(Rmean%*%C2)^2/apply(C2^2/nmean,2,sum)