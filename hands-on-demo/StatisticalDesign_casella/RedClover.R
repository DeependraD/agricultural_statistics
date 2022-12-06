#source("c:/WorkBeach06/StatDesign/Programs/R/RedClover.R",print.eval=TRUE)#
#Does anova for RedClover data
data<-read.table("c:/WorkBeach06/StatDesign/DataSets/RedClover.txt",sep = "",header=T)
Nitrogen<-as.character(data[,1])
Sulphur<-as.character(data[,2])
Yield<-data[,3]
aovdata <- data.frame(Yield,Sulphur,Nitrogen)
#----------This gives the twoway anova table----------------
summary(aov(Yield~Sulphur*Nitrogen,data=aovdata))
#-----------Cell means-------------------------------
aov1<-subset(aovdata,Nitrogen=="0");mean1<-tapply(aov1[,1],aov1[,2],mean)
aov2<-subset(aovdata,Nitrogen=="20");mean2<-tapply(aov2[,1],aov2[,2],mean)
#-----------Interaction Plots------------------------
Sulphur<-c(0,3,6,9)
plot(Sulphur,mean1,pch=19,type="b",ylim=c(4.5,7),ylab="Yield")
par(new=T)
plot(Sulphur,mean2,pch=19,type="b",lty=2,ylim=c(4.5,7),ylab="")
#-----------Polynomial Contrasts-----------------------
PC<-contr.poly(4,score=1:4)		#polynomial contrasts
c(mean1)*PC					#contrast X mean for hCG=N
c(mean2)*PC					#contrast X mean for hCG=Y
num<-(apply(c(mean1)*PC-c(mean2)*PC,2,sum))^2	#numerator of contrast SS
n1<-c(3,3,3,3);n2<-c(3,3,3,3)		#observations per cell
den<-apply(PC^2/n1+PC^2/n2,2,sum)	#denominator of contrast SS
SScon<-num/den;SScon				#Contrast SS