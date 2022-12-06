#source("c:/WorkBeach06/StatDesign/Programs/R/FishTissue.R",print.eval=TRUE)#
#Does anova for FishTissue Data
data<-read.table("c:/WorkBeach06/StatDesign/DataSets/FishTissueMass.txt",sep = "",header=T)
Tissue<-as.character(data[,1])
hCG<-as.character(data[,2])
Y<-log(data[,3])
aovdata <- data.frame(Y,Tissue,hCG)
#----------This gives the anova table----------------
summary(aov(Y~Tissue*hCG,data=aovdata))
#-----------Cell means-------------------------------
aov1<-subset(aovdata,hCG=="N");mean1<-tapply(aov1[,1],aov1[,2],mean)
aov2<-subset(aovdata,hCG=="Y");mean2<-tapply(aov2[,1],aov2[,2],mean)
#-----------Interaction Plots------------------------
Tissue<-c(50,100,150,200)
plot(Tissue,mean1,pch=19,type="b",ylim=c(2.5,5.5),ylab="Log Expression Level")
par(new=T)
plot(Tissue,mean2,pch=19,type="b",lty=2,ylim=c(2.5,5.5),ylab="n")
#-----------Polynomial Contrasts-----------------------
PC<-contr.poly(4,score=1:4)		#polynomial contrasts
c(mean1)*PC					#contrast X mean for hCG=N
c(mean2)*PC					#contrast X mean for hCG=Y
num<-(apply(c(mean1)*PC-c(mean2)*PC,2,sum))^2	#numerator of contrast SS
n1<-c(2,1,2,1);n2<-c(1,2,1,2)		#observations per cell
den<-apply(PC^2/n1+PC^2/n2,2,sum)	#denominator of contrast SS
SScon<-num/den				#Contrast SS
