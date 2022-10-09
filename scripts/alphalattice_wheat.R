library(agricolae)

## Alpha lattice

#### Wheat data

# import from data directory







#### Oats data

j_oats <- agridat::john.alpha

sp.oats$GY<-as.numeric(sp.oats$GY)
sp.oats$REP<-as.factor(sp.oats$REP)
sp.oats$BLOCK<-as.factor(sp.oats$BLOCK)
sp.oats$ENT<-as.factor(sp.oats$ENT)

wak<- PBIB.test(sp.oats$BLOCK, sp.oats$ENT, sp.oats$REP, sp.oats$GY, method="VC",5, test="lsd", alpha = 0.05, console=FALSE, group= TRUE)
wak

PBIB.test(block,trt,replication,y,k, method=c("REML","ML","VC"), 
test = c("lsd","tukey"), alpha=0.05, console=FALSE, group=TRUE)
