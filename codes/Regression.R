Y=NULL
reserve=NULL
condition=NULL
rsq=NULL
FuncReg=NULL

PR=NULL
value=NULL;
condition=NULL;
reserve=NULL;
seller=NULL;
bidder=NULL;
early=NULL;
jump=NULL;
open=NULL;
current=NULL
########
r= -2##
########
p1=0;p2=0;n1=0;n2=0
for(i in 1:length(EBAY.training)){
  if(EBAY.training[[i]]$Value==1){p1=p1+max(EBAY.training[[i]]$Price);n1=n1+1}
  if(EBAY.training[[i]]$Value==0){p2=p2+max(EBAY.training[[i]]$Price);n2=n2+1}
}
p1=p1/n1
p2=p2/n2

for(i in 1:(NT.grid)){
  t=rep(0,length(EBAY.training))
  for(j in 1:length(EBAY.training)){
    Y[j] = pdf.1.beta[[j]][i]*length(EBAY.training[[j]]$Time)
    value[j]<-EBAY.training[[j]]$Value
    #if(value[j]==1) mean.N=mean.N.1
    #else mean.N=mean.N.2
    #Y[j] = Y[j]-smoothv.diff[i]*mean.N
    currentnumber<-CurrentNumber.last[[j]][i]
    current.time=(i)/NT.grid*(END-START)+START
    reserve[j]=EBAY.training[[j]]$Reserve
    condition[j]=EBAY.training[[j]]$Condition
    for(k in 1:currentnumber){
      t[j]=t[j]+exp((current.time-EBAY.training[[j]]$Time[k])*r)
    }
    pcurrent=StepCheck(EBAY.training[[j]]$Time,EBAY.training[[j]]$Price,((i)/NT.grid*(END-START)+START))
    if(EBAY.training[[j]]$Value==1){WTP=p1}
    else{WTP=p2}
    PR[j]=WTP-pcurrent
    
    value[j]<-EBAY.training[[j]]$Value
    condition[j]<-EBAY.training[[j]]$Condition
    reserve[j]<-EBAY.training[[j]]$Reserve
    seller[j]<-EBAY.training[[j]]$Seller
    bidder[j]<-StepCheck(EBAY.training[[j]]$Time,EBAY.training[[j]]$Bidder,((i)/NT.grid*(END-START)+START))
    early[j]<-EBAY.training[[j]]$Early
    jump[j]<-StepCheck(EBAY.training[[j]]$Time,EBAY.training[[j]]$Jump,((i)/NT.grid*(END-START)+START))
    open[j]<-EBAY.training[[j]]$Price[1]
    current[j]<-CurrentNumber.last[[j]][i]
  }
 # allv=data.frame(Self.Exciting=t,  Price.Relative=PR,  Value=value,
                 # Condition=condition,  Reserve=reserve, Seller.Rating=seller,
                 # Bidder.Rating=bidder, Early.Bidding=early, Jump.Bidding=jump,
                 # Opening.Price=open)
   allv=data.frame(Self.Exciting=t,  Price.Relative=PR, Value=value,         
                   Condition=condition,Early.Bidding=early,
                   Jump.Bidding=jump, Opening.Price=open)
#   
#    allv=data.frame(Self.Exciting=t,  Price.Relative=PR,  Value=value,
#                   Condition=condition)
  FuncReg[[i]] = lm(Y~.-1,data=allv,na.action=na.exclude)
  #FuncReg[[i]] = lm(Y~t+PR+value+condition+early+jump+open+t*PR+t*value+t*condition+t*early+t*jump+t*open-1)
}


#############################################################################
par(mfrow=c(5,2))
betaP=matrix(0,length(FuncReg),length(FuncReg[[1]]$coef))
rsq=matrix(0,length(FuncReg),1)
for(i in 1: length(FuncReg)){
  m=length(summary(FuncReg[[i]])$coefficients[,4])
  betaP[i,1:m]=summary(FuncReg[[i]])$coefficients[,4]
  rsq[i,]=summary(FuncReg[[i]])$adj.r.squared
}
bataP=as.matrix(betaP)
timeaxis=(c(1:(NT.grid)))/NT.grid*(END-START)+START
layout(matrix(c(1,2,3,4,5,6,0,7,0), 3, 3, byrow = TRUE))
for(i in 2: dim(betaP)[2]){plot(timeaxis, betaP[,i],type="l")}#,main=colnames(allv)[i],xlab="time",ylab="p-value")}
i=1
plot(timeaxis, betaP[,i],type="l",main=colnames(allv)[i],xlab="time",ylab="p-value")

par(mfrow=c(1,1))
bataP=as.matrix(betaP)
timeaxis=(c(1:(NT.grid)))/NT.grid*(END-START)+START
plot(timeaxis, betaP[,1],type="l",ylim=c(0,1))
for(i in 2: dim(betaP)[2]){lines(timeaxis, betaP[,i])}


#Plot time series of beta, input data frame: ALLVar
#############################################################################

par(mfrow=c(1,1))
plot(timeaxis, rsq,cex=0.5,ylab="adj.R-square")
#############################################################################
par(mfrow=c(2,3))
beta=matrix(0,length(FuncReg),length(FuncReg[[i]]$coef))
rsq=matrix(0,length(FuncReg),1)
for(i in 1: length(FuncReg)){
  beta[i,]=FuncReg[[i]]$coef
  rsq[i,]=summary(FuncReg[[i]])$adj.r.squared
}
bata=as.matrix(beta)
nanumber=0
for(i in 1:dim(beta)[1]){
  for(j in 1:dim(beta)[2]){
    if(is.na(beta[i,j]) == TRUE){
      beta[i,j]=0
      nanumber=nanumber+1
    }
  }
}

beta5=matrix(0,dim(beta)[1]/200,dim(beta)[2])
beta95=matrix(0,dim(beta)[1]/200,dim(beta)[2])
for(j in 1:dim(beta)[2]){    #Leave the last point ignorned for too large
  for(i in 1:dim(beta)[1]){
    if((i%%200)==0){
      beta5[i/200,j]=beta[i,j]+summary(FuncReg[[i]])$coefficient[j,2]*qt(0.05,summary(FuncReg[[i]])$df[2])
      beta95[i/200,j]=beta[i,j]+summary(FuncReg[[i]])$coefficient[j,2]*qt(0.95,summary(FuncReg[[i]])$df[2])
    }  
  }
}
timeconf=(seq(1:(dim(beta)[1]/200))*200)/NT.grid*(END-START)+START


first=1
last=4000-11
#layout(matrix(c(1,2,3,4,5,6,0,7,0), 3, 3, byrow = TRUE))
# page setting for plotting 10 variables
par(mfrow=c(2,5),mar=c(4,2,2,2))
for(i in 1: dim(beta)[2]){
 plot(timeaxis[first:last], beta[,i][first:last],type="l",main=colnames(allv)[i],xlab="time",ylab="beta",cex.main=1)
 points(timeconf,beta5[,i],col="red",cex=0.7)
 points(timeconf,beta95[,i],col="red",cex=0.7)
 abline(h=0,col="grey",lty=2)
  # plot(timeaxis, betaP[,i],type="l",main=colnames(allv)[i],xlab="time",ylab="p-value",cex.main=1,ylim=c(0,1))
}
par(mfrow=c(5,2),mar=c(4,2,2,2))
for(i in 1: dim(beta)[2]){
 plot(timeaxis, betaP[,i],type="l",main=colnames(allv)[i],xlab="time",ylab="p-value",cex.main=1,ylim=c(0,1))
  abline(h=0.1,col="grey",lty=2)
}


