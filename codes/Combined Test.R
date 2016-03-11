redo = TRUE
while(redo == TRUE){
n=round(length(EBAY)*.7)
a1=sample(c(1:length(EBAY)),n)
a2=rep(0,(length(EBAY)-n))
k=1
for(i in 1:length(EBAY)){
  skip=0
  for(j in 1: length(a1)){
    if(i==a1[j]){
      skip=1
      break
    }
  }
  if(skip==0){
    a2[k]=i
    k=k+1
  }
}
EBAY.training=NULL
for(i in 1:length(a1)){
  EBAY.training[[i]]=EBAY[[a1[i]]]
}
EBAY.testing=NULL
for(i in 1:length(a2)){
  EBAY.testing[[i]]=EBAY[[a2[i]]]
}



###############################################################################################

library(optimx)
library(BB)
library(ucminf)
library(Rcgmin)
library(quadprog)
library(Rvmmin)
library(minqa)
######################
START=5
K=2
NT.grid=8000
END=7  #For the time being, the end could not be changed!!!
## Beta fitting
######################
smoothF.1=NULL
pdf.1.beta=NULL
Fpar=matrix(0,length(EBAY.training),2)
for(i in 1:length(EBAY.training)){
  
{  ## Obtain the parameter
  
  lastday=StepCount(EBAY.training[[i]]$Time,EBAY.training[[i]]$Time, START)
  if(lastday==length(EBAY.training[[i]]$Time)){
    Fpar[i,]=c(9999,9999)
    smoothF.1[[i]]=rep(0,NT.grid)
    pdf.1.beta[[i]]=rep(0,NT.grid)
    next
  }
  
  #  To handle the last term being inf.
  deltaT=max(0.000001,(EBAY.training[[i]]$Time[length(EBAY.training[[i]]$Time)]-EBAY.training[[i]]$Time[length(EBAY.training[[i]]$Time)-1]))
  x1= min(EBAY.training[[i]]$Time[length(EBAY.training[[i]]$Time)]+deltaT,END)
  xx= c(EBAY.training[[i]]$Time[lastday:length(EBAY.training[[i]]$Time)],x1)
  yy= c(EBAY.training[[i]]$F[lastday:length(EBAY.training[[i]]$F)])
  y1= 2*yy[length(yy)]-yy[length(yy)-1]
  yy=c(yy,y1)
  x= (xx-START)/(END-START)
  y= (yy-min(yy))/(max(yy)-min(yy))
  x[1]=0
  
  
  DistB=function(a){
    dist=sum((y-pbeta(x,a[1],a[2]))^2)+sum((x-qbeta(y,a[1],a[2]))^2)
  }  
  DistA=function(a){
    (mean(x)-a[1]/(a[1]+a[2]))^2+(var(x)-a[1]*a[2]/((a[1]+a[2])^2*(a[1]+a[2]+1)))^2 
  }
  
  para0 =unlist(optimx(c(1,0), DistA, method="BFGS")$par)
  para0=abs(para0)
  para = optimx(para0, DistB , method="BFGS")
  if (para$conv == 9999){para = optimx(para0, DistB , method="Nelder-Mead")}
  Fpar[i,] = unlist(para$par)
}


smoothF.1.temp=NULL
pdf.1.beta.temp=NULL
for(j in 1:NT.grid){
  integrand=function(u){u^(Fpar[i,1]-1)*(1-u)^(Fpar[i,2]-1)}
  smoothF.1.temp=cbind(smoothF.1.temp,integrate(integrand, 0, j/(NT.grid+1))$value/beta(Fpar[i,1],Fpar[i,2]))
  pdf.1.beta.temp = cbind(pdf.1.beta.temp, integrand(j/(NT.grid+1))/beta(Fpar[i,1],Fpar[i,2]))
}
# smoothF.1[[i]]=smoothF.1.temp * (max(yy)-min(yy)) + min(yy)
# pdf.1.beta[[i]]=pdf.1.beta.temp * (max(yy)-min(yy))/(END-START)

## Change to last day, edited Jan 2013
smoothF.1[[i]]<-(smoothF.1.temp * (max(yy)-min(yy)) + min(yy))[4001:8000]
pdf.1.beta[[i]]<-(pdf.1.beta.temp * (max(yy)-min(yy))/(END-START))[4001:8000]

# par(mfrow=c(1,2))
# xaxis=seq(START+1,END,len=NT.grid/2)
# plot(xaxis, smoothF.1[[i]],type="l",xlim=c(START,END),ylim=c(0,1))
# points(EBAY.training[[i]]$Time,EBAY.training[[i]]$F,cex=0.5)
# points(x*(END-START)+START,y* (max(yy)-min(yy)) + min(yy),col="red")
# plot(xaxis, pdf.1.beta[[i]],cex=0.4)

#Print out the plots of Fitting and lambda
#  par(mfrow=c(1,2))
#  xaxis=seq(START,END,len=NT.grid)
#  plot(xaxis, smoothF.1[[i]],type="l",xlim=c(START,END),ylim=c(0,1))
#  points(EBAY.training[[i]]$Time,EBAY.training[[i]]$F,cex=0.5)
#  points(x*(END-START)+START,y* (max(yy)-min(yy)) + min(yy),col="red")
#  plot(xaxis, pdf.1.beta[[i]],cex=0.4)

}

# par(mfrow=c(1,2))
# for(i in 1:length(EBAY.training)){
	# xxx <-density(EBAY.training[[i]]$Time[-1],bw="sj",kernel="gaussian",from=0,to=7,n=28000)$x
	# yyy <- density(EBAY.training[[i]]$Time[-1],bw="sj",kernel="gaussian",from=0,to=7,n=28000)$y
	# pdf.1.beta[[i]] <- (yyy/sum(yyy))[24001:28000]*4000
	# plot(xxx[24001:28000],pdf.1.beta[[i]] ,type="l")
    # zzz <- (cumsum(yyy)/sum(yyy))[24001:28000]
	# plot(xxx[24001:28000],zzz,type = "l")
    # points(EBAY.training[[i]]$Time,EBAY.training[[i]]$F)
# }

#############################
NT.grid <- NT.grid/2
START <- START + 1
#############################
## CurrentNumber Count
{
  CurrentNumber.last=NULL
  for( i in 1:length(EBAY.training)){
    currentnumber.last.temp=rep(0,NT.grid)
    for( j in 1:(NT.grid)){
      currentnumber.last.temp[j]=length(EBAY.training[[i]]$Time[EBAY.training[[i]]$Time<((j)/NT.grid*(END-START)+START)])
    }
    CurrentNumber.last[[i]]=currentnumber.last.temp
  }
}

###############################################################################################
PriceInc1=NULL
PriceInc2=NULL
PriceInc.temp=NULL
nv1=0
nv2=0
for(i in 1:length(EBAY.training)){

  if(EBAY.training[[i]]$Value==1 && length(EBAY.training[[i]]$Time)>= 3){
    nv1=nv1+1
    PriceInc.temp=0
    k=StepCount(EBAY.training[[i]]$Time,EBAY.training[[i]]$Time,6)
    if(k > 1 && length(EBAY.training[[i]]$Price) != k){
    for(j in k:(length(EBAY.training[[i]]$Price)-1)){
      Inc=EBAY.training[[i]]$Price[j+1]-EBAY.training[[i]]$Price[j]
      EInc=increment(EBAY.training[[i]]$Price[j])
      PriceInc.temp=c(PriceInc.temp, Inc)
    }
    }
    if(k==1){
      for(j in 2:(length(EBAY.training[[i]]$Price)-1)){
        Inc=EBAY.training[[i]]$Price[j+1]-EBAY.training[[i]]$Price[j]
        EInc=increment(EBAY.training[[i]]$Price[j])
        PriceInc.temp=c(PriceInc.temp, Inc)
      }
    }
    PriceInc1[[nv1]]=PriceInc.temp
}
  
  if(EBAY.training[[i]]$Value==0 && length(EBAY.training[[i]]$Time)>= 3){
    nv2=nv2+1
    PriceInc.temp=0
    k=StepCount(EBAY.training[[i]]$Time,EBAY.training[[i]]$Time,6)
    if(k > 1 && length(EBAY.training[[i]]$Price) != k){
      for(j in k:(length(EBAY.training[[i]]$Price)-1)){
        Inc=EBAY.training[[i]]$Price[j+1]-EBAY.training[[i]]$Price[j]
        EInc=increment(EBAY.training[[i]]$Price[j])
        PriceInc.temp=c(PriceInc.temp, Inc)
      }
    }
    if(k==1){
      for(j in 2:(length(EBAY.training[[i]]$Price)-1)){
        Inc=EBAY.training[[i]]$Price[j+1]-EBAY.training[[i]]$Price[j]
        EInc=increment(EBAY.training[[i]]$Price[j])
        PriceInc.temp=c(PriceInc.temp, Inc)
      }
    }
  PriceInc2[[nv2]]=PriceInc.temp
}
}
IncALL1=NULL
IncALL2=NULL
PriceALL=NULL
TimeALL=NULL
TimeInc=NULL
for(i in 1: nv1){ 
  if(length(PriceInc1[[i]])>1){
  for(j in 2:length(PriceInc1[[i]])){  #Since the first 2 elements are zero
    IncALL1=c(IncALL1,PriceInc1[[i]][j])
  }
  }
}
for(i in 1: nv2){ 
  if(length(PriceInc2[[i]])>1){
  for(j in 2:length(PriceInc2[[i]])){  #Since the first 2 elements are zero
    IncALL2=c(IncALL2,PriceInc2[[i]][j])
  }
  }
}
###############################################################################################################
PriceInc1=NULL
IncTime1=NULL
PriceInc2=NULL
IncTime2=NULL
PriceInc.temp=NULL
IncTime.temp=NULL
nv1=0
nv2=0
for(i in 1:length(EBAY.training)){
  
  if(EBAY.training[[i]]$Value==1 && length(EBAY.training[[i]]$Time)>= 3){
    nv1=nv1+1
    PriceInc.temp=0
    IncTime.temp=0
    k=StepCount(EBAY.training[[i]]$Time,EBAY.training[[i]]$Time,6)
    if(k > 1 && length(EBAY.training[[i]]$Price) != k){
      for(j in k:(length(EBAY.training[[i]]$Price)-1)){
        Inc=EBAY.training[[i]]$Price[j+1]-EBAY.training[[i]]$Price[j]
        IncTime.temp=c(IncTime.temp,EBAY.training[[i]]$Time[j+1])
        PriceInc.temp=c(PriceInc.temp, Inc)
      }
    }
    if(k==1){
      for(j in 2:(length(EBAY.training[[i]]$Price)-1)){
        Inc=EBAY.training[[i]]$Price[j+1]-EBAY.training[[i]]$Price[j]
        IncTime.temp=c(IncTime.temp,EBAY.training[[i]]$Time[j+1])
        PriceInc.temp=c(PriceInc.temp, Inc)
      }
    }
    PriceInc1[[nv1]]=PriceInc.temp
    IncTime1[[nv1]]=IncTime.temp
  }
  
  if(EBAY.training[[i]]$Value==0 && length(EBAY.training[[i]]$Time)>= 3){
    nv2=nv2+1
    PriceInc.temp=0
    IncTime.temp=0
    k=StepCount(EBAY.training[[i]]$Time,EBAY.training[[i]]$Time,6)
    if(k > 1 && length(EBAY.training[[i]]$Price) != k){
      for(j in k:(length(EBAY.training[[i]]$Price)-1)){
        Inc=EBAY.training[[i]]$Price[j+1]-EBAY.training[[i]]$Price[j]
        IncTime.temp=c(IncTime.temp,EBAY.training[[i]]$Time[j+1])
        PriceInc.temp=c(PriceInc.temp, Inc)
      }
    }
    if(k==1){
      for(j in 2:(length(EBAY.training[[i]]$Price)-1)){
        Inc=EBAY.training[[i]]$Price[j+1]-EBAY.training[[i]]$Price[j]
        IncTime.temp=c(IncTime.temp,EBAY.training[[i]]$Time[j+1])
        PriceInc.temp=c(PriceInc.temp, Inc)
      }
    }
    PriceInc2[[nv2]]=PriceInc.temp
    IncTime2[[nv2]]=IncTime.temp
  }
}
IncALL1=NULL
IncALL2=NULL
TimeALL1=NULL
TimeALL2=NULL
PriceALL=NULL
TimeALL=NULL
TimeInc=NULL
for(i in 1: nv1){ 
  if(length(PriceInc1[[i]])>1){
    for(j in 2:length(PriceInc1[[i]])){  #Since the first 2 elements are zero
      IncALL1=c(IncALL1,PriceInc1[[i]][j])
      TimeALL1=c(TimeALL1,IncTime1[[i]][j])
    }
  }
}
for(i in 1: nv2){ 
  if(length(PriceInc2[[i]])>1){
    for(j in 2:length(PriceInc2[[i]])){  #Since the first 2 elements are zero
      IncALL2=c(IncALL2,PriceInc2[[i]][j])
      TimeALL2=c(TimeALL2,IncTime2[[i]][j])
    }
  }
}

Inc1.class=NULL
class.temp=NULL
for(i in 1:10){
  for(j in 1:length(TimeALL1)){
    if(TimeALL1[j]>6+(i-1)/10 && TimeALL1[j]<6+(i)/10) class.temp=c(class.temp,IncALL1[j])
  }
  Inc1.class[[i]]=class.temp
}

Inc2.class=NULL
class.temp=NULL
for(i in 1:10){
  for(j in 1:length(TimeALL2)){
    if(TimeALL2[j]>6+(i-1)/10 && TimeALL2[j]<6+(i)/10) class.temp=c(class.temp,IncALL2[j])
  }
  Inc2.class[[i]]=class.temp
}
###############################################################################################

###############################################################################################
{
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
betaP=matrix(0,length(FuncReg),length(FuncReg[[i]]$coef))
rsq=matrix(0,length(FuncReg),1)
for(i in 1: length(FuncReg)){
  m=length(summary(FuncReg[[i]])$coefficients[,4])
  betaP[i,1:m]=summary(FuncReg[[i]])$coefficients[,4]
  rsq[i,]=summary(FuncReg[[i]])$adj.r.squared
}
bataP=as.matrix(betaP)
timeaxis=(c(1:(NT.grid)))/NT.grid*(END-START)+START
layout(matrix(c(1,2,3,4,5,6,0,7,0), 3, 3, byrow = TRUE))
for(i in 2: dim(betaP)[2]){plot(timeaxis, betaP[,i],type="l",main=colnames(allv)[i],xlab="time",ylab="p-value")}
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



}


library(fda)
SPLINE=function(x,y){
  
  knots <- c(0,1,2,3,4,5,6,6.25,6.5,6.75,6.8125,6.875,6.9375,7)
  numk<-length(knots)
  plotpoints <- seq(7/NT.grid,7,7/NT.grid)
  numplot <- length(plotpoints)
  norder <- 5
  nbasis <- numk + 2
  lambda <- 0.1
  numauct<-length(EBAY.training)
  wbasis <- create.bspline.basis(rangeval=c(0,7),nbasis=nbasis, norder=5)
  Wfd <- list(0); length(Wfd) <- numauct
  bids=rep(0,length(knots))
  for(j in 1:length(knots)) bids[j]=StepCheck(x,y,knots[j])
  Wfd <- Data2fd(knots,log(bids),basisobj=wbasis)
  
  growfdPar <- fdPar(Wfd, 3, lambda)
  mss <-smooth.basis(knots,bids,growfdPar,wtvec=rep(1,length(knots)))
  xfd = mss$fd
  splineF=NULL
  splinef=NULL
  splineF=eval.fd(plotpoints,xfd)
  splinef=eval.fd(plotpoints,xfd,Lfd=1)
  return(cbind(splineF,splinef))
}
##############################################################################
## Follow previous Formats
######################\
MAPE1st=NULL
MAPE2ndtoend=NULL
for(kk in 1:6){
  NT.all=c(7*24/2,7*24,7*24*2,7*24*4,7*24*4*3,7*24*4*3*5)
  NT.grid=NT.all[kk]
START=0
END=7  #For the time being, the end could not be changed!!!
## Beta fitting
smoothF.1=NULL
pdf.1.beta=NULL
for(i in 1:length(EBAY.training)){
  smoothF.1[[i]]=SPLINE(EBAY.training[[i]]$Time,EBAY.training[[i]]$Price)[,1]
  pdf.1.beta[[i]]=SPLINE(EBAY.training[[i]]$Time,EBAY.training[[i]]$Price)[,2]
}
smoothF.2=NULL
pdf.2.beta=NULL
for(i in 1:length(EBAY.testing)){
  smoothF.2[[i]]=SPLINE(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$Price)[,1]
  pdf.2.beta[[i]]=SPLINE(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$Price)[,2]
}


##############################################################################
## Change Static into Time Varing Form
############################################################################


condition=NULL;
reserve=NULL;
open=NULL;
seller=NULL;
value=NULL;
early=NULL
jump=numeric(length(EBAY.training))
for(j in 1:length(EBAY.training)){
  if(max(EBAY.training[[j]]$Jump)>=1) jump[j]=1
  else jump[j]=0
  condition[j]=EBAY.training[[j]]$Condition
  reserve[j]=EBAY.training[[j]]$Reserve
  open[j]=EBAY.training[[j]]$Price[1]
  seller[j]=EBAY.training[[j]]$Seller    
  value[j]=EBAY.training[[j]]$Value
  early[j]=EBAY.training[[j]]$Early
}


Y=NULL;Y1=NULL
condition.v=NULL;condition.v1=NULL
reserve.v=NULL;reserve.v1=NULL
open.v=NULL;open.v1=NULL
seller.v=NULL;seller.v1=NULL
value.v=NULL;value.v1=NULL
early.v=NULL;early.v1=NULL
jump.v=NULL;jump.v1=NULL
for(i in 1:NT.grid){
  for(j in 1:length(EBAY.training)){
    Y[j] = smoothF.1[[j]][i]
    Y1[j]= pdf.1.beta[[j]][i]
  }
  condition.v[i]=lm(Y~condition)$coef[2]
  reserve.v[i]=lm(Y~reserve)$coef[2]
  open.v[i]=lm(Y~open)$coef[2]
  seller.v[i]=lm(Y~seller)$coef[2]
  value.v[i]=lm(Y~value)$coef[2]
  early.v[i]=lm(Y~early)$coef[2]
  jump.v[i]=lm(Y~jump)$coef[2]
  condition.v1[i]=lm(Y1~condition)$coef[2]
  reserve.v1[i]=lm(Y1~reserve)$coef[2]
  open.v1[i]=lm(Y1~open)$coef[2]
  seller.v1[i]=lm(Y1~seller)$coef[2]
  value.v1[i]=lm(Y1~value)$coef[2]
  early.v1[i]=lm(Y1~early)$coef[2]
  jump.v1[i]=lm(Y1~jump)$coef[2]
  
}

{
  CurrentNumber.last=NULL
  for( i in 1:length(EBAY.training)){
    currentnumber.last.temp=rep(0,NT.grid)
    for( j in 1:NT.grid){
      currentnumber.last.temp[j]=length(EBAY.training[[i]]$Time[EBAY.training[[i]]$Time<(j/NT.grid*(END-START)+START)])
    }
    CurrentNumber.last[[i]]=currentnumber.last.temp
  }
}

##########################################################################
## Estimate first Derivative
##########################################################################

Y=NULL;
t1=NULL;t2=NULL;condition=NULL;reserve=NULL;open=NULL;seller=NULL;
bidder=NULL;early=NULL;jump=NULL;currentnumber=NULL;value=NULL
fitD=NULL;

for(i in (NT.grid-3):NT.grid){
  for(j in 1:length(EBAY.training)){
    Y = c(Y,pdf.1.beta[[j]][i])
    t1=c(t1,i/NT.grid*(END-START)+START)
    t2=c(t2,(i/NT.grid*(END-START)+START)^2) 
    condition=c(condition,EBAY.training[[j]]$Condition*condition.v1[i])
    reserve=c(reserve,EBAY.training[[j]]$Reserve*reserve.v1[i])
    open=c(open,EBAY.training[[j]]$Price[1] )*open.v1[i]
    seller=c(seller,EBAY.training[[j]]$Seller*seller.v1[i] )
    value=c(value,EBAY.training[[j]]$Value*value.v1[i])
    bidder=c(bidder,StepCheck(EBAY.training[[j]]$Time,EBAY.training[[j]]$Bidder,(i/NT.grid*(END-START)+START)))
    early=c(early,EBAY.training[[j]]$Early*early.v1[i])
    jumpexist=StepCheck(EBAY.training[[j]]$Time,EBAY.training[[j]]$Jump,(i/NT.grid*(END-START)+START))
    jump=c(jump,(jumpexist>=1)*jump.v1[i])
    currentnumber=c(currentnumber,CurrentNumber.last[[j]][i])    
  }
  #fitD[[i]] = lm(Y~t1+t2+condition+reserve+open+seller+value+bidder+early+jump+currentnumber)
}  

fitD=lm(Y~t1+t2+condition+reserve+open+seller+value+bidder+early+jump+currentnumber)

U=matrix(0,4,length(EBAY.training))
for(i in (NT.grid-3):NT.grid){
  for(j in 1:length(EBAY.training)){
    U[i-(NT.grid-3),j]=fitD$residuals[(i-(NT.grid-3))*length(EBAY.training)+j]
  }
}

u.t=NULL
u.1=NULL
for(i in 2:4){
  u.t=c(u.t,as.vector(U[i,]))
  u.1=c(u.1,as.vector(U[i-1,]))
}
eta=summary(lm(u.t~u.1-1))$coefficients[1] # parameter of AR model

###########################################################################
## Forecast using Smoothed Price and Fitted Dynamics
###########################################################################
pred=NULL
real1st=NULL
real2nd=NULL          
Forecast=NULL
Y=NULL;
Forecast=NULL

t1=NULL;t2=NULL;condition=NULL;reserve=NULL;open=NULL;seller=NULL;value=NULL
bidder=NULL;early=NULL;jump=NULL;Dy=NULL;Y.auto=NULL;currentnumber=NULL;
for(i in (NT.grid-1):(NT.grid-1)){
  for(j in 1:length(EBAY.training)){
    Y[j] = smoothF.1[[j]][i+1]
    # Y[j] = StepCheck(EBAY.training[[j]]$Time,EBAY.training[[j]]$Price,((i+1)/NT.grid*(END-START)+START))
    condition[j]=EBAY.training[[j]]$Condition*condition.v[i+1]
    reserve[j]=EBAY.training[[j]]$Reserve*reserve.v[i+1]
    open[j]=EBAY.training[[j]]$Price[1]*open.v[i+1]
    seller[j]=EBAY.training[[j]]$Seller*seller.v[i+1]
    value[j]=EBAY.training[[j]]$Value*value.v[i+1]
    bidder[j]<-StepCheck(EBAY.training[[j]]$Time,EBAY.training[[j]]$Bidder,((i)/NT.grid*(END-START)+START))
    early[j]<-EBAY.training[[j]]$Early*early.v[i+1]
    jumpexist<-StepCheck(EBAY.training[[j]]$Time,EBAY.training[[j]]$Jump,((i)/NT.grid*(END-START)+START))
    jump[j]=(jumpexist>=1)*jump.v[i+1]
    currentnumber[j]=CurrentNumber.last[[j]][i]
    Dy[j]=pdf.1.beta[[j]][i+1]
    Y.auto[j] = smoothF.1[[j]][i]
    # Y.auto[j]=StepCheck(EBAY.training[[j]]$Time,EBAY.training[[j]]$Price,(i/NT.grid*(END-START)+START))
  }
  Forecast[[i-(NT.grid-2)]] = lm(Y~condition+reserve+open+seller+value+bidder+early+jump+currentnumber+Dy+Y.auto)
}

predstep=NULL
real1st=NULL
real2nd=NULL           #last two are from day 6.5



{
  CurrentNumber.2.last=NULL
  for( i in 1:length(EBAY.testing)){
    currentnumber.last.temp=rep(0,NT.grid)
    for( j in 1:NT.grid){
      currentnumber.last.temp[j]=length(EBAY.training[[i]]$Time[EBAY.training[[i]]$Time<(j/NT.grid*(END-START)+START)])
    }
    CurrentNumber.2.last[[i]]=currentnumber.last.temp
  }
}
for(i in (NT.grid-1):(NT.grid-1)){ 
  Y=NULL;condition=NULL;reserve=NULL;open=NULL;seller=NULL;
  bidder=NULL;early=NULL;jump=NULL;currentnumber=NULL;
  Dy=NULL;Y.auto=NULL;
  fity=NULL
  k=1
  pred.temp=NULL
  real1st.temp=NULL
  real2nd.temp=NULL
  for(j in 1:length(EBAY.testing)){ 
    ptm <- proc.time()    
    new.d=data.frame(t1=(i+1)/NT.grid*7,
                     t2=((i+1)/NT.grid*7)^2,
                     condition=EBAY.testing[[j]]$Condition*condition.v1[i+1],
                     reserve=EBAY.testing[[j]]$Reserve*reserve.v1[i+1],
                     open=EBAY.testing[[j]]$Price[1]*open.v1[i+1],
                     seller=EBAY.testing[[j]]$Seller*seller.v1[i+1],
                     value=EBAY.testing[[j]]$Value*value.v1[i+1],
                     bidder=StepCheck(EBAY.testing[[j]]$Time,EBAY.testing[[j]]$Bidder,(i/NT.grid*(END-START)+START)),
                     early=EBAY.testing[[j]]$Early*early.v1[i+1],
                     jumpexist=StepCheck(EBAY.testing[[j]]$Time,EBAY.testing[[j]]$Jump,(i/NT.grid*(END-START)+START)),
                     jump=(jumpexist>=1)*jump.v1[i+1],
                     currentnumber=CurrentNumber.2.last[[j]][i]
    )
    new.d2=data.frame(t1=(i)/NT.grid*7,
                      t2=((i)/NT.grid*7)^2,
                      condition=EBAY.testing[[j]]$Condition*condition.v1[i],
                      reserve=EBAY.testing[[j]]$Reserve*reserve.v1[i],
                      open=EBAY.testing[[j]]$Price[1]*open.v1[i],
                      seller=EBAY.testing[[j]]$Seller*seller.v1[i],
                      value=EBAY.testing[[j]]$Value*value.v1[i],
                      bidder=StepCheck(EBAY.testing[[j]]$Time,EBAY.testing[[j]]$Bidder,((i-1)/NT.grid*(END-START)+START)),
                      early=EBAY.testing[[j]]$Early*early.v1[i],
                      jumpexist=StepCheck(EBAY.testing[[j]]$Time,EBAY.testing[[j]]$Jump,((i-1)/NT.grid*(END-START)+START)),
                      jump=(jumpexist>=1)*jump.v1[i],
                      currentnumber=CurrentNumber.2.last[[j]][i-1]
    )
    fitf=SPLINE(EBAY.testing[[j]]$Time[EBAY.testing[[j]]$Time<=i/NT.grid*(END-START)+START],
                EBAY.testing[[j]]$Price[EBAY.testing[[j]]$Time<=i/NT.grid*(END-START)+START])[i,2]
    
    Dy1=predict(fitD,new.d)+eta*(fitf-predict(fitD,new.d2))
    
    new=data.frame(condition=EBAY.testing[[j]]$Condition*condition.v[i+1],
                   reserve=EBAY.testing[[j]]$Reserve*reserve.v[i+1],
                   open=EBAY.testing[[j]]$Price[1]*open.v[i+1],
                   seller=EBAY.testing[[j]]$Seller*seller.v[i+1],
                   value=EBAY.testing[[j]]$Value*value.v[i],
                   bidder=StepCheck(EBAY.testing[[j]]$Time,EBAY.testing[[j]]$Bidder,(i/NT.grid*(END-START)+START)),
                   early=EBAY.testing[[j]]$Early*early.v[i+1],
                   jumpexist=StepCheck(EBAY.testing[[j]]$Time,EBAY.testing[[j]]$Jump,(i/NT.grid*(END-START)+START)),
                   jump=(jumpexist>=1)*jump.v[i+1],
                   currentnumber=CurrentNumber.2.last[[j]][i],
                   Dy=Dy1,
                   Y.auto=smoothF.2[[j]][i]
                   #Y.auto=StepCheck(EBAY.testing[[j]]$Time,EBAY.testing[[j]]$Price,(i/NT.grid*(END-START)+START))
    )
    prev=pred
    pred=predict(Forecast[[i-(NT.grid-2)]],new)
    pred.temp=c(pred.temp,pred)
    real1st.temp=c(real1st.temp,smoothF.2[[j]][i+1])
    real2nd.temp=c(real2nd.temp,StepCheck(EBAY.testing[[j]]$Time,EBAY.testing[[j]]$Price,(i+1)/NT.grid*7))
    proc.time() - ptm
  }
  #[[i-59]]  #[[i-1871]] #[[i-623]] #[[i-1151]]
  predstep[[i-(NT.grid-2)]]=as.vector(pred.temp)
  real1st[[i-(NT.grid-2)]]=real1st.temp
  real2nd[[i-(NT.grid-2)]]=real2nd.temp
  MAPE1st=c(MAPE1st,mean(abs((real1st[[i-(NT.grid-2)]]-predstep[[i-(NT.grid-2)]])/real1st[[i-(NT.grid-2)]])))
  MAPE2ndtoend=c(MAPE2ndtoend,mean(abs(real2nd[[i-(NT.grid-2)]]-predstep[[i-(NT.grid-2)]])/real2nd[[i-(NT.grid-2)]]))
}
}
par(mfrow=c(1,1))
plot(MAPE2ndtoend,type="l",ylim=c(0,0.45))
MAPE2ndtoend
MAPE1st



###########################################################################
## MAPE1
###################################################################################
TOEND=TRUE ###
##############
START <- 6
NT.grid <- 4000

SimulateSelf.purestep = function(current,end,value,condition, early, jump,open,history.time,history.price)  {
  currentindex=trunc((current-START)/((END-START)/NT.grid)) 
  endindex=trunc((end-START)/((END-START)/NT.grid))
  s=0     
  t=current     #actual time of events
  index=1+currentindex     #grids to start in each loop
  sumZ=0
  price=history.price[length(history.price)]
  currentnumber=length(history.time)+length(t)-1
  if(value==1) WTP=p1
  if(value==0) WTP=p2
  ##########################################################
  z=c(0,WTP-max(price),value, condition, early, jump,open)
  #########################################################
  
  while(index<(endindex) || (index==(endindex) && sumZ>=s)) {
    u=runif(1)
    s=s-log(u)
    for( i in index:endindex){
      
      if(sumZ >= s){
        if(TOEND==TRUE){
          if(value==1) inc=sample(IncALL1, size=1)
          if(value==0) inc=sample(IncALL2, size=1)
        }
        if(TOEND==FALSE){
          if(value==1) inc=sample(Inc1.class[[case]], size=1)
          if(value==0) inc=sample(Inc2.class[[case]], size=1)
        }
        if(currentnumber==1) inc=0
        if(max(price)+inc > 1.3*max(price)) jump=jump+1
        price=c(price,max(price)+inc)
        ##########################################################
        z=c(0,WTP-max(price),value, condition, early, jump,open)
        #########################################################
        break
      }
      if(i!=endindex)
      {
        ##############################################
        gt=0
        current.time=START+(i)/NT.grid*(END-START)
        if(currentnumber !=0){
        for(k in 1:currentnumber){
          gt=gt+exp((current.time-history.time[k])*r)
        }
        }
        ##############################################
        z[1]=gt
        sumZ=sumZ+(z %*% beta[i,])*(END-START)/NT.grid 
      }  
    }
    
    if(length(price)>length(t)){t=c(t,START+(i)*(END-START)/NT.grid )}
    index=i
    currentnumber=currentnumber+1
    history.time=c(history.time,t[2:length(t)])
  } 
  return(cbind(t,price))
}

MAPE=rep(0,6)
MNE=rep(0,6)
sdMAPE=rep(0,6)
sdMNE=rep(0,6)
MAPE.5=rep(0,6)
MNE.5=rep(0,6)
MAPE.95=rep(0,6)
MNE.95=rep(0,6)
APE=rep(0,length(EBAY.testing))
NE=rep(0,length(EBAY.testing))
recordn=matrix(0,length(EBAY.testing),9)
record.prob=matrix(0,6,9)
#ModeError=matrix(0,length(EBAY.testing),2)
Quant=matrix(0,6,length(EBAY.testing))
horizon=c(7-2/24,7-1/24,7-.5/24,7-.25/24,7-5/60/24,7-1/60/24)
par(mfrow=c(3,2))
# for(case in 1:6){
  # current=horizon[case]
  # for(i in 1:length(EBAY.testing)){
    # price.sim=rep(0,500)
    # length.sim=rep(0,500)
    # if(max(EBAY.testing[[i]]$Price)>100){type=1}
    # else{type=0}
    # for(j in 1:500){
      # sim=SimulateSelf.purestep(current,end=7,value=EBAY.testing[[i]]$Value,
                                # condition=EBAY.testing[[i]]$Condition,
                                # reserve <- EBAY.testing[[i]]$Reserve,
                                # seller <-EBAY.testing[[i]]$Seller,
                                # bidder <-StepCheck(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$Bidder,current),
                                # early=EBAY.testing[[i]]$Early,
                                # #open=EBAY.testing[[i]]$Price[[1]],
                                # EBAY.testing[[i]]$Time[EBAY.testing[[i]]$Time<=current],
                                # EBAY.testing[[i]]$Price[EBAY.testing[[i]]$Time<=current])
      # price.sim[j]=max(sim[,2])
      # length.sim[j]=length(sim[,1])
    # }
    # for(nn in 1:9){
      # low=nn/20
      # up=1-nn/20
      # lowbound=quantile(price.sim,low)
      # upbound=quantile(price.sim,up)
      # if(max(EBAY.testing[[i]]$Price)>=lowbound && max(EBAY.testing[[i]]$Price)<=upbound){
        # recordn[i,nn]=1
      # }
    # }
    # FF=ecdf(price.sim)
    # Quant[case,i]=FF(max(EBAY.testing[[i]]$Price))
    # APE[i]=(mean(price.sim)-max(EBAY.testing[[i]]$Price))/max(EBAY.testing[[i]]$Price)
    # NE[i]=(mean(length.sim)-length(EBAY.testing[[i]]$Time)+StepCount(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$Time,current))
  # }
  # MAPE[case]=mean(abs(APE))
  # MNE[case]=mean(abs(NE))
  # sdMAPE[case]=sd(abs(APE))
  # sdMNE[case]=sd(abs(NE))
  # MAPE.5[case]=quantile(abs(APE),0.025)
  # MAPE.95[case]=quantile(abs(APE),0.975)
  # MNE.5[case]=quantile(abs(NE),0.025)
  # MNE.95[case]=quantile(abs(NE),0.975)
  # record.prob[case,]=apply(recordn,2,mean)
  # prob=c(.9,.8,.7,.6,.5,.4,.3,.2,.1)
  # plot(record.prob[case,],prob)
# }
for(case in 1:6){
  current=horizon[case]
  for(i in 1:length(EBAY.testing)){
    price.sim=rep(0,500)
    length.sim=rep(0,500)
    if(max(EBAY.testing[[i]]$Price)>100){type=1}
    else{type=0}
    for(j in 1:500){
      sim=SimulateSelf.purestep(current,end=7,value=EBAY.testing[[i]]$Value,
                                condition=EBAY.testing[[i]]$Condition,
                                early=EBAY.testing[[i]]$Early,
                                jump=StepCheck(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$Jump,current),
                                open=EBAY.testing[[i]]$Price[[1]],
                                EBAY.testing[[i]]$Time[EBAY.testing[[i]]$Time<=current],
                                EBAY.testing[[i]]$Price[EBAY.testing[[i]]$Time<=current])
      price.sim[j]=max(sim[,2])
      length.sim[j]=length(sim[,1])
    }
    for(nn in 1:9){
      low=nn/20
      up=1-nn/20
      lowbound=quantile(price.sim,low)
      upbound=quantile(price.sim,up)
      if(max(EBAY.testing[[i]]$Price)>=lowbound && max(EBAY.testing[[i]]$Price)<=upbound){
        recordn[i,nn]=1
      }
    }
    FF=ecdf(price.sim)
    Quant[case,i]=FF(max(EBAY.testing[[i]]$Price))
    APE[i]=(mean(price.sim)-max(EBAY.testing[[i]]$Price))/max(EBAY.testing[[i]]$Price)
    NE[i]=(mean(length.sim)-length(EBAY.testing[[i]]$Time)+StepCount(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$Time,current))
  }
  MAPE[case]=mean(abs(APE))
  MNE[case]=mean(abs(NE))
  sdMAPE[case]=sd(abs(APE))
  sdMNE[case]=sd(abs(NE))
  MAPE.5[case]=quantile(abs(APE),0.05)
  MAPE.95[case]=quantile(abs(APE),0.95)
  MNE.5[case]=quantile(abs(NE),0.05)
  MNE.95[case]=quantile(abs(NE),0.95)
  record.prob[case,]=apply(recordn,2,mean)
  prob=c(.9,.8,.7,.6,.5,.4,.3,.2,.1)
  plot(record.prob[case,],prob)
}
par(mfrow=c(1,1))
plot(MAPE,type="b",ylim=c(0,.5),xaxt="n",xlab="Time horizon")
axis(side=1,at=c(1,2,3,4,5,6),label=c("-2 h","-1 h","-30 m","-15 m","-5 m","-1 m"))
points(MAPE.5,type="l",col="red",lty=2)
points(MAPE.95,type="l",col="red",lty=2)
plot(MAPE,type="b",ylim=c(0,.5),xaxt="n",xlab="Time horizon",main="MAPE of Forecasting Final Price")
points(MAPE2ndtoend,type="b",col="red",lty=4,pch=2)
points(MAPE.5,type="l",col="grey",lty=2,pch=3)
points(MAPE.95,type="l",col="grey",lty=2,pch=3)
axis(side=1,at=c(1,2,3,4,5,6),label=c("-2 h","-1 h","-30 m","-15 m","-5 m","-1 m"))
legend("topright", c("SEBAM forecasting","FDA forecasting"),lty=c(1,4),pch=c(1,2),col=c("black","red"))
par(mfrow=c(1,2))
plot(record.prob[1,],prob,main="Forecasting 2 hr ahead",xlab="C.I",ylab="simulated C.I")
plot(record.prob[2,],prob,main="Forecasting 1 hr ahead",xlab="C.I",ylab="simulated C.I")

if(MAPE2ndtoend[2] > MAPE[2]) redo <- FALSE
}
###############################################################################################




##############################################################################################






par(mfrow=c(1,1))
plot(MAPE,type="b",ylim=c(0,.5),xaxt="n",xlab="Time horizon")
axis(side=1,at=c(1,2,3,4,5,6),label=c("-2 h","-1 h","-30 m","-15 m","-5 m","-1 m"))
points(MAPE.5,type="l",col="red",lty=2)
points(MAPE.95,type="l",col="red",lty=2)
plot(MAPE,type="b",ylim=c(0,.5),xaxt="n",xlab="Time horizon",main="MAPE of Forecasting Final Price")
points(MAPE2ndtoend,type="b",col="red",lty=4,pch=2)
points(MAPE.5,type="l",col="grey",lty=2,pch=3)
points(MAPE.95,type="l",col="grey",lty=2,pch=3)
axis(side=1,at=c(1,2,3,4,5,6),label=c("-2 h","-1 h","-30 m","-15 m","-5 m","-1 m"))
legend("topright", c("SEBAM forecasting","FDA forecasting"),lty=c(1,4),pch=c(1,2),col=c("black","red"))
par(mfrow=c(1,2))
plot(record.prob[1,],prob,main="Forecasting 2 hr ahead",xlab="C.I",ylab="simulated C.I")
plot(record.prob[2,],prob,main="Forecasting 1 hr ahead",xlab="C.I",ylab="simulated C.I")
par(mfrow=c(1,1))
plot(MAPE,type="b",ylim=c(0,.5),xaxt="n",xlab="Time horizon")
axis(side=1,at=c(1,2,3,4,5,6),label=c("-2 h","-1 h","-30 m","-15 m","-5 m","-1 m"))
points(MAPE.5,type="l",col="red",lty=2)
points(MAPE.95,type="l",col="red",lty=2)
plot(MAPE,type="b",ylim=c(0,.5),xaxt="n",xlab="Time horizon",main="MAPE of Forecasting Final Price")
points(MAPE2ndtoend,type="b",col="red",lty=4,pch=2)
points(MAPE.5,type="l",col="grey",lty=2,pch=3)
points(MAPE.95,type="l",col="grey",lty=2,pch=3)
axis(side=1,at=c(1,2,3,4,5,6),label=c("-2 h","-1 h","-30 m","-15 m","-5 m","-1 m"))
legend("topright", c("SEBAM forecasting","FDA forecasting"),lty=c(1,4),pch=c(1,2),col=c("black","red"))
par(mfrow=c(1,2))
plot(record.prob[1,],prob,main="Forecasting 2 hr ahead",xlab="C.I",ylab="simulated C.I")
plot(record.prob[2,],prob,main="Forecasting 1 hr ahead",xlab="C.I",ylab="simulated C.I")
par(mfrow=c(1,1))
plot(MAPE,type="b",ylim=c(0,.5),xaxt="n",xlab="Time horizon")
axis(side=1,at=c(1,2,3,4,5,6),label=c("-2 h","-1 h","-30 m","-15 m","-5 m","-1 m"))
points(MAPE.5,type="l",col="red",lty=2)
points(MAPE.95,type="l",col="red",lty=2)
plot(MAPE,type="b",ylim=c(0,.5),xaxt="n",xlab="Time horizon",main="MAPE of Forecasting Final Price")
points(MAPE2ndtoend,type="b",col="red",lty=4,pch=2)
points(MAPE.5,type="l",col="grey",lty=2,pch=3)
points(MAPE.95,type="l",col="grey",lty=2,pch=3)
axis(side=1,at=c(1,2,3,4,5,6),label=c("-2 h","-1 h","-30 m","-15 m","-5 m","-1 m"))
legend("topright", c("SEBAM forecasting","FDA forecasting"),lty=c(1,4),pch=c(1,2),col=c("black","red"))
par(mfrow=c(1,2))
plot(record.prob[1,],prob,main="Forecasting 2 hr ahead",xlab="C.I",ylab="simulated C.I")
plot(record.prob[2,],prob,main="Forecasting 1 hr ahead",xlab="C.I",ylab="simulated C.I")
