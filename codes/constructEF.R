install.packages(c("optimx","BB","ucminf","Rcgmin","quadprog","Rvmmin","minqa"))
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
smoothF.12=NULL
pdf.testing=NULL
Fpar=matrix(0,length(EBAY.testing),2)
for(i in 1:length(EBAY.testing)){
  
{  ## Obtain the parameter
  
  lastday=StepCount(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$Time, START)
  if(lastday==length(EBAY.testing[[i]]$Time)){
    Fpar[i,]=c(9999,9999)
    smoothF.12[[i]]=rep(0,NT.grid)
    pdf.12.beta[[i]]=rep(0,NT.grid)
    next
  }
  
  #  To handle the last term being inf.
  deltaT=max(0.000001,(EBAY.testing[[i]]$Time[length(EBAY.testing[[i]]$Time)]-EBAY.testing[[i]]$Time[length(EBAY.testing[[i]]$Time)-1]))
  x1= min(EBAY.testing[[i]]$Time[length(EBAY.testing[[i]]$Time)]+deltaT,END)
  xx= c(EBAY.testing[[i]]$Time[lastday:length(EBAY.testing[[i]]$Time)],x1)
  yy= c(EBAY.testing[[i]]$F[lastday:length(EBAY.testing[[i]]$F)])
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


smoothF.12.temp=NULL
pdf.12.beta.temp=NULL
for(j in 1:NT.grid){
  integrand=function(u){u^(Fpar[i,1]-1)*(1-u)^(Fpar[i,2]-1)}
  smoothF.12.temp=cbind(smoothF.12.temp,integrate(integrand, 0, j/(NT.grid+1))$value/beta(Fpar[i,1],Fpar[i,2]))
  pdf.12.beta.temp = cbind(pdf.12.beta.temp, integrand(j/(NT.grid+1))/beta(Fpar[i,1],Fpar[i,2]))
}

## Change to last day, edited Jan 2013
smoothF.12[[i]]<-(smoothF.12.temp * (max(yy)-min(yy)) + min(yy))[4001:8000]
pdf.testing[[i]]<-(pdf.12.beta.temp * (max(yy)-min(yy))/(END-START))[4001:8000]

par(mfrow=c(1,2))
xaxis=seq(START+1,END,len=NT.grid/2)
plot(xaxis, smoothF.12[[i]],type="l",xlim=c(START,END),ylim=c(0,1))
points(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$F,cex=0.5)
points(x*(END-START)+START,y* (max(yy)-min(yy)) + min(yy),col="red")
plot(xaxis, pdf.testing[[i]],cex=0.4)
}
#############################
NT.grid <- NT.grid/2
START <- START + 1
#############################
## CurrentNumber Count
{
  CurrentNumber.test=NULL
  for( i in 1:length(EBAY.testing)){
    currentnumber.test.temp=rep(0,NT.grid)
    for( j in 1:(NT.grid)){
      currentnumber.test.temp[j]=length(EBAY.testing[[i]]$Time[EBAY.testing[[i]]$Time<((j)/NT.grid*(END-START)+START)])
    }
    CurrentNumber.test[[i]]=currentnumber.test.temp
  }
}


fit.pdf <- matrix(0, length(EBAY.testing),NT.grid)
fit.low <- matrix(0, length(EBAY.testing),NT.grid)
fit.up <- matrix(0, length(EBAY.testing),NT.grid)
PR <- rep(0, length(EBAY.testing))
value <- rep(0, length(EBAY.testing))
condition <-rep(0, length(EBAY.testing))
reserve <-rep(0, length(EBAY.testing))
seller <-rep(0, length(EBAY.testing))
bidder <-rep(0, length(EBAY.testing))
early <- rep(0, length(EBAY.testing))
jump <- rep(0, length(EBAY.testing))
open <- rep(0, length(EBAY.testing))
for(i in 1:(NT.grid)){
  t=rep(0,length(EBAY.testing))
  for(j in 1:length(EBAY.testing)){
    # Y[j] = pdf.1.beta[[j]][i]*length(EBAY.testing[[j]]$Time)
    value[j]<-EBAY.testing[[j]]$Value
    currentnumber<-CurrentNumber.test[[j]][i]
    current.time=(i)/NT.grid*(END-START)+START
    reserve[j]=EBAY.testing[[j]]$Reserve
    condition[j]=EBAY.testing[[j]]$Condition
    for(k in 1:currentnumber){
      t[j]=t[j]+exp((current.time-EBAY.testing[[j]]$Time[k])*r)
    }
    pcurrent=StepCheck(EBAY.testing[[j]]$Time,EBAY.testing[[j]]$Price,((i)/NT.grid*(END-START)+START))
    if(EBAY.testing[[j]]$Value==1){WTP=p1}
    else{WTP=p2}
    PR[j]=WTP-pcurrent
    
    value[j]<-EBAY.testing[[j]]$Value
    condition[j]<-EBAY.testing[[j]]$Condition
    reserve[j]<-EBAY.testing[[j]]$Reserve
    seller[j]<-EBAY.testing[[j]]$Seller
    bidder[j]<-StepCheck(EBAY.testing[[j]]$Time,EBAY.testing[[j]]$Bidder,((i)/NT.grid*(END-START)+START))
    early[j]<-EBAY.testing[[j]]$Early
    jump[j]<-StepCheck(EBAY.testing[[j]]$Time,EBAY.testing[[j]]$Jump,((i)/NT.grid*(END-START)+START))
    open[j]<-EBAY.testing[[j]]$Price[1]
  }
 # allv=data.frame(Self.Exciting=t,  Price.Relative=PR,  Value=value,
 #                 Condition=condition,  Reserve=reserve, Seller.Rating=seller,
 #                 Bidder.Rating=bidder, Early.Bidding=early, Jump.Bidding=jump,
 #                 Opening.Price=open)
   allv=data.frame(Self.Exciting=t,  Price.Relative=PR, Value=value,         
                   Condition=condition,Early.Bidding=early,
                   Jump.Bidding=jump, Opening.Price=open)

    fitmat <- as.matrix(predict(FuncReg[[i]],allv,interval="prediction"))
    fit.pdf[, i] <- fitmat[,1]
    fit.low[, i]<- fitmat[,2]
    fit.up[, i]<- fitmat[,3]
}
par(mfrow=c(1,2))
timeaxis <-(c(1:(NT.grid)))/NT.grid*(END-START)+START
# i <- 22
i<-sample(seq(1,length(EBAY.testing)),1)
plot(timeaxis,fit.pdf[i, ], type = "l",ylim = c(max(min(fit.low[i,]),0),max(pdf.testing[[i]])),xlab="time",ylab="intensity",lty=6)
lines(timeaxis, pdf.testing[[i]]*length(EBAY.testing[[i]]$Time), col="red",lwd=2)
lines(timeaxis, fit.low[i, ], col="grey", lty=4)
lines(timeaxis, fit.up[i, ], col= "grey", lty=4)
legend("topleft",lty=c(1,6,4),col=c("red","black","grey"),c("intensity from data","predicted intensity from regression","95% C.I for predicted intensity"))
plot(timeaxis,cumsum(fit.pdf[i, ])/NT.grid,type="l",xlab="time",ylab="integration of intensity",lty=6)
lines(timeaxis,cumsum(fit.up[i, ])/NT.grid,type="l",col="grey", lty=4)
lines(timeaxis,cumsum(fit.low[i, ])/NT.grid,type="l",col="grey", lty=4)
lines(xaxis, cumsum(pdf.testing[[i]])*length(EBAY.testing[[i]]$Time)/NT.grid,type="l",xlim=c(START,END),col="red",lwd=2)
legend("topleft",lty=c(1,6,4),col=c("red","black","grey"),c("fitted from data","predicted from regression","95% C.I for predicted"))
length(EBAY.testing[[i]]$Time>6)

count.within <- 0
for(i in 1:length(EBAY.testing)){
	for(j in 1:NT.grid){
		if(pdf.testing[[i]][j]*length(EBAY.testing[[i]]$Time) >= fit.low[i, j] && pdf.testing[[i]][j]*length(EBAY.testing[[i]]$Time) <= fit.up[i, j]){
			count.within <- count.within + 1
		}
	}
}
count.within/length(EBAY.testing)/NT.grid