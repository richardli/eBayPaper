library(spatstat)
##############
TOEND=TRUE ###
##############
par(mfrow=c(1,2))
current=6.5
price.sim=rep(0,30)
length.sim=rep(0,30)
k=sample(seq(1:length(EBAY.testing)),1)
EBAY.testing.new=EBAY.testing[[k]]
simall=NULL
plot(EBAY.testing.new$Time,EBAY.testing.new$Price,
     xlim=c(6,7),ylim=c(EBAY.testing.new$Price[1],max(EBAY.testing.new$Price)*2),
     xlab="Time horizon",ylab="Price",pch=3)
abline(v=current)
ptm <- proc.time()
for(j in 1:40){
  sim=SimulateSelf.purestep(current,end=7,value=EBAY.testing.new$Value,
                            condition=EBAY.testing.new$Condition,
                            reserve = EBAY.testing.new$Reserve,
                            seller=EBAY.testing.new$Seller,
                            bidder=StepCheck(EBAY.testing.new$Time,EBAY.testing.new$Bidder,current),
                            early=EBAY.testing.new$Early,
            
                            EBAY.testing.new$Time[EBAY.testing.new$Time<=current],
                            EBAY.testing.new$Price[EBAY.testing.new$Time<=current])
  if(dim(sim)[1]>1){simall=rbind(simall,sim[2:dim(sim)[1],])}
  lines(sim[,1],sim[,2],cex=0.1,col="red")
}
points(EBAY.testing.new$Time,EBAY.testing.new$Price,pch=3)
proc.time() - ptm


s1=(simall[,1]-min(simall[,1]))/(max(simall[,1])-min(simall[,1]))
s2=(simall[,2]-min(simall[,2]))/(max(simall[,2])-min(simall[,2]))
s=cbind(s1,s2)
w=as.ppp(s,c(0,1,0,1))
plot(density(w),main="density plot")
a=(EBAY.testing.new$Time-min(simall[,1]))/(max(simall[,1])-min(simall[,1]))
b=(EBAY.testing.new$Price-min(simall[,2]))/(max(simall[,2])-min(simall[,2]))
points(a,b)

###################################################################################################
k=sample(seq(1:length(EBAY.testing)),1)
for(k in 1:length(EBAY.testing)){
START=6
current.time=7-5/60/24
currentnumber=length(EBAY.testing[[k]]$Time[EBAY.testing[[k]]$Time<current.time])
currentindex=trunc((current.time-START)*4000)+1
t=rep(0,NT.grid-currentindex+1)
for(j in 1:length(t)){
for(i in 1:currentnumber){
  t[j]=t[j]+exp((START+(currentindex+j-1)/NT.grid-EBAY.testing[[k]]$Time[i])*r)
}
}
currentprice=StepCheck(EBAY.testing[[k]]$Time, EBAY.testing[[k]]$Price, current.time)
value=EBAY.testing[[k]]$Value
condition=EBAY.testing[[k]]$Condition
early=EBAY.testing[[k]]$Early
jump=StepCheck(EBAY.testing[[k]]$Time,EBAY.testing[[k]]$Jump,current.time)
open=EBAY.testing[[k]]$Price[1]
if(value==1) WTP=p1
if(value==0) WTP=p2
PR=WTP-currentprice

par(mfrow=c(3,2))
plotcase=function(name){
plot(xaxis, pdf.2.beta[[k]],cex=0.4,xlim=c(7-8/60/24,7),type="l",col="white",ylab="lambda",xlab="time",ylim=c(-100,300), main=name)
abline(v=START+(currentindex)/NT.grid)
for(i in 1:length(EBAY.testing[[k]]$Time)){
  abline(v=EBAY.testing[[k]]$Time[i],col="red")
}
lines(xaxis[currentindex:NT.grid], beta[currentindex:NT.grid,1]*t)
lines(xaxis[currentindex:NT.grid], beta[currentindex:NT.grid,2]*PR)
lines(xaxis[currentindex:NT.grid], beta[currentindex:NT.grid,3])
lines(xaxis[currentindex:NT.grid], beta[currentindex:NT.grid,4])
lines(xaxis[currentindex:NT.grid], beta[currentindex:NT.grid,5])
lines(xaxis[currentindex:NT.grid], beta[currentindex:NT.grid,6])
lines(xaxis[currentindex:NT.grid], beta[currentindex:NT.grid,7]*open)
lines(xaxis[currentindex:NT.grid], beta[currentindex:NT.grid,1]*t+
                                   beta[currentindex:NT.grid,2]*PR+                                
                                   beta[currentindex:NT.grid,3]*value+
                                   beta[currentindex:NT.grid,4]*condition+
                                   beta[currentindex:NT.grid,5]*early+
                                   beta[currentindex:NT.grid,6]*jump+
                                   beta[currentindex:NT.grid,7]*open, lty=2, col="red")
}

plotcase("Original")
value=as.numeric(value==0)
plotcase("Change value")
value=as.numeric(value==0)
condition=as.numeric(condition==0)
plotcase("Change condition")
condition=as.numeric(condition==0)
early=as.numeric(early==0)
plotcase("Change early")
early=as.numeric(early==0)
open=open*2
plotcase("Double opening price")
open=open/2
jump=jump+3
plotcase("Add three more jumps")
jump=jump-3
}
############################################################################################

library(optimx)
library(BB)
library(ucminf)
library(Rcgmin)
library(quadprog)
library(Rvmmin)
library(minqa)
######################
START=6
K=2
NT.grid=4000
END=7  #For the time being, the end could not be changed!!!
## Beta fitting
######################
smoothF.2=NULL
pdf.2.beta=NULL
Fpar=matrix(0,length(EBAY.training),2)
for(i in 1:length(EBAY.training)){
  
{  ## Obtain the parameter
  
  lastday=StepCount(EBAY.training[[i]]$Time,EBAY.training[[i]]$Time, START)
  if(lastday==length(EBAY.training[[i]]$Time)){
    Fpar[i,]=c(9999,9999)
    smoothF.2[[i]]=rep(0,NT.grid)
    pdf.2.beta[[i]]=rep(0,NT.grid)
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


smoothF.2.temp=NULL
pdf.2.beta.temp=NULL
for(j in 1:NT.grid){
  integrand=function(u){u^(Fpar[i,1]-1)*(1-u)^(Fpar[i,2]-1)}
  smoothF.2.temp=cbind(smoothF.2.temp,integrate(integrand, 0, j/(NT.grid+1))$value/beta(Fpar[i,1],Fpar[i,2]))
  pdf.2.beta.temp = cbind(pdf.2.beta.temp, integrand(j/(NT.grid+1))/beta(Fpar[i,1],Fpar[i,2]))
}
smoothF.2[[i]]=smoothF.2.temp * (max(yy)-min(yy)) + min(yy)
pdf.2.beta[[i]]=pdf.2.beta.temp * (max(yy)-min(yy))/(END-START)

#Print out the plots of Fitting and lambda
{par(mfrow=c(1,2))
 xaxis=seq(START,END,len=NT.grid)
 plot(xaxis, smoothF.2[[i]],type="l",xlim=c(START,END),ylim=c(0,1))
 points(EBAY.training[[i]]$Time,EBAY.training[[i]]$F,cex=0.5)
 points(x*(END-START)+START,y* (max(yy)-min(yy)) + min(yy),col="red")
 plot(xaxis, pdf.2.beta[[i]],cex=0.4)
}
}


