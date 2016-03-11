############################################## Include beta fitting and regression##########################
#################################### Parameters: START, NT.grid, EBAY.training##############################
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

par(mfrow=c(1,2))
xaxis=seq(START+1,END,len=NT.grid/2)
plot(xaxis, smoothF.1[[i]],type="l",xlim=c(START,END),ylim=c(0,1))
points(EBAY.training[[i]]$Time,EBAY.training[[i]]$F,cex=0.5)
points(x*(END-START)+START,y* (max(yy)-min(yy)) + min(yy),col="red")
plot(xaxis, pdf.1.beta[[i]],cex=0.4)

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
