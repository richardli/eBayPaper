
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
MAPE2nd=NULL
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
  MAPE2nd=c(MAPE2nd,mean(abs(real2nd[[i-(NT.grid-2)]]-predstep[[i-(NT.grid-2)]])/real2nd[[i-(NT.grid-2)]]))
}
}
par(mfrow=c(1,1))
plot(MAPE2nd,type="l",ylim=c(0,0.45))
MAPE2nd
MAPE1st