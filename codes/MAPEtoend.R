###########################################################################
## MAPE1
###################################################################################
TOEND=TRUE ###
##############
START <- 6
NT.grid <- 4000

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
recordn=matrix(0,length(EBAY.testing),19)
record.prob=matrix(0,6,19)
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
    for(nn in 1:19){
      low=nn/40
      up=1-nn/40
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
  prob=seq(0.95,0.05,len = 19)
  plot(record.prob[case,],prob)
}
par(mfrow=c(1,1))
plot(MAPE,type="b",ylim=c(0,.5),xaxt="n",xlab="Time horizon")
axis(side=1,at=c(1,2,3,4,5,6),label=c("-2 h","-1 h","-30 m","-15 m","-5 m","-1 m"))
points(MAPE.5,type="l",col="red",lty=2)
points(MAPE.95,type="l",col="red",lty=2)
plot(MAPE,type="b",ylim=c(0,.5),xaxt="n",xlab="Time horizon",main="MAPE of Forecasting Final Price")
points(MAPE2nd,type="b",col="red",lty=4,pch=2)
points(MAPE.5,type="l",col="grey",lty=2,pch=3)
points(MAPE.95,type="l",col="grey",lty=2,pch=3)
axis(side=1,at=c(1,2,3,4,5,6),label=c("-2 h","-1 h","-30 m","-15 m","-5 m","-1 m"))
legend("topright", c("SEBAM forecasting","FDA forecasting"),lty=c(1,4),pch=c(1,2),col=c("black","red"))
par(mfrow=c(1,2))
plot(record.prob[1,],prob,main="Forecasting 2 hr ahead",ylab="C.I",xlab="simulated C.I",type="b",cex=0.5)
plot(record.prob[2,],prob,main="Forecasting 1 hr ahead",ylab="C.I",xlab="simulated C.I",type="b",cex=0.5)
plot(record.prob[4,],prob,main="Forecasting 0.5 hr ahead",ylab="C.I",xlab="simulated C.I")
abline(a=0,b=1)

##########################################################################
# For debug
#########################################################################
WTP=EBAY.testing$Pricediff[1]+EBAY.testing$Price[1]
history.time=EBAY.testing$Time[EBAY.testing$Time<=current]
history.price=EBAY.testing$Price[EBAY.testing$Time<=current]
reserve=EBAY.testing$Reserve

###########################################################################
#Simulation Function
#############################################################################################
# SimulateSelf= function(current,value,condition,history.time,history.price)  {
#   currentindex=trunc((current-START)/((END-START)/NT.grid)) ## here uses the fact that index starts from halfway
#   Tmax=END
#   s=0     
#   t=current     #actual time of events
#   index=1+currentindex     #grids to start in each loop
#   sumZ=0
#   price=history.price[length(history.price)]
#   currentnumber=length(history.time)+length(t)-1
#   if(value==1) {WTP=p1}
#   if(value==0) {WTP=p2}
#   z=c(1,0,WTP-max(price),value,condition)
#   
#   while(index<(NT.grid) || (index==(NT.grid) && sumZ>=s)) {
#     u=runif(1)
#     s=s-log(u)
#     for( i in index:NT.grid){
#       
#       if(sumZ >= s){
#         if(value==1) inc=rnorm(1)^2*4.5                    ## Price Inc
#         if(value==0) inc=rnorm(1)^2*.6
#         price=c(price,max(price)+inc)
#         z=c(1,0,WTP-max(price),value,condition)
#         break
#       }
#       if(i!=NT.grid)
#       {
#         ##############################################
#         gt=0
#         current.time=START+(i)/NT.grid*(END-START)
#         for(k in max(1,currentnumber-max.count):currentnumber){
#           gt=gt+exp((current.time-history.time[k])*r)
#         }
#         ##############################################
#         z[2]=gt
#         sumZ=sumZ+(z %*% beta[i,])*(END-START)/NT.grid 
#       }  
#     }
#     
#     if(length(price)>length(t)){t=c(t,START+(i)*(Tmax-START)/NT.grid )}
#     index=i
#     currentnumber=currentnumber+1
#     history.time=c(history.time,t[2:length(t)])
#   } 
#   return(cbind(t,price))
# }
