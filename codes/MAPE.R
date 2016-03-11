
###########################################################################
#Simulation Function To defined End Time
###########################################################################
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
        sumZ=sumZ+(z %*% beta.coef[i,])*(END-START)/NT.grid 
      }  
    }
    
    if(length(price)>length(t)){t=c(t,START+(i)*(END-START)/NT.grid )}
    index=i
    currentnumber=currentnumber+1
    history.time=c(history.time,t[2:length(t)])
  } 
  return(cbind(t,price))
}
###########################################################################
## Note, needs to change the starting time in Regressing to 6!
## MAPE2
####################################################################################################################
TOEND=FALSE ###
##############
START <- 6
NT.grid <- 4000

sep=0.1
horizon=seq(6,7-sep,by=sep)

MAPEday6=rep(0,length(horizon))
MNEday6=rep(0,length(horizon))
sdMAPEday6=rep(0,length(horizon))
sdMNEday6=rep(0,length(horizon))
MAPEday6.5=rep(0,length(horizon))
MNEday6.5=rep(0,length(horizon))
MAPEday6.95=rep(0,length(horizon))
MNEday6.95=rep(0,length(horizon))
APEday6=rep(0,length(EBAY.testing))
NEday6=rep(0,length(EBAY.testing))



# for(case in 1:length(horizon)){  
  # current=horizon[case]
  # for(i in 1:length(EBAY.testing)){
    # priceday6.sim=rep(0,500)
    # lengthday6.sim=rep(0,500)
    # for(j in 1:500){
      # sim=SimulateSelf.purestep(current,end=current+sep,value=EBAY.testing[[i]]$Value,
                                # condition=EBAY.testing[[i]]$Condition,
                                # reserve <- EBAY.testing[[i]]$Reserve,
                                # seller <-EBAY.testing[[i]]$Seller,
                                # bidder <-StepCheck(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$Bidder,current),
                                # early=EBAY.testing[[i]]$Early,
                                # #open=EBAY.testing[[i]]$Price[[1]],
                                # EBAY.testing[[i]]$Time[EBAY.testing[[i]]$Time<=current],
                                # EBAY.testing[[i]]$Price[EBAY.testing[[i]]$Time<=current])
      # priceday6.sim[j]=max(sim[,2])
      # lengthday6.sim[j]=length(sim[,1])
    # }
    # a=StepCheck(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$Price,current+0.1)
    # b=StepCount(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$Price,current+0.1)
    # priceday6.sim=sort(priceday6.sim)
    # priceday6.sim=priceday6.sim
    # APEday6[i]=(mean(priceday6.sim)-a)/a
    # NEday6[i]=mean(lengthday6.sim)-b+StepCount(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$Time,current)
  # }
# MAPEday6[case]=mean(abs(APEday6))
# MNEday6[case]=mean(abs(NEday6))
# sdMAPEday6[case]=sd(abs(APEday6))
# sdMNEday6[case]=sd(abs(NEday6))
# MAPEday6.5[case]=quantile(abs(APEday6),0.05)
# MAPEday6.95[case]=quantile(abs(APEday6),0.95)
# MNEday6.5[case]=quantile(abs(NEday6),0.05)
# MNEday6.95[case]=quantile(abs(NEday6),0.95)
# }

for(case in 1:length(horizon)){  
  current=horizon[case]
  for(i in 1:length(EBAY.testing)){
    priceday6.sim=rep(0,500)
    lengthday6.sim=rep(0,500)
    for(j in 1:500){
      sim=SimulateSelf.purestep(current,end=current+sep,value=EBAY.testing[[i]]$Value,
                                condition=EBAY.testing[[i]]$Condition,
                                early=EBAY.testing[[i]]$Early,
                                jump=StepCheck(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$Jump,current),
                                open=EBAY.testing[[i]]$Price[[1]],
                                EBAY.testing[[i]]$Time[EBAY.testing[[i]]$Time<=current],
                                EBAY.testing[[i]]$Price[EBAY.testing[[i]]$Time<=current])
      priceday6.sim[j]=max(sim[,2])
      lengthday6.sim[j]=length(sim[,1])
    }
    a=StepCheck(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$Price,current+0.1)
    b=StepCount(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$Price,current+0.1)
    priceday6.sim=sort(priceday6.sim)
    priceday6.sim=priceday6.sim
    APEday6[i]=(mean(priceday6.sim)-a)/a
    NEday6[i]=mean(lengthday6.sim)-b+StepCount(EBAY.testing[[i]]$Time,EBAY.testing[[i]]$Time,current)
  }
MAPEday6[case]=mean(abs(APEday6))
MNEday6[case]=mean(abs(NEday6))
sdMAPEday6[case]=sd(abs(APEday6))
sdMNEday6[case]=sd(abs(NEday6))
MAPEday6.5[case]=quantile(abs(APEday6),0.05)
MAPEday6.95[case]=quantile(abs(APEday6),0.95)
MNEday6.5[case]=quantile(abs(NEday6),0.05)
MNEday6.95[case]=quantile(abs(NEday6),0.95)
}

par(mfrow=c(1,1),mar=rep(4,4))
horizon=seq(6,7-sep,by=sep)
plot(horizon+0.1, MAPEday6,type="b",ylim=c(0,.5),xlab="Time horizon",main="MAPE of Forecasting 0.1 day Ahead")
points(horizon+0.1,MAPE2nd,type="b",col="red",lty=4,pch=2)
points(horizon+0.1,MAPEday6.5,type="l",col="grey",lty=2,pch=3)
points(horizon+0.1,MAPEday6.95,type="l",col="grey",lty=2,pch=3)
legend("topleft", c("SEBAM forecasting","FDA forecasting"),lty=c(1,4),pch=c(1,2),col=c("black","red"))

#############################################################################################
# SimulateSelf.purestep = function(current,end,condition,value, reserve, seller, bidder, early,history.time,history.price)  {
  # currentindex=trunc((current-START)/((END-START)/NT.grid)) ## here uses the fact that index starts from halfway
  # endindex=trunc((end-START)/((END-START)/NT.grid))
  # s=0     
  # t=current     #actual time of events
  # index=1+currentindex     #grids to start in each loop
  # sumZ=0
  # price=history.price[length(history.price)]
  # currentnumber=length(history.time)+length(t)-1
  # if(value==1) WTP=p1
  # if(value==0) WTP=p2
  # ##########################################################
  # z=c(0,WTP-max(price),value,condition, reserve, seller, bidder, early)
  # #########################################################
  
  # while(index<(endindex) || (index==(endindex) && sumZ>=s)) {
    # u=runif(1)
    # s=s-log(u)
    # for( i in index:endindex){
      
      # if(sumZ >= s){
        # if(TOEND==TRUE){
          # if(value==1) inc=sample(IncALL1, size=1)
          # if(value==0) inc=sample(IncALL2, size=1)
        # }
        # if(TOEND==FALSE){
          # if(value==1) inc=sample(Inc1.class[[case]], size=1)
          # if(value==0) inc=sample(Inc2.class[[case]], size=1)
        # }
        # if(currentnumber==1) inc=0
        # #if(max(price)+inc > 1.3*max(price)) jump=jump+1
        # price=c(price,max(price)+inc)
        # ##########################################################
        # z=c(0,WTP-max(price),value,condition, reserve, seller, bidder, early)
        # #########################################################
        # break
      # }
      # if(i!=endindex)
      # {
        # ##############################################
        # gt=0
        # current.time=START+(i)/NT.grid*(END-START)
        # if(currentnumber !=0){
        # for(k in 1:currentnumber){
          # gt=gt+exp((current.time-history.time[k])*r)
        # }
        # }
        # ##############################################
        # z[1]=gt
        # sumZ=sumZ+(z %*% beta[i,])*(END-START)/NT.grid 
      # }  
    # }
    
    # if(length(price)>length(t)){t=c(t,START+(i)*(END-START)/NT.grid )}
    # index=i
    # currentnumber=currentnumber+1
    # history.time=c(history.time,t[2:length(t)])
  # } 
  # return(cbind(t,price))
# }