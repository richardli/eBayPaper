setwd("~/Dropbox/Ebay_2012/Modeling Online Auction")
Xbox<-read.csv("XboxBidAll.csv",header=T)
HP<-read.csv("HPBidALL.csv",header=T)
Value=c(rep(1,dim(Xbox)[1]),rep(0,dim(HP)[1]))
Xbox=rbind(Xbox,HP)
Xbox=cbind(Xbox,Value)
library(fda)
#Function: Stepcheck(time, vector, t), return vector value at t.
StepCheck <- function(time,vector,t){ 
  stage=0
  if(length(time)==1 && time[1]>=t){return(0)}
  if(length(time)==1 && time[1]<t){return(vector[1])}
  for(i in 1 : (length(time)-1)){
    if (t>=time[i] && t<time[i+1]){stage=vector[i]}
  }
  if (t>=time[length(time)]){stage=vector[length(time)]}
  return(stage)
}
#Function: Stepcheck(time, vector, t), return vector step count at t.
StepCount <- function(time,vector,t){ 
  if(length(time)==1 && time[1]>=t){return(0)}
  if(length(time)==1 && time[1]<t){return(1)}
  for(i in 1 : (length(time)-1)){
    if (t>=time[i] && t<time[i+1]){count=i}
  }
  if (t>=time[length(time)]){count=length(time)}
  return(count)
}

## Initialize Variables
{
Price=NULL
Time=NULL
Length=NULL
Value=NULL
Condition=NULL
Reserve=NULL
Rating.seller=NULL
Rating.bidder=NULL
id.bidder <- NULL

Price.temp=Xbox[1,8]       	  	             # Openbids 
Time.temp=0
Length.temp=Xbox$Length[1]
Value.temp=Xbox$Value[1]
Condition.temp = Xbox$Condition[1]
Reserve.temp = Xbox$Reserve[1]
Rating.seller.temp = Xbox$SellerRating[1]
Rating.bidder.temp = 0
id.bidder.temp <- 0
index=1
total.n=nrow(Xbox)
}
#Note: all vectors start from time 0 value, not first bid value
#Price; Time; Condition; Reserve; Rating.seller; Rating.bidder
#Time,Price,F(cdf)Condition(0=new,1=used),Reserve(0=No,1=Yes)
#Seller(rating),Bidder(rating,average),Early,Jump)
{
for (i in 1:total.n){ 
  Price.temp=c(Price.temp,Xbox[i,9])
  Time.temp=c(Time.temp,Xbox[i,5])
  Rating.bidder.temp=c(Rating.bidder.temp, Xbox$BidderRating[i])
  id.bidder.temp <- c(id.bidder.temp, Xbox$BidderID[i])
  
  if (i!=total.n){	# Reset things if A new auction begins
    if ((Xbox[i,1]!=Xbox[i+1,1])){   # New auction if the 1st col number are different 
      Price[[index]]=Price.temp
      Time[[index]]=Time.temp
      Length[[index]]=Length.temp
      Value[[index]]=Value.temp
      Condition[[index]]=as.numeric(Condition.temp)-1
      Reserve[[index]]=Reserve.temp
      Rating.seller[[index]]=Rating.seller.temp
      Rating.bidder[[index]]=Rating.bidder.temp
      id.bidder[[index]] <- id.bidder.temp
      
      index=index+1		
      Price.temp=Xbox[i+1,8]
      Length.temp=Xbox[i+1,6]
      Value.temp=Xbox[i+1,23]
      Time.temp=0
      Condition.temp = Xbox$Condition[i+1]
      Reserve.temp = Xbox$Reserve[i+1]
      Rating.seller.temp = Xbox$SellerRating[i+1]
      Rating.bidder.temp = 0
      id.bidder.temp <- 0
    }
  }
}
# For the last auction
Price[[index]]=Price.temp
Time[[index]]=Time.temp
Length[[index]]=Length.temp
Value[[index]]=Value.temp
Condition[[index]]=as.numeric(Condition.temp)-1
Reserve[[index]]=Reserve.temp
Rating.seller[[index]]=Rating.seller.temp
Rating.bidder[[index]]=Rating.bidder.temp
id.bidder[[index]] <- id.bidder.temp
}

## Early: the first biding within 1.5 days. Dummy.
{
Early=NULL
for (i in 1:index){
  if (Time[[i]][2]< 1.5){ 
    Early[[i]]= 1}
  else{
    Early[[i]] = 0}
}
}
## Jump: the # of jump biding 
{
Jump=NULL
for (j in 1:index){
  Jump.temp = 0
  for (k in 2:length(Price[[j]])){
    if (Price[[j]][k] > 1.3*Price[[j]][k-1]){
      Jump.temp=c(Jump.temp,1)}
    else {
      Jump.temp=c(Jump.temp,0)}
  }
  Jump[[j]]=cumsum(Jump.temp)
}
}

## Rating.bidder:Change Bidder rating to moving average
## Special Note: the first element is 0
{
for ( i in 1:index){   
  for ( k in 3: length(Price[[i]])){
    Rating.bidder[[i]][k] = (Rating.bidder[[i]][k-1]*(k-2)+Rating.bidder[[i]][k])/(k-1) 
  }
}
}

## Pricelive: Transfer to live bid
{
Price.raw <- Price
Pricelive=Price
increment=function(p){
  inc=0
  if(p<=0.99){inc=0.05}
  if(p>=1 && p<=4.99){inc=0.25}
  if(p>=5 && p<=24.99){inc=0.5}
  if(p>=25 && p<=99.99){inc=1}
  if(p>=100 && p<=249.99){inc=2.5}
  if(p>=250 && p<=499.99){inc=5}
  if(p>=500 && p<=999.99){inc=10}
  if(p>=1000 && p<=2499.99){inc=25}
  if(p>=2500 && p<=4999.99){inc=50}
  if(p>=5000){inc=100}
  return(inc)
}
for( i in 1: length(Price)){
  Pricelive[[i]][1]=Price[[i]][1]
  Pricelive[[i]][2]=Price[[i]][1]
  max=Price[[i]][2]
  if(length(Price[[i]])>=3){
  for(j in 3: length(Price[[i]])){
    d=Price[[i]][j] - max
    if(d>0){ 
      Pricelive[[i]][j] <- min( max + increment(max), Price[[i]][j])
      max = Price[[i]][j]
      }
    if(d<0) {
      Pricelive[[i]][j] <- max(Price[[i]][j]+increment(Price[[i]][j]), Pricelive[[i]][j-1])
      }
    if(d==0){
      Pricelive[[i]][j]<-Price[[i]][j]
      }
  }
}
}
}
## Pricediff: Price differnence comparing with the end
{
#   Pricenew=NULL
#   Priceused=NULL
#   sumnew=0;countnew=0
#   sumused=0;countused=0
#   for(i in 1:length(Pricelive)){
#     if(Condition[[i]]==0){
#       sumnew=sumnew+max(Price[[i]])
#       countnew=countnew+1
#       Pricenew=c(Pricenew,max(Price[[i]]))
#       }
#     if(Condition[[i]]==1){
#       sumused=sumused+max(Price[[i]])
#       countused=countused+1
#       Priceused=c(Priceused,max(Price[[i]]))
#     }
#   }
#   mnew=sumnew/countnew
#   mused=sumused/countused
#   Eprice=c(mnew,mused)
#   Pricediff=Pricelive
#   for( i in 1: length(Pricelive)){
#     for(j in 1: length(Pricelive[[i]])){
#       Pricediff[[i]][j]=Eprice[Condition[[i]]+1]-Pricelive[[i]][j]
#     }
#   }
}

## F: Get CDF: F(t)
{
F=NULL
F.temp=0
for(i in 1:index){
  for(j in 1:(length(Price[[i]])-1)){
    F.temp=c(F.temp,j/(length(Price[[i]])-1))
  }
  F[[i]]=F.temp
  F.temp=0
}
}
 
## RecentTime: Mean Rencent Biding interval, at most 5 bids are counted
{
#   RecentTime=NULL
#   cut=5
#   for (j in 1:index){
#     RecentTime.temp = rep(0,1)
#     if(length(Time[[j]])>1){
#       for (k in 2:length(Price[[j]])){
#         ## eg. when cut =5, only from the 7th observation could use 5 intervals
#         if(k<cut+1){kk=k-1}
#         else   {kk=cut}
#         interval=rep(0,cut)
#         for(ii in 1:kk)
#         {
#           interval[ii]= Time[[j]][k-ii+1]-Time[[j]][k-ii]
#         }
#         RecentTime.temp = c( RecentTime.temp, sum(interval)/kk)
#       }
#     }
#      RecentTime[[j]]=RecentTime.temp
#   }
}

#All related information in each observation is saved to an element in EBAY
#Only consider 7-day auction
#Delete observation with bidding number < 7
{
EBAY=NULL
j=0
for(i in 1:index){
  j=j+1
  if (Length[[i]]==7){
    EBAY[[j]]=list(Time=Time[[i]],
                   Price=Pricelive[[i]],
                   Pricediff=max(Price[[i]])-Pricelive[[i]],
                   F=F[[i]],
                   Condition=Condition[[i]],
                   Reserve=Reserve[[i]],
                   Seller=Rating.seller[[i]], 
                   Bidder=Rating.bidder[[i]],
                   id = id.bidder[[i]],
                   Early=Early[[i]],
                   Jump=Jump[[i]],
                   Value=Value[[i]],
    			   BidAmount = Price.raw[[i]])

  }
  else{ j = j-1  }
}
}
###########################################################
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

