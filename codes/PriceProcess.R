findprice <- function(data){
	PriceInc1=NULL
    PriceInc2=NULL
    PriceInc.temp=NULL
    nv1=0
    nv2=0
    for(i in 1:length(data)){

      if(data[[i]]$Value==1 && length(data[[i]]$Time)>= 3){
         nv1=nv1+1
         PriceInc.temp=0
         k=StepCount(data[[i]]$Time,data[[i]]$Time,6)
         if(k > 1 && length(data[[i]]$Price) != k){
           for(j in k:(length(data[[i]]$Price)-1)){
              Inc=data[[i]]$Price[j+1]-data[[i]]$Price[j]
              EInc=increment(data[[i]]$Price[j])
              PriceInc.temp=c(PriceInc.temp, Inc)
             }
         }
    if(k==1){
        for(j in 2:(length(data[[i]]$Price)-1)){
          Inc=data[[i]]$Price[j+1]-data[[i]]$Price[j]
          EInc=increment(data[[i]]$Price[j])
          PriceInc.temp=c(PriceInc.temp, Inc)
        }
     }
     PriceInc1[[nv1]]=PriceInc.temp
   }
  
   if(data[[i]]$Value==0 && length(data[[i]]$Time)>= 3){
    nv2=nv2+1
    PriceInc.temp=0
    k=StepCount(data[[i]]$Time,data[[i]]$Time,6)
    if(k > 1 && length(data[[i]]$Price) != k){
      for(j in k:(length(data[[i]]$Price)-1)){
        Inc=data[[i]]$Price[j+1]-data[[i]]$Price[j]
        EInc=increment(data[[i]]$Price[j])
        PriceInc.temp=c(PriceInc.temp, Inc)
      }
    }
    if(k==1){
      for(j in 2:(length(data[[i]]$Price)-1)){
        Inc=data[[i]]$Price[j+1]-data[[i]]$Price[j]
        EInc=increment(data[[i]]$Price[j])
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
return(list(IncALL1 = IncALL1, 
			IncALL2 = IncALL2))
}

###############################################################################################################
findprice.cl <- function(data){
PriceInc1=NULL
IncTime1=NULL
PriceInc2=NULL
IncTime2=NULL
PriceInc.temp=NULL
IncTime.temp=NULL
nv1=0
nv2=0
for(i in 1:length(data)){
  
  if(data[[i]]$Value==1 && length(data[[i]]$Time)>= 3){
    nv1=nv1+1
    PriceInc.temp=0
    IncTime.temp=0
    k=StepCount(data[[i]]$Time,data[[i]]$Time,6)
    if(k > 1 && length(data[[i]]$Price) != k){
      for(j in k:(length(data[[i]]$Price)-1)){
        Inc=data[[i]]$Price[j+1]-data[[i]]$Price[j]
        IncTime.temp=c(IncTime.temp,data[[i]]$Time[j+1])
        PriceInc.temp=c(PriceInc.temp, Inc)
      }
    }
    if(k==1){
      for(j in 2:(length(data[[i]]$Price)-1)){
        Inc=data[[i]]$Price[j+1]-data[[i]]$Price[j]
        IncTime.temp=c(IncTime.temp,data[[i]]$Time[j+1])
        PriceInc.temp=c(PriceInc.temp, Inc)
      }
    }
    PriceInc1[[nv1]]=PriceInc.temp
    IncTime1[[nv1]]=IncTime.temp
  }
  
  if(data[[i]]$Value==0 && length(data[[i]]$Time)>= 3){
    nv2=nv2+1
    PriceInc.temp=0
    IncTime.temp=0
    k=StepCount(data[[i]]$Time,data[[i]]$Time,6)
    if(k > 1 && length(data[[i]]$Price) != k){
      for(j in k:(length(data[[i]]$Price)-1)){
        Inc=data[[i]]$Price[j+1]-data[[i]]$Price[j]
        IncTime.temp=c(IncTime.temp,data[[i]]$Time[j+1])
        PriceInc.temp=c(PriceInc.temp, Inc)
      }
    }
    if(k==1){
      for(j in 2:(length(data[[i]]$Price)-1)){
        Inc=data[[i]]$Price[j+1]-data[[i]]$Price[j]
        IncTime.temp=c(IncTime.temp,data[[i]]$Time[j+1])
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
return(list(Inc1.class = Inc1.class, 
			Inc2.class = Inc2.class))
}