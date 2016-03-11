source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/PriceProcess.R', chdir = TRUE)

mape <- function(data, data.train = data,  beta.coef, r, plotpoints, Start = 6, toend, sim = 500 ){
	
  if(toend == FALSE){
  	sep <- 0.1
  	horizon <- seq(Start, 7-sep, by = sep)
  	record.prob=matrix(0,10,19)
  	}
  if(toend == TRUE){
  	horizon <- c(7-2/24,7-1/24,7-.5/24,7-.25/24,7-5/60/24,7-1/60/24) 
  	record.prob=matrix(0,6,19)
  }
  #######################################################	
  p1 <- p2 <- n1 <- n2 <- 0
	for(i in 1:length(data.train)){
		if(data.train[[i]]$Value == 1){
			p1 <- p1 + max(data.train[[i]]$Price)
			n1 <- n1 + 1
		}
		if(data.train[[i]]$Value == 0){
			p2 <- p2 + max(data.train[[i]]$Price)
			n2 <- n2 + 1
		}
	}
	p1 <- p1 / n1
	p2 <- p2 / n2
	######################################################
    Inc <- findprice(data.train)
    IncALL1 <- Inc$IncALL1
    IncALL2 <- Inc$IncALL2
    Inc.class <- findprice.cl(data.train)
    Inc1.class <- Inc.class$Inc1.class
    Inc2.class <- Inc.class$Inc2.class
	############################################################
  MAPE <- matrix(0, length(horizon), 3)	
  
  for(case in 1:length(horizon)){
  	  recordn=matrix(0,length(data),19)
	  current <- horizon[case]
	  APE <- rep(0, length(data))
	for(i in 1:length(data)){
	  	
	  	price.sim <- rep(0, sim)
	  	for(j in 1:sim){
	  		if(toend == FALSE) end <- current + sep
	  		if(toend == TRUE) end <- 7
	  		sim.one <- SimSelf(start     = current, 
	  					       end       = end,
	  					       data      = data[[i]], 
	  					       beta.coef = beta.coef,
	  					               r = r,     
	  					       plotpoints = plotpoints, 
	  					       p1 = p1, 
	  					       p2 = p2, 
	  					       IncALL1  = IncALL1, 
	  					       IncALL2  = IncALL2,
	  					       Inc1.class = Inc1.class, 
	  					       Inc2.class = Inc2.class, 
	  					       toend = toend, 
	  					       case  = case )
	  		price.sim[j] <- max(sim.one[, 2])
	  	}
	  	price.true <- StepCheck(data[[i]]$Time, 
	  							data[[i]]$Price, 
	  							end)
	  	APE[i] <- (mean(price.sim) - price.true)/price.true
	 	    	  for(nn in 1:19){
                low=nn/40
      			up=1-nn/40
     		    lowbound=quantile(price.sim,low)
    	        upbound=quantile(price.sim,up)
                if(price.true>=lowbound && price.true<=upbound){
        recordn[i,nn]=1
      }
    } 	
	  }
	  MAPE[case, 1] <- mean(abs(APE))
	  MAPE[case, 2] <- quantile(abs(APE), 0.05)
	  MAPE[case, 3] <- quantile(abs(APE), 0.95)	
	  record.prob[case,] <- apply(recordn,2,mean) 
	  print(paste("MAPE done ", case, " of ", length(horizon))) 
  }
  return(list(MAPE, record.prob))
}

SimSelf <- function(start, end, data, beta.coef, r, plotpoints, p1 = 134.6, p2 = 11.6, IncALL1 = all1, IncALL2 = all2, Inc1.class = cl1, Inc2.class = cl2, toend = TRUE, case){
	# start/end is the actual time
	# first find the corresponding index in plotpoints
	start.index <- trunc(start/(7/(length(plotpoints) )))
	end.index <- trunc(end/(7/(length(plotpoints) )))
	s <- 0
	t <- start
	index <- start.index + 1
	sumZ <- 0
	value <- data$Value
	if(value == 1) WTP <- p1
	if(value == 0) WTP <- p2
	price <- StepCheck(data$Time, data$Price, start)
	history.time <- data$Time[data$Time <= start]
	history.price <- data$Price[data$Time <= start]
	current.number <- length(history.time) + length(t) - 1
	z <- c(0, WTP-price, data$Condition, data$Early, data$Price[1])
	
	while(index < end.index || (index==(end.index) && sumZ>=s)){
		u <- runif(1)
		s <- s- log(u)
		for(i in index : end.index){
			if(sumZ >= s){
         	    if(toend==TRUE){
         		   if(value==1) inc=sample(IncALL1, size=1)
          		   if(value==0) inc=sample(IncALL2, size=1)
                 }
               if(toend==FALSE){
                   if(value==1) inc=sample(Inc1.class[[case]], size=1)
                   if(value==0) inc=sample(Inc2.class[[case]], size=1)
                  }
                if(current.number==1) inc=0
                #if(max(price)+inc > 1.3*max(price)) jump=jump+1
                price=c(price,max(price)+inc)
                ##########################################################
                 z=c(0,WTP-max(price),data$Condition, data$Early,data$Price[1])
                #########################################################
                 break
            }
            if(i != end.index){
            	##############################################
                gt=0
        		current.time=plotpoints[i]
        		if(current.number !=0){
        			for(k in 1:current.number){
          				gt=gt+exp((plotpoints[i]-history.time[k])*r)
        			}
        		}
        		##############################################
        		z[1]=gt
        		sumZ=sumZ+(z %*% beta.coef[,i])*7/(length(plotpoints) ) 
            }
		}
	 if(length(price)>length(t)){t=c(t,plotpoints[i] )}
     index <- i
     current.number <- current.number+1
     history.time <- c(history.time,t[2:length(t)])
  } 
  return(cbind(t,price))	
}