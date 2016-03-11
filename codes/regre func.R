require(abind)
reg.ebay <- function(data = EBAY, 
					 dens,
					 plotpoints, 
					 r, 
					 reduced = FALSE){
	NT.grid <- length(plotpoints)
	N.data <- length(data)
	###############################################
	p1 <- p2 <- n1 <- n2 <- 0
	for(i in 1:length(data)){
		if(data[[i]]$Value == 1){
			p1 <- p1 + max(data[[i]]$Price)
			n1 <- n1 + 1
		}
		if(data[[i]]$Value == 0){
			p2 <- p2 + max(data[[i]]$Price)
			n2 <- n2 + 1
		}
	}
	p1 <- p1 / n1
	p2 <- p2 / n2
	###############################################
	Y <- value <- reserve <-condition <- seller <- bidder <- early <- jump <- open <- PR <- rep(0, N.data)
	FuncReg <- NULL 
	for( j in 1:NT.grid){
		if(j %% 1000 == 0) cat("-")
		t <- rep(0, N.data)
		for(i in 1:N.data){
			Y[i] <- dens[i, j]
			value[i] <- data[[i]]$Value
			reserve[i] <- data[[i]]$Reserve
			condition[i] <- data[[i]]$Condition
			seller[i] <- data[[i]]$Seller
			bidder[i] <- StepCheck(data[[i]]$Time, data[[i]]$Bidder, plotpoints[j])
			early[i] <- data[[i]]$Early
			jump[i] <- StepCheck(data[[i]]$Time, data[[i]]$Jump, plotpoints[j])
			open[i] <- data[[i]]$Price[1]
			
			arr <- data[[i]]$Time[data[[i]]$Time < plotpoints[j] ]
			t[i] <- sum(exp(r * (plotpoints[j] - arr)))	
			
			current.p <- StepCheck(data[[i]]$Time, data[[i]]$Price, plotpoints[j])
			if(data[[i]]$Value == 1) WTP <- p1
			if(data[[i]]$Value == 0) WTP <- p2
			PR[i] <- WTP - current.p			
		}
		allv <- data.frame(Self.Exciting=t,  
						   Price.Relative=PR,
				           Value=value,
						   Reserve = reserve,
				           Condition=condition,
				           Early.Bidding=early,
				           Seller = seller, 
				           Bidder = bidder, 
                           Jump.Bidding=jump, 
                           Opening.Price=open)
        if(reduced){
        	allv <- data.frame(Self.Exciting=t,  
						   Price.Relative=PR,
						   #Reserve = reserve,
				           Condition=condition,
				           Early.Bidding=early,
                           Opening.Price=open)
                           
        }
#   
#    allv <- data.frame(Self.Exciting=t,  Price.Relative=PR,  Value=value,
#                   Condition=condition)
  FuncReg[[j]] <-  lm(Y~.-1,data=allv,na.action=na.exclude)
	}				
	return(FuncReg)
}
#################################
reg.ebay.functional <- function(data = EBAY, 
					 dens,
					 plotpoints, 
					 r, 
					 reduced = FALSE){
	NT.grid <- length(plotpoints)
	N.data <- length(data)
	###############################################
	p1 <- p2 <- n1 <- n2 <- 0
	for(i in 1:length(data)){
		if(data[[i]]$Value == 1){
			p1 <- p1 + max(data[[i]]$Price)
			n1 <- n1 + 1
		}
		if(data[[i]]$Value == 0){
			p2 <- p2 + max(data[[i]]$Price)
			n2 <- n2 + 1
		}
	}
	p1 <- p1 / n1
	p2 <- p2 / n2
	###############################################
	Y <- value <- reserve <-condition <- seller <- bidder <- early <- jump <- open <- PR <- self <- matrix(0, length(plotpoints), N.data)
	FuncReg <- NULL 
	for( j in 1:NT.grid){
		for(i in 1:N.data){
			Y[j, i] <- dens[i, j]
			value[j, i] <- data[[i]]$Value
			reserve[j, i] <- data[[i]]$Reserve
			condition[j, i] <- data[[i]]$Condition
			seller[j, i] <- data[[i]]$Seller
			bidder[j, i] <- StepCheck(data[[i]]$Time, data[[i]]$Bidder, plotpoints[j])
			early[j, i] <- data[[i]]$Early
			jump[j, i] <- StepCheck(data[[i]]$Time, data[[i]]$Jump, plotpoints[j])
			open[j, i] <- data[[i]]$Price[1]
			
			arr <- data[[i]]$Time[data[[i]]$Time < plotpoints[j] ]
			self[j, i] <- sum(exp(r * (plotpoints[j] - arr)))	
			
			# current.num <- length(which(data[[i]]$Time <= current.time))
			# # since Time start at 0, current.time is always at least 1
			# for(k in 1:current.num){
				# self[j, i] <- self[j, i] + exp(r * (current.time - data[[i]]$Time[k]))
			# }
			current.p <- StepCheck(data[[i]]$Time, data[[i]]$Price, plotpoints[j])
			if(data[[i]]$Value == 1) WTP <- p1
			if(data[[i]]$Value == 0) WTP <- p2
			PR[j, i] <- WTP - current.p			
		}
	}
	numplot <- length(plotpoints)
	norder <- 5
	nbasis <- 9
	lambda.sm <- 0.1
	numauct<-N.data
	wbasis <- create.bspline.basis(rangeval=c(0,7),nbasis=nbasis, 
												   norder = norder)
	
	yfdPar  =fdPar(wbasis, 2, lambda.sm)
	yfdobj <- smooth.basis(plotpoints, Y, yfdPar) 
    yfd     = yfdobj$fd
    #plot(yfd)
	xbasis <- create.bspline.basis(rangeval = c(0,7), nbasis = nbasis,
														norder = norder)
	if(reduced == FALSE){
		allv <- abind(self, PR, value, reserve, condition, seller, 
		              bidder, early, jump, open, along = 3)
		title <- c("Self.Exciting", "Price.Relative", "Value", "Reserve", "Condition",
					 "Seller", "Bidder", "Early.Bidding", "Jump.Bidding", "Opening.Price")
	}else{
		allv <- abind(self, PR, condition, early, along = 3)
		title <- c("Self.Exciting", "Price.Relative", "Condition", "Early.Bidding")
	}
	betaList <- vector("list", dim(allv)[3])
	for(i in 1:length(betaList)){
		betaList[[i]] <- xbasis
	}
	xList <- vector("list", dim(allv)[3])
	for(i in 1:length(xList)){
		xList[[i]] <- smooth.basis(plotpoints, allv[ , , i], yfdPar)$fd
	}
	fRegressList = fRegress(yfd, xList, betaList)
	betaestList  = fRegressList$betaestlist

    yhatmat = eval.fd(plotpoints, yfd)
  	rmatb   = Y - yhatmat
  	SigmaE = var(t(rmatb))
  	y2cMap = yfdobj$y2cMap

  	fRegressList = fRegress(yfd, xList, betaList, y2cMap=y2cMap, SigmaE = SigmaE)
    fRegressList2 = fRegress.stderr(fRegressList, y2cMap, SigmaE)
    betastderrList = fRegressList2$betastderrlist
    stdList <- vector("list", length(betastderrList))
    for(i in 1:length(betastderrList)) stdList[[i]] <- eval.fd(plotpoints, betastderrList[[i]])
    #########################################
    coefs <- matrix(0, length(plotpoints), dim(allv)[3])
    ci.low <- matrix(0, length(plotpoints), dim(allv)[3])
    ci.up <- matrix(0, length(plotpoints), dim(allv)[3])
    for(i in 1:dim(allv)[3]){
    	coefs[, i] <- eval.fd(plotpoints, betaestList[[i]]$fd)
    	ci.low[, i] <- coefs[, i]  - 2 * eval.fd(plotpoints, betastderrList[[i]])
    	ci.up[, i] <- coefs[, i]  + 2 * eval.fd(plotpoints, betastderrList[[i]]) 	
    } 
    return(list(coef = coefs, 
    			low = ci.low, 
    			up = ci.up))		
}
#################################
plot.rsq <- function(FuncReg, plotpoints, noplot){
	rsq <- rep(0, length(FuncReg))
	for(i in 1:length(FuncReg)){
		rsq[i] <- summary(FuncReg[[i]])$adj.r.squared
	}
	if(!noplot) plot(plotpoints, rsq, type = "l", ylim = c(0, 1))
	return(rsq)
}
#################################
plot.beta <- function(FuncReg, plotpoints, order, noplot){
	beta <- rep(0, length(FuncReg))
	for(i in 1:length(FuncReg)){
		if(summary(FuncReg[[i]])$aliased[order] == FALSE){		
 		    offset <- length(which(summary(FuncReg[[i]])$aliased[1:order] ==TRUE ))		
	     	beta[i] <- summary(FuncReg[[i]])$coefficients[order - offset, 4]
		}
	}
	if(!noplot) plot(plotpoints[240:length(plotpoints)], beta[240:length(plotpoints)], 
					 type = "l", 
					 main = rownames(summary(FuncReg[[100]])$coefficients)[order], 
					 xlab = "time", 
					 ylab = "p-value", 
					 ylim = c(0, 1), 
					 cex.main = 1)
	return(beta)
}
#################################
plot.beta.coef <- function(FuncReg, plotpoints, order, noplot){
	beta <- rep(0, length(FuncReg) )
	beta.sd <- rep(0, length(FuncReg) )
	for(i in 1:length(FuncReg)){		
		if(summary(FuncReg[[i]])$aliased[order] == FALSE){
		   offset <- length(which(summary(FuncReg[[i]])$aliased[1:order] ==TRUE ))	
		   beta[i] <- summary(FuncReg[[i]])$coefficients[order - offset, 1]
		   beta.sd[i] <- summary(FuncReg[[i]])$coefficients[order - offset, 2]
		   }
	}
	if(!noplot){
		y.min <- min((beta - 1.96*beta.sd)[240:(length(plotpoints) - 200)], 0)
		y.max <- max((beta + 1.96*beta.sd)[240:(length(plotpoints) - 200)])
		pl <- data.frame(xx = plotpoints[240:(length(plotpoints) - 200)], 
						 yy = beta[240:(length(plotpoints) - 200)], 
						 yy.min = beta[240:(length(plotpoints) - 200)] - 
	            1.96 * beta.sd[240:(length(plotpoints) - 200)], 
	                     yy.max = beta[240:(length(plotpoints) - 200)] + 
	            1.96 * beta.sd[240:(length(plotpoints) - 200)])
	ppp <- ggplot(pl, aes(xx))+
	      	geom_line(aes(y = yy), color = "blue")+
	      	geom_ribbon(aes(ymin = yy.min, ymax = yy.max), alpha = 0.2)+
	      	geom_hline(yintercept = 0, color = "red") +
	      	xlab("time")+
	      	ylab("coefficient")+
	      	ggtitle(rownames(summary(FuncReg[[100]])$coefficients)[order])
# # 		plot(plotpoints[240:length(plotpoints)], beta[240:length(plotpoints)], 
					 # type = "l", 
					 # main = rownames(summary(FuncReg[[10]])$coefficients)[order], 
					 # xlab = "time", 
					 # ylab = "coefficient", 
					 # ylim = c(y.min, y.max), 
					 # cex.main = 0.7)
	# # for(kk in 240:length(plotpoints)){
		# # if(kk %% (length(plotpoints)/20) == 0){
			# # points(plotpoints[kk], beta[kk] + 1.96 * beta.sd[kk], col = "red", cex = 0.6)
			# # points(plotpoints[kk], beta[kk] - 1.96 * beta.sd[kk], col = "red", cex = 0.6)
		# # }
	# # }
	
	# polygon(c(plotpoints[240:length(plotpoints)], 
	          # rev(plotpoints[240:length(plotpoints)])), 
	        # c((beta[240:length(plotpoints)] + 
	            # 1.96 * beta.sd[240:length(plotpoints)]), 
	          # rev((beta[240:length(plotpoints)] + 
	            # 1.96 * beta.sd[240:length(plotpoints)]))),
            # col = "red", border = NA)
            
# # # 		lines(plotpoints[240:length(plotpoints)], beta[240:length(plotpoints)] + 1.96 * beta.sd[240:length(plotpoints)],
			   # # col = "grey")#, type = "l", lty = 2)
		# # lines(plotpoints[240:length(plotpoints)], beta[240:length(plotpoints)] - 1.96 * beta.sd[240:length(plotpoints)],
			   # # col = "grey")#, type = "l", lty = 2)	
	} 
	if(noplot) return(cbind(beta, beta.sd))
	if(!noplot) return(ppp)
}

plot.coef.only <- function(plotpoints, beta, beta.low, beta.up){
    	y.min <- min(beta.low[240:(length(plotpoints) - 200)], 0)
		y.max <- max(beta.up[240:(length(plotpoints) - 200)])
		pl <- data.frame(xx = plotpoints[240:(length(plotpoints) - 200)], 
						 yy = beta[240:(length(plotpoints) - 200)], 
						 yy.min = beta.low[240:(length(plotpoints) - 200)], 
	                     yy.max = beta.up[240:(length(plotpoints) - 200)])
	ppp <- ggplot(pl, aes(xx))+
	      	geom_line(aes(y = yy), color = "blue")+
	      	geom_ribbon(aes(ymin = yy.min, ymax = yy.max), alpha = 0.2)+
	      	geom_hline(yintercept = 0, color = "red") +
	      	xlab("time")+
	      	ylab("coefficient")+
	      	ggtitle(rownames(summary(FuncReg[[100]])$coefficients)[order])
    return(ppp)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}