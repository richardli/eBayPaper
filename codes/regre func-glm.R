reg.ebay <- function(data = EBAY, 
					 plotpoins, 
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
	last.coef <- rep(1,10) 
	for( j in 1:NT.grid){
		t <- rep(0, N.data)
		for(i in 1:N.data){
			#Y[i] <- dens[i, j]
			# let Y[i] be index function?
			Y[i] <- binary[i,j]
			value[i] <- data[[i]]$Value
			reserve[i] <- data[[i]]$Reserve
			condition[i] <- data[[i]]$Condition
			seller[i] <- data[[i]]$Seller
			bidder[i] <- StepCheck(data[[i]]$Time, data[[i]]$Bidder, plotpoints[j])
			early[i] <- data[[i]]$Early
			jump[i] <- StepCheck(data[[i]]$Time, data[[i]]$Jump, plotpoints[j])
			open[i] <- data[[i]]$Price[1]
			
			current.time <- plotpoints[j]
			current.num <- length(which(data[[i]]$Time <= current.time))
			# since Time start at 0, current.time is always at least 1
			for(k in 1:current.num){
				t[i] <- t[i] + exp(r * (current.time - data[[i]]$Time[k]))
			}
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
				           Early.Bidding=early)
                           #Opening.Price=open
                           
        }
#   
#    allv <- data.frame(Self.Exciting=t,  Price.Relative=PR,  Value=value,
#                   Condition=condition)
  FuncReg[[j]] <-  glm(Y~.-1,data=allv,na.action=na.exclude, family = poisson)
  last.coef <- FuncReg[[j]]$coefficients
	}				
	return(FuncReg)
}

plot.rsq <- function(FuncReg, plotpoints, noplot){
	rsq <- rep(0, length(FuncReg))
	for(i in 1:length(FuncReg)){
		rsq[i] <- summary(FuncReg[[i]])$adj.r.squared
	}
	if(!noplot) plot(plotpoints, rsq, type = "l", ylim = c(0, 1))
	return(rsq)
}

plot.beta <- function(FuncReg, plotpoints, order, noplot){
	beta <- rep(0, length(FuncReg))
	for(i in 1:length(FuncReg)){
		if(summary(FuncReg[[i]])$aliased[order] == FALSE){			
	     	beta[i] <- summary(FuncReg[[i]])$coefficients[order, 4]
		}
	}
	if(!noplot) plot(plotpoints[240:length(plotpoints)], beta[240:length(plotpoints)], 
					 type = "l", 
					 main = rownames(summary(FuncReg[[100]])$coefficients)[order], 
					 xlab = "time", 
					 ylab = "p-value", 
					 ylim = c(0, 1), 
					 cex.main = 0.7)
	return(beta)
}

plot.beta.coef <- function(FuncReg, plotpoints, order, noplot){
	beta <- rep(0, length(FuncReg) )
	beta.sd <- rep(0, length(FuncReg) )
	for(i in 1:length(FuncReg)){		
		if(summary(FuncReg[[i]])$aliased[order] == FALSE){	
		   beta[i] <- summary(FuncReg[[i]])$coefficients[order, 1]
		   beta.sd[i] <- summary(FuncReg[[i]])$coefficients[order, 2]
		   }
	}
	if(!noplot){
		y.min <- min((beta - 1.96*beta.sd)[240:length(plotpoints)], 0)
		y.max <- max((beta + 1.96*beta.sd)[240:length(plotpoints)])
		pl <- data.frame(xx = plotpoints[240:length(plotpoints)], 
						 yy = beta[240:length(plotpoints)], 
						 yy.min = beta[240:length(plotpoints)] - 
	            1.96 * beta.sd[240:length(plotpoints)], 
	                     yy.max = beta[240:length(plotpoints)] + 
	            1.96 * beta.sd[240:length(plotpoints)])
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