    print("start looking for r")
    len.sim <- 80
    sub <- which(seq(1:length(plotpoints)) %% 10 == 0)
    plotpoints.sub <- plotpoints[sub]
    dens.sub <- dens[,sub]
	rsq <- matrix(0, len.sim, length(plotpoints.sub))
	for(i in 1:len.sim){
	    r <- seq(-2, 0, len = len.sim)[i] 
		FuncReg <- reg.ebay(EBAY,dens = dens.sub, plotpoints=plotpoints.sub, r, reduced = FALSE)
		rsq[i, ] <- plot.rsq(FuncReg, plotpoints.sub, noplot = TRUE)
		cat(".")	
	}
    save(rsq, file = paste("Find-R-", Lambda.type, Fit.type, "h0=", h0, ".rda"))
    whichmax <- which.max(apply(rsq, 1, mean))
    r <- seq(-2, 0, len = len.sim)[whichmax]
	print("best r is:")
	print(r)
	
	####################################################
	plot(seq(-2, 0, len = len.sim), 
		apply(rsq,1,mean), 
		type = "l",
		xlab = expression(beta), 
		ylab = "Mean adj-R square")