# ############################
# ##   input variable here ###
# ############################
# directory <- "~/Dropbox/ebay-compare"
# version <- 1
# seed.sample <- 100 
# Lambda.Type.All <- c("betafitting", "kernel", "var-kernel")
# Fit.Type.All <- c( "pointwise", "functional")

# Lambda.type <- Lambda.Type.All[1]
# Fit.type <- Fit.Type.All[1]
# FitAll <- FALSE
# ## for kernel methods
# h0 <- 1
# plotpoints <- seq(0, 7, len = 40000)

# ## whether this is to find r first
# Find.r <- FALSE
# r.import <-  -0.6025641 # when h0 = 1, kernel
# r.import <-  -1.607692 # when h0 = 0.5, kernel
# r.import <-   # when h0 = 0.2, kernel

# r.import <-  -0.6025641 # when h0 = 1, var-kernel
# r.import <-  -1.607692 # when h0 = 0.5, var-kernel
# r.import <-   # when h0 = 0.2, var-kernel

# ## whether to do FDA approach when forecasting
# forecast.fda <- TRUE
# ## whether to skip fitting process (when using some pre-worked workspace)
# only.forecast <- FALSE

# image prefix-
# paste(version, Lambda.type, Fit.type, "h0=", h0, "r=", r.import, ".RData")
#########################################################################
#install.packages(c("fda", "quantreg", "abind"))
#install.packages(c("optimx","BB","ucminf","Rcgmin","quadprog","Rvmmin","minqa", "Rcpp"))

require(fda)
require(quantreg)
library(ggplot2)

source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/DataPreparation.R', chdir = TRUE)
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/reflection density.R', chdir = TRUE)
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/BetafittingFunction.R', chdir = TRUE)
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/regre func.R', chdir = TRUE)
setwd(directory)

############################
dens <- matrix(0, length(EBAY),length(plotpoints))
if(Lambda.type == "betafitting"){
	for(i in 1: length(EBAY) ){
		fit <- betafitting(x = EBAY[[i]]$Time, 
						y = seq(1:length(EBAY[[i]]$Time))/length(EBAY[[i]]$Time), 
						plotpoints = plotpoints, 
						START = 0, END = 7)
		dens[i, ] <- fit * length(EBAY[[i]]$Time)
	}
}else if(Lambda.type == "kernel"){
	for(i in 1: length(EBAY) ){
		density <- reflect.sm(EBAY[[i]]$Time, plotpoints, bandwidth = h0) 
		dens[i, ] <- density$yy * length(EBAY[[i]]$Time)	
	}
}else if(Lambda.type == "var-kernel"){
	for(i in 1: length(EBAY) ){
		density <- reflect.akj(EBAY[[i]]$Time, plotpoints, h = h0, alpha = 0.3) 
		dens[i, ] <- density$yy * length(EBAY[[i]]$Time)	
	}
}
############################
if(Find.r == TRUE){
	source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/findR.R', chdir = TRUE)	
}else{
	r <- r.import
}
############################
if(!skipLargeReg){
	if(Fit.type == "pointwise"){
		    FuncReg <- reg.ebay(EBAY, dens, plotpoints, r, reduced = FALSE)
			rsq <- plot.rsq(FuncReg, plotpoints, noplot = TRUE)
			beta.coef <- matrix(0, length(FuncReg[[1]]$coef), length(plotpoints))
			for(i in 1:length(FuncReg[[1]]$coef)){	
				beta.coef[i, ] <- plot.beta.coef(FuncReg, plotpoints, order = i, noplot = TRUE)[, 1]
			}
			########################################
			pdf(paste(directory, "/fig-allsig", version, Lambda.type, Fit.type, "h0=", h0, "r=", r, ".pdf", sep = ""), width=10, height= 6 )
			par(mfrow = c(5,2), mar=c(3, 4, 1, 1) + 0.1)	
			for(i in 1:length(FuncReg[[1]]$coef)){	
				beta <- plot.beta(FuncReg, plotpoints, order = i, noplot = FALSE)
				abline(h = 0.05, col = "red")
			}
			dev.off()
			
			pdf(paste(directory, "/fig-allcoef", version, Lambda.type, Fit.type, "h0=", h0, "r=", r, ".pdf", sep = ""), width=10, height= 6 )
			 ppp <- NULL
			for(i in 1:length(FuncReg[[1]]$coef)){	
				 ppp[[i]] <- plot.beta.coef(FuncReg, plotpoints, order = i, noplot = FALSE)
			 }
			 multiplot(ppp[[1]], ppp[[2]], ppp[[3]], ppp[[4]],  ppp[[5]], ppp[[6]], 
						 ppp[[7]], ppp[[8]], ppp[[9]], ppp[[10]], cols = 5)
			dev.off()
	}else{
			FuncReg <- reg.ebay.functional(EBAY, dens, plotpoints, r, reduced = FALSE)
			beta.coef <- FuncReg$coef
			beta.coef.lower <- FuncReg$low
			beta.coef.upper <- FuncReg$up
			pdf(paste(directory, "/fig-allcoef-func", version, Lambda.type, Fit.type, "h0=", h0, "r=", r, ".pdf", sep = ""), width=10, height= 6 )
			par(mfrow = c(5,2), mar=c(3, 4, 1, 1) + 0.1)
     		ppp <- NULL
			for(i in 1:length(FuncReg[[1]]$coef)){	
				ppp[[i]] <- plot.coef.only(plotpoints, 
											beta = beta.coef, 
											beta.low = beta.coef.lower, 
											beta.up = beta.coef.upper)
			}
			multiplot(ppp[[1]], ppp[[2]], ppp[[3]], ppp[[4]],  ppp[[5]], ppp[[6]], 
						 ppp[[7]], ppp[[8]], ppp[[9]], ppp[[10]], cols = 5)
			dev.off()
	}
	print("first regression (all variables) done!")
	
save.image(paste(directory, "/fullregre.RData", sep = ""))
}
############################
	set.seed(seed.sample)
	n=round(length(EBAY)*.7)
	a1=sample(c(1:length(EBAY)),n)
	a2=setdiff(c(1:length(EBAY)), a1)
	EBAY.training=NULL
	for(i in 1:length(a1)){
	  EBAY.training[[i]]=EBAY[[a1[i]]]
	}
	EBAY.testing=NULL
	for(i in 1:length(a2)){
	  EBAY.testing[[i]]=EBAY[[a2[i]]]
	}
	dens <- dens[a1, ]

############################
if(Fit.type == "pointwise"){
	FuncReg <- reg.ebay(EBAY.training, dens, plotpoints, r, reduced = TRUE)
	rsq <- plot.rsq(FuncReg, plotpoints, noplot = TRUE)
	beta.coef <- matrix(0, length(FuncReg[[1]]$coef), length(plotpoints))
	for(i in 1:length(FuncReg[[1]]$coef)){	
		beta.coef[i, ] <- plot.beta.coef(FuncReg, plotpoints, order = i, noplot = TRUE)[, 1]
	}
	########################################
# # 	pdf(paste(directory, "/reduced-allsig", version, Lambda.type, Fit.type, "h0=", h0, "r=", r, ".pdf", sep = ""), width=10, height= 6 )
	# par(mfrow = c(2,2), mar=c(3, 4, 1, 1) + 0.1)	
	# for(i in 1:length(FuncReg[[1]]$coef)){	
		# beta <- plot.beta(FuncReg, plotpoints, order = i, noplot = FALSE)
		# abline(h = 0.05, col = "red")
	# }
	# dev.off()
	
	pdf(paste(directory, "/fig-reducedcoef", version, Lambda.type, Fit.type, "h0=", h0, "r=", r, ".pdf", sep = ""), width=10, height= 6 )
	 ppp <- NULL
	for(i in 1:length(FuncReg[[1]]$coef)){	
		 ppp[[i]] <- plot.beta.coef(FuncReg, plotpoints, order = i, noplot = FALSE)
	 }
	 multiplot(ppp[[1]], ppp[[2]], ppp[[3]], ppp[[4]], cols = 2)#, ppp[[5]], ppp[[6]], 
				# ppp[[7]], ppp[[8]], ppp[[9]], ppp[[10]], cols = 5)
	dev.off()

	print("regression done!")	
}else{
	FuncReg <- reg.ebay.functional(EBAY.training, dens, plotpoints, r, reduced = TRUE)
	beta.coef <- FuncReg$coef
	beta.coef.lower <- FuncReg$low
	beta.coef.upper <- FuncReg$up
	pdf(paste(directory, "/fig-reducedcoef-func", version, Lambda.type, Fit.type, "h0=", h0, "r=", r, ".pdf", sep = ""), width=10, height= 6 )
	par(mfrow = c(5,2), mar=c(3, 4, 1, 1) + 0.1)
	     		ppp <- NULL
	for(i in 1:length(FuncReg[[1]]$coef)){	
		ppp[[i]] <- plot.coef.only(plotpoints, 
									beta = beta.coef, 
									beta.low = beta.coef.lower, 
									beta.up = beta.coef.upper)
	}
	multiplot(ppp[[1]], ppp[[2]], ppp[[3]], ppp[[4]],  cols = 2)
	dev.off()
	
}

#################################################################################

	source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/New MAPE.R', chdir = TRUE)
	source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/PriceProcess.R', chdir = TRUE)
	
	mape1 <- mape(EBAY.testing, data.train = EBAY.training, beta.coef, r, plotpoints, Start = 6, toend = FALSE, sim = 500)
	if(forecast.fda){
		source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/FDA_Spline.R', chdir = TRUE)
		mape1.fda <- MAPE2nd
	}
	mape2 <- mape(EBAY.testing, data.train = EBAY.training, beta.coef, r, plotpoints, Start = 6, toend = TRUE, sim = 500)
	if(forecast.fda){
		source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/FDA_Spline_toend.R', chdir = TRUE)
		mape2.fda <- MAPE2nd
	}	

	
mape <- list(mape1, mape2)
save(mape, file = paste(directory, "/mape", version, Lambda.type, Fit.type, "h0=", h0, "r=", r, ".Rda", sep = ""))
save.image(paste(directory, "/workspace", version, Lambda.type, Fit.type, "h0=", h0, "r=", r, ".RData", sep = ""))
