
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/reflection density.R', chdir = TRUE)
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/BetafittingFunction.R', chdir = TRUE)
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/regre func.R', chdir = TRUE)
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/reflection density.R', chdir = TRUE)

library(fda)
library(quantreg)
#######
sim.one <- function(plotpoints, base.x1,base.x2, base.z1, base.z2, beta, alpha, scale = 1){	
	x1 <- runif(1, min = 0, max = 1)
	x2 <- rbinom(1, 1, prob = 0.6)
	z1 <- rep(0, length(plotpoints))
	z2 <- rep(0, length(plotpoints))
	z1[1] <- runif(1, min = 0, max = 1)
             #rnorm(1, mean = 0, sd = 1)
	#z2[1] <- rnorm(1, mean = 1, sd = 1)
	arrival <- NULL
	s <- -log(runif(1))
	sumv <- 0
	vt <- rep(0, length(plotpoints))
	for(i in 1:length(plotpoints)){
		vt[i] <-  base.x1(plotpoints[i]) * x1 /scale+
		        		 base.x2(plotpoints[i]) * x2 /scale +
        				 base.z1(plotpoints[i]) * z1[i] /scale
				         # + base.z2(plotpoints[i]) * z2[i]) 
		if(length(arrival) > 0){
			vt[i] <- max(0, vt[i] + alpha(plotpoints[i])/scale * sum(exp(-beta * (plotpoints[i] - arrival))))
		}else{vt[i] <- max(0, vt[i])}
	
		sumv <- sumv + vt[i] * max(plotpoints) / (length(plotpoints) - 1)
		if(sumv >= s) {
			arrival <- c(arrival, plotpoints[i])
			s <- s - log(runif(1))
			z1[i + 1] <- runif(1, min = 0, max = 1)
                       # rnorm(1, mean = 0, sd = 1)
			#z2[i + 1] <- z2[i] + rnorm(1, mean = 1, sd = 1)
		}
		if(sumv < s){
			z1[i + 1] <- z1[i]
			#z2[i + 1] <- z2[i] 
		}
			
	}
	
	N <- length(arrival)
	scale <- 40 / (N + 1)
	 
return(list(bid = arrival,
			x1 = x1, 
			x2 = x2, 
			z1 = z1, 
			#z2 = z2, 
			vt = vt, 
			scale.exp = scale))
}
bestplotRange <- function(y1,y2, y3 = NULL, y4 = NULL, scale = 1){
	## y1 is the true vector
	## y2 is the estimated vector
	## y3 and y4 are CI for estimated vector
	## scale is how much higher and lower should the margin go, 
	##       compared with the true vector range
	x11 <- min(y1, na.rm = TRUE)
	x12 <- max(y1, na.rm = TRUE)
	x21 <- min(y2, na.rm = TRUE)
	x22 <- max(y2, na.rm = TRUE)
	if(!is.null(y3)){
		x31 <- min(y3, na.rm = TRUE)
     	x32 <- max(y3, na.rm = TRUE)	
	}else{x31 <- x21; x32 <- x22}
    if(!is.null(y4)){
		x41 <- min(y4, na.rm = TRUE)
     	x42 <- max(y4, na.rm = TRUE)	
	}else{x41 <- x21; x42 <- x22}
	y.min <- min(x11, x21, x31, x41)
	y.max <- max(x12, x22, x32, x42)
	if(x11-y.min > scale*(x12 - x11)) y.min <- x11 - scale*(x12 - x11)
     if(y.max -x12 > scale*(x12 - x11)) y.max <- x12 + scale*(x12 - x11)
     return(c(y.min, y.max))
}
simFromDens <- function(lambda, folder){
	err <- rep(0, length(plotpoints))
	for(i in 1:length(plotpoints)){
		err.list <- rep(0, length(bids))
		for(j in 1:length(bids)){
	     	err.list[j] <- bids[[j]]$vt[i] - lambda[j, i]	
		}
		err[i] <- mean(err.list)
	}
	##############
	pdf(paste("~/Dropbox/", folder, "/error-plots-case", case, "version", version, ".pdf",sep = ""))
	plot(err)
	dev.off()
	##############
	pdf(paste("~/Dropbox/", folder, "/sample-intensity-case", case,"version", version, ".pdf", sep = ""), width=12, height=6 )	
	par(mfrow = c(2,4))
	index <- sample(seq(1:length(bids)), 8)
	for(i in index){
		mean(bids[[i]]$vt/length(bids[[i]]$bid))
		mean(lambda[i, ]/length(bids[[i]]$bid))
		plot(plotpoints, bids[[i]]$vt,type = "l", xlab="time",ylab="intensity", 
			 main=paste("auction", i), ylim = bestplotRange(bids[[i]]$vt, lambda[i,]))
		lines(plotpoints, lambda[i,], col = "red")
		points(bids[[i]]$bid,rep(mean(bids[[i]]$vt), length(bids[[i]]$bid)),col="blue")
	}
	dev.off()	
	###############
	coef <- matrix(0, length(plotpoints), 3)
	confint <- array(0, c(length(plotpoints), 3, 2))
	rsq <- rep(0, length(plotpoints))
	for(i in 1:length(plotpoints)){
			Y <- rep(0, length(bids))
			x1 <- rep(0, length(bids))
			x2 <- rep(0, length(bids))
			z1 <- rep(0, length(bids))
			#z2 <- rep(0, length(bids))
			self <- rep(0, length(bids))
			for(j in 1:length(bids)){
				Y[j] <- lambda[j, i]
				x1[j] <- bids[[j]]$x1
				x2[j] <- bids[[j]]$x2
				z1[j] <- bids[[j]]$z1[i]
				arr <- bids[[j]]$bid[bids[[j]]$bid < plotpoints[i] ]
				self[j] <- sum(exp(-beta * (plotpoints[i] - arr)))
			}
			fit <- lm(Y ~ x1 + z1 + self - 1)
			coef[i, ] <- coef(fit)
			confint[i, , ] <- confint(fit)
			rsq[i] <- summary(fit)$adj.r.squared
	 }
	 #############
	 pdf(paste("~/Dropbox/", folder, "/rsquare-case", case, "version", version, ".pdf",sep = ""))	
	 plot(plotpoints, rsq, type = "l", ylab = "adj.r.square", xlab = "time")
	 dev.off()
	 #############
	 truncmin = trunc(length(plotpoints)/100)
	 plotrange = truncmin:(length(plotpoints) - truncmin) 
	pdf(paste("~/Dropbox/", folder, "/coef-curve-case", case, "version", version, ".pdf",sep = ""),  width=12, height=6)	
	par(mfrow = c(1,3) )
	plot(plotpoints[plotrange], base.x1(plotpoints[plotrange])/scale.coef, main = "beta1", type = "l",
		ylim = bestplotRange(base.x1(plotpoints)/scale.coef, coef[,1], 
				            confint[, 1, 1], confint[, 1, 2], scale = 0.6), xlab="time")			            
	lines(plotpoints[plotrange], coef[plotrange, 1], type = "l", col = "red")
	lines(plotpoints[plotrange], confint[plotrange, 1, 1], type = "l", col ="grey")
	lines(plotpoints[plotrange], confint[plotrange, 1, 2], type = "l", col ="grey")
	
	plot(plotpoints[plotrange], base.z1(plotpoints[plotrange])/scale.coef, main = "beta2", type = "l",
		ylim = bestplotRange(base.z1(plotpoints)/scale.coef, coef[,2], 
				            confint[, 2, 1], confint[, 2, 2]), xlab="time")
	lines(plotpoints[plotrange], coef[plotrange, 2], type = "l", col = "red")
	lines(plotpoints[plotrange], confint[plotrange, 2, 1], type = "l", col ="grey")
	lines(plotpoints[plotrange], confint[plotrange, 2, 2], type = "l", col ="grey")
	
	plot(plotpoints[plotrange], alpha(plotpoints[plotrange])/scale.coef, main = "alpha", type = "l",
		ylim = bestplotRange(base.x1(plotpoints)/scale.coef, coef[,3], 
				            confint[, 3, 1], confint[, 3, 2], scale = 0.4), xlab="time")
	lines(plotpoints[plotrange], coef[plotrange, 3], type = "l", col = "red")
	lines(plotpoints[plotrange], confint[plotrange, 3, 1], type = "l", col ="grey")
	lines(plotpoints[plotrange], confint[plotrange, 3, 2], type = "l", col ="grey") 
	dev.off()
	####################
	MAPE.beta <- rep(0, 3)
	MAPE.beta[1] <- mean(abs((coef[,1]-base.x1(plotpoints)/scale.coef)/(base.x1(plotpoints)/scale.coef)), na.rm = TRUE)
	MAPE.beta[2] <- mean(abs((coef[,2]-base.z1(plotpoints)/scale.coef)/(base.z1(plotpoints)/scale.coef)), na.rm = TRUE)
	MAPE.beta[3] <- mean(abs((coef[,3]-alpha(plotpoints)/scale.coef)/(alpha(plotpoints)/scale.coef)), na.rm = TRUE)
	return(MAPE.beta)
}
simFromDens.func <- function(lambda, folder){ 
  ########################################################### 
	Y <- matrix(0, length(plotpoints), length(bids))
		x1 <- matrix(0, length(plotpoints), length(bids))
		z1 <- matrix(0, length(plotpoints), length(bids))
		self <- matrix(0, length(plotpoints),length(bids))
		for(i in 1:length(plotpoints)){
     		for(j in 1:length(bids)){
	     		Y[i, j] <- lambda[j, i]
		     	#Y[i, j] <- bids[[j]]$vt[i] #+ rnorm(1, mean = 0, sd = 2)
				x1[i, j] <- bids[[j]]$x1
				z1[i, j] <- bids[[j]]$z1[i]
				arr <- bids[[j]]$bid[bids[[j]]$bid < plotpoints[i] ]
				self[i, j] <- sum(exp(-beta * (plotpoints[i] - arr)))
			}
		}	
	
	
	
	#knots <- c(0,1,2,3,4,5,6,6.25,6.5,6.75,6.8125,6.875,6.9375,7)
	#numk<-length(knots)
	numplot <- length(plotpoints)
	norder <- 5
	nbasis <- 9
	lambda.sm <- 0.1
	numauct<-length(bids)
	wbasis <- create.bspline.basis(rangeval=c(0,7),nbasis=nbasis, 
												   norder = norder)
	
	yfdPar  =fdPar(wbasis, 2, lambda.sm)
	yfdobj <- smooth.basis(plotpoints, Y, yfdPar) 
    yfd     = yfdobj$fd
    #plot(yfd)
	xbasis <- create.bspline.basis(rangeval = c(0,7), nbasis = nbasis,
														norder = norder)
	zbasis <- create.bspline.basis(rangeval = c(0,7), nbasis = nbasis,
														norder = norder)
	selfbasis <- create.bspline.basis(rangeval = c(0,7), 
									 nbasis = nbasis,
									 norder = norder) 						
    betaList <- list(fdPar(xbasis), fdPar(zbasis), fdPar(selfbasis))
	x1.fd <- smooth.basis(plotpoints, x1, yfdPar)$fd
	z1.fd <- smooth.basis(plotpoints, z1, yfdPar)$fd
	self.fd <- 	smooth.basis(plotpoints, self, yfdPar)$fd
	
	xList <- list(x1.fd, z1.fd, self.fd)
	fRegressList = fRegress(yfd, xList, betaList)
  
 	betaestList  = fRegressList$betaestlist
  	yFit    = fRegressList$yhatfd
	coef.name <- c("x1", "z1", "self")
	
    yhatmat = eval.fd(plotpoints, yfd)
  	rmatb   = Y - yhatmat
  	SigmaE = var(t(rmatb))
  	y2cMap = yfdobj$y2cMap

  	fRegressList = fRegress(yfd, xList, betaList, y2cMap=y2cMap, SigmaE = SigmaE)
    fRegressList2 = fRegress.stderr(fRegressList, y2cMap, SigmaE)
    betastderrList = fRegressList2$betastderrlist
    stdList <- vector("list", length(betastderrList))
    for(i in 1:length(betastderrList)) stdList[[i]] <- eval.fd(plotpoints, betastderrList[[i]])
    
	##################################################
   # par(mfrow=c(2,2),cex=1)
	  pdf(paste("~/Dropbox/", folder, "/coef-functional-case", case, "version", version, ".pdf",sep = ""),  width=12, height=6)	
	   par(mfrow = c(1, 3))
	   plotrange <- vector("list", 3)
	   for(i in 1:3){
	   	plotrange[[i]] <- bestplotRange(base.x1(plotpoints)/scale.coef,    
	   					    eval.fd(plotpoints, betaestList[[i]]$fd) , 
				            eval.fd(plotpoints, betaestList[[i]]$fd) - 2 * stdList[[i]], 						   			    eval.fd(plotpoints, betaestList[[i]]$fd) + 2 * stdList[[i]])
	   	}
	   	
	    plot.curv(base.x1, scale = scale.coef, xlab = "time", 
	     					main = "x1", ylim = plotrange[[1]])
	    lines(betaestList[[1]]$fd, col = "red")
	    lines(plotpoints, eval.fd(plotpoints,
	    							betaestList[[1]]$fd) + 2 * stdList[[1]], 
	    				type = "l", col = "grey")
	    lines(plotpoints, eval.fd(plotpoints,
	    							betaestList[[1]]$fd) - 2 * stdList[[1]], 
	    				type = "l", col = "grey")
	
	    plot.curv(base.z1, scale = scale.coef, xlab = "time", 
	     					main = "z1", ylim = plotrange[[2]])
	    lines(betaestList[[2]]$fd, col = "red")
	    lines(plotpoints, eval.fd(plotpoints,
	    							betaestList[[2]]$fd) + 2 * stdList[[2]], 
	    				type = "l", col = "grey")
	    lines(plotpoints, eval.fd(plotpoints,
	    							betaestList[[2]]$fd) - 2 * stdList[[2]], 
	    				type = "l", col = "grey")
	    
	    plot.curv(alpha, scale = scale.coef, xlab = "time",
	     					main = "self", ylim = plotrange[[3]])
	    lines(betaestList[[3]]$fd, col = "red")
	    lines(plotpoints, eval.fd(plotpoints,
	    							betaestList[[3]]$fd) + 2 * stdList[[3]], 
	    				type = "l", col = "grey")
	    lines(plotpoints, eval.fd(plotpoints,
	    							betaestList[[3]]$fd) - 2 * stdList[[3]], 
	    				type = "l", col = "grey")
	    dev.off()
	    
	####################
	MAPE.beta <- rep(0, 3)
	MAPE.beta[1] <- mean(abs((eval.fd(plotpoints, betaestList[[1]]$fd)-base.x1(plotpoints)/scale.coef)/(base.x1(plotpoints)/scale.coef)), na.rm = TRUE)
	MAPE.beta[2] <- mean(abs((eval.fd(plotpoints, betaestList[[2]]$fd)-base.z1(plotpoints)/scale.coef)/(base.z1(plotpoints)/scale.coef)), na.rm = TRUE)
	MAPE.beta[3] <- mean(abs((eval.fd(plotpoints, betaestList[[3]]$fd)-alpha(plotpoints)/scale.coef)/(alpha(plotpoints)/scale.coef)), na.rm = TRUE)
	return(MAPE.beta)
}
plot.curv <- function(func, scale = 1, main = NULL ,...){
	vec <- rep(0, length(plotpoints))
	for(i in 1:length(plotpoints)){
		vec[i] <- func(plotpoints[i])/scale
	}
	plot(plotpoints, vec, type = "l" , main = main, ...)
}
#########################################################
# Now estimate the function for the above
# see if really self-similar
plotpoints <- seq(0, 7, length = 500)
#baseline <- function(t){ 12 * t^3 - 1* t^2 + 5 * t + 10 * cos(t*2)}
arrival <- NULL
beta = 2
par(mfrow = c(3, 2))
alpha <- function(t){8*exp(.1*(t-3)^2) + .5}#}
plot.curv(alpha)
#base.x1 <- function(t){  5 * t^3 - 2 * t }
#base.x1 <- function(t){1}
base.x1 <- function(t){5 + 10 * t}#2*t^3 - 2.4*t^2 + 0.54*t + 0.2}#2*t^3 - 2.4*t^2 + 0.54*t + 0.2 }10*cos(t /3 + 5) + 10
# 2: 40 - 5*t
plot.curv(base.x1)
#base.x2 <- function(t){  -exp(2 * t) + 3 }
base.x2 <- function(t){0}# cos(t * 3 + 5) + 1}
plot.curv(base.x2)
#base.z1 <- function(t){ t^3 - 0.05 * t^2 + 0.2 * t + 0.5 * cos(t*2)}
base.z1 <- function(t){-t^2 + 6*t + 15}
# 1: 10*cos(t /3 + 5) + 10}
# 2: 15 + t - t
# 3: t^2 -6*t + 10
plot.curv(base.z1)
#base.z2 <- function(t){  - 3 * t^3 - cos(t*2)}
# base.z2 <- function(t){ 0 }
# plot.curv(base.z2)
######################################
version <- 6
######################################
bids <- NULL
scale.coef <- 25
for(i in 1:200){
    repeat{
    bids[[i]] <- sim.one(plotpoints, base.x1, base.x2, base.z1, base.z2, beta, alpha, scale = scale.coef)
    if(length(bids[[i]]$bid) >= 10){cat(".");break} [ , case]
    }
}
par(mfrow = c(2,1))
i <- sample(seq(1:200), 1)
plot(bids[[i]]$bid, seq(1, length(bids[[i]]$bid)))
lambda <- matrix(0, length(bids), length(plotpoints))
bidcount <- rep(0, length(bids))
for( i in 1:length(bids)){bidcount[i] <- length(bids[[i]]$bid)}
boxplot(bidcount)
mean(bidcount)
MABEcomb <- matrix(0, 3, 7)
MABEcomb.func <- matrix(0, 3, 7)
folder <- "ebay-compare/simulation"
##################################################################
case <- 1
bw.ave = 0.1
for(i in 1:length(bids)){
	fit  <- reflect.sm(bids[[i]]$bid, plotpoints, bandwidth = bw.ave, end = 7)
	#fit  <- reflect.akj(bids[[i]]$bid, plotpoints, h = bw.ave, alpha = 0.1)
	lambda[i, ] <- fit$yy * length(bids[[i]]$bid)
	}
MABEcomb[ , case] <- simFromDens(lambda, folder) 
MABEcomb.func[ , case] <- simFromDens.func(lambda, folder) 
##################################################################
case <- 2
bw.ave = 0.5
for(i in 1:length(bids)){
	fit  <- reflect.sm(bids[[i]]$bid, plotpoints, bandwidth = bw.ave, end = 7)
	#fit  <- reflect.akj(bids[[i]]$bid, plotpoints, h = bw.ave, alpha = 0.1)
	lambda[i, ] <- fit$yy * length(bids[[i]]$bid)
	}
MABEcomb[ , case] <- simFromDens(lambda, folder) 
MABEcomb.func[ , case] <- simFromDens.func(lambda, folder) 
##################################################################
case <- 3
bw.ave = 1
for(i in 1:length(bids)){
	fit  <- reflect.sm(bids[[i]]$bid, plotpoints, bandwidth = bw.ave, end = 7)
	#fit  <- reflect.akj(bids[[i]]$bid, plotpoints, h = bw.ave, alpha = 0.1)
	lambda[i, ] <- fit$yy * length(bids[[i]]$bid)
	}
MABEcomb[ , case] <- simFromDens(lambda, folder) 
MABEcomb.func[ , case] <- simFromDens.func(lambda, folder) 
##################################################################
case <- 4
bw.ave = 0.1
for(i in 1:length(bids)){
	fit  <- reflect.akj(bids[[i]]$bid, plotpoints, h = bw.ave, end = 7)
	#fit  <- reflect.akj(bids[[i]]$bid, plotpoints, h = bw.ave, alpha = 0.1)
	lambda[i, ] <- fit$yy * length(bids[[i]]$bid)
	}
MABEcomb[ , case] <- simFromDens(lambda, folder) 
MABEcomb.func[ , case] <- simFromDens.func(lambda, folder) 
##################################################################
case <- 5
bw.ave = 0.5
for(i in 1:length(bids)){
	fit  <- reflect.akj(bids[[i]]$bid, plotpoints, h = bw.ave, end = 7)
	#fit  <- reflect.akj(bids[[i]]$bid, plotpoints, h = bw.ave, alpha = 0.1)
	lambda[i, ] <- fit$yy * length(bids[[i]]$bid)
	}
MABEcomb[ , case] <- simFromDens(lambda, folder) 
MABEcomb.func[ , case] <- simFromDens.func(lambda, folder) 
##################################################################
case <- 6
bw.ave = 1
for(i in 1:length(bids)){
	fit  <- reflect.akj(bids[[i]]$bid, plotpoints, h = bw.ave, end = 7)
	#fit  <- reflect.akj(bids[[i]]$bid, plotpoints, h = bw.ave, alpha = 0.1)
	lambda[i, ] <- fit$yy * length(bids[[i]]$bid)
	}
MABEcomb[ , case] <- simFromDens(lambda, folder) 
MABEcomb.func[ , case] <- simFromDens.func(lambda, folder) 		
###########################################################
case <- 7
for(i in 1:length(bids)){
	fit  <- betafitting(x = bids[[i]]$bid, 
						y = seq(1:length(bids[[i]]$bid))/length(bids[[i]]$bid), 
						plotpoints = plotpoints, 
						START = 0, END = 7)
	lambda[i, ] <- fit * length(bids[[i]]$bid)
	}
MABEcomb[ , case] <- simFromDens(lambda, folder) 
MABEcomb.func[ , case] <- simFromDens.func(lambda, folder) 		
###########################################################
