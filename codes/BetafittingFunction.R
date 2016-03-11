library(optimx)
library(BB)
library(ucminf)
library(Rcgmin)
library(quadprog)
library(Rvmmin)
#library(minqa)
######################
betafitting <- function(x, y, plotpoints, START, END){
	NT <- length(plotpoints)
	smoothF <- NULL
	pdf <- NULL
	Fpar <- rep(0, 2)
	
	# last term being inf
	deltaT <- max(0.000001, x[length(x)]-x[length(x)-1] )
	x1 <- min(x[length(x)] + deltaT, END)
	xx <- c(x[1:length(x)], x1 )
	y1 <- 2 * y[length(y)] - y[length(y) - 1]
	yy <- c(y[1:length(y)], y1)
	x.sc <- (xx-START)/(END-START)
	#y.sc <- yy
	y.sc <- (yy - min(yy))/(max(yy) - min(yy))
	
	DistB <- function(a){
		sum((y.sc-pbeta(x.sc, a[1], a[2]))^2 + sum(x.sc-qbeta(y.sc, a[1], a[2]))^2)
	}
	DistA <- function(a){
		(mean(x.sc)-a[1]/(a[1]+a[2]))^2+(var(x.sc)-a[1]*a[2]/((a[1]+a[2])^2*(a[1]+a[2]+1)))^2 	
	}
	
	para0 <- c(optimx(c(1, 0), DistA, method = "BFGS")$p1, 
			   optimx(c(1, 0), DistA, method = "BFGS")$p2	)
	para0 <- abs(para0)
	paraopt <- optimx(para0, DistB, method = "BFGS") 
	para <- c(paraopt$p1, paraopt$p2)
	if(paraopt$convcode == 52){
		para <- c(optimx(para0, DistB, method = "Nelder-Mead")$p1, 
			      optimx(para0, DistB, method = "Nelder-Mead")$p2	)
	}
	Fpar <- para
	
	integrand <- function(u){u^(Fpar[1]-1) * (1-u)^(Fpar[2]-1)}
	for(j in 1:NT){
		#smoothF[j] <- integrate(integrand, 0, j/(NT+1))$value / beta(Fpar[1], Fpar[2])*(max(yy) - min(yy)) + min(yy)
		pdf[j] <- integrand(j/(NT+1))/beta(Fpar[1], Fpar[2])/(END-START)* (max(yy)-min(yy))		
	}	
	return(pdf)
}

