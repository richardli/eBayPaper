#x <- c(0, 0.01, 0.02, 0.05, 3, 3.1, 3.12, 3.5, 4, 4.5, 4.8, 5, 6.8, 6.801, 6.9, 6.902, 6.904, 6.95, 6.951, 6.9512, 6.952, 6.99)

reflect.sm <- function(x, plotpoints, bandwidth, end = 7, kernel = "gaussian"){
x.f1 <- -1 * x
x.f2 <- end * 2 - x
x.comb <- c(x.f1, x, x.f2)
dens.x <- density(x.comb, 
				  bw = bandwidth, 
		          kernel = kernel, 
		          n = length(plotpoints) * 3 - 2, 
		          from = -end, 
		          to = end * 2)
#plot(dens.x, xlim = c(0, 7))
#abline(v = 7)
#points(x, rep(0, length(x)))
#summary(dens.x)
xx <- dens.x$x[(length(plotpoints)) : (length(plotpoints) * 2 - 1)]
yy <- 3 * dens.x$y[(length(plotpoints) + 1) : (length(plotpoints) * 2 )]
return(list(yy = yy, bw = dens.x$bw))
}

reflect.akj <- function(x, plotpoints,end = 7,kernel = 0,...){
x.f1 <- -1 * x
x.f2 <- end * 2 - x
x.comb <- c(x.f1, x, x.f2)
plotpoints.comb <- c(-plotpoints, plotpoints, end * 2 - plotpoints)
dens.x <- akj(x.comb, plotpoints.comb, iker1 = kernel, ...)
yy <- 3 * dens.x$dens[(length(plotpoints) + 1) : (length(plotpoints) * 2 )]
return(list(yy = yy))
}
