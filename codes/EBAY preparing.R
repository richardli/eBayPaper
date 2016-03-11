
## import data
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/DataPreparation.R', chdir = TRUE)
# EBAY[[1]]

## import reflection density fitting
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/reflection density.R', chdir = TRUE)
# reflect.sm

## introduce the grids on 0 to 7
plotpoints <- seq(0, 7, len = 200)

## first fit of density with sj selection of bandwidth
dens <- matrix(0, length(EBAY),length(plotpoints))
# bw <- rep(0, length(EBAY))
# for(i in 1: length(EBAY) ){
	# density <- reflect.sm(EBAY[[i]]$Time, plotpoints, bandwidth = "sj") 
	# dens[i, ] <- density$yy * length(EBAY[[i]]$Time)	
	# bw[i] <- density$bw
# }

#ave.bw <- mean(bw)
# redo density estimation using mean bandwidth
for(i in 1: length(EBAY) ){
	density <- reflect.sm(EBAY[[i]]$Time, plotpoints, bandwidth = ave.bw) 
	dens[i, ] <- density$yy * length(EBAY[[i]]$Time)	
}
# plot to see the fitted intensity
# par(mfrow = c(2,4),mar=c(3,4,1.5,1.5))
# pick <- c(2, 61, 91,  117, 119, 129, 146, 190)
# #pick <- sort(sample(seq(1:length(EBAY)), 8))
# for(i in 1:8){
	# picked <- pick[i]
	# plot(plotpoints, dens[picked, ], 
		 # type = "l",
		 # xlab = "time", 
		 # ylab = "bidding intensity",
		 # main = paste("auction", picked), 
		 # cex.main = 0.8)
	# points(EBAY[[picked]]$Time,
	       # rep(min(dens[picked, ]), length(EBAY[[picked]]$Time)), 
	       # pch = "|", 
	       # col = "red")
# }

## import the regression function
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/regre func.R', chdir = TRUE)

# EBAY.test <- NULL
# EBAY.test[[1]] <- EBAY[[1]]
# EBAY.test[[2]] <- EBAY[[2]]
#reg.ebay(EBAY.test,plotpoints,  -2)
#######################################################################
# search of r and store the rsq
len.sim <- 101
rsq <- matrix(0, len.sim, length(plotpoints))
for(i in 1:len.sim){
    r <- seq(-9.9, 0, len = len.sim)[i] 
	FuncReg <- reg.ebay(EBAY,plotpoints,  r)
	rsq[i, ] <- plot.rsq(FuncReg, plotpoints, noplot = FALSE)	
}
# save(rsq, file = "rsq.rda")

z <- rep(0, 101*200)
BETA <- rep(0, 101*200)
for(i in 1:101){
	z[(1+200*(i-1)):(200*i)] <- rsq[i, ]
	# y is the grid of beta
	BETA[(1+200*(i-1)):(200*i)] <- rep(seq(-9.9, 0, len = len.sim)[i]  , 200)
}
# # # x is the time grid
# T <- rep(seq(0, 7, len = 200), 101)
# library(scatterplot3d)
# scatterplot3d( BETA, T, z, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", main="scatterplot3d - 1", pch=20)
# install.packages(c("rgl", "evd"))
# library(rgl)
# library(evd)
# persp3d(seq(-9.9, 0, len = len.sim), seq(0, 7, len = 200), rsq, col = "green", zlim = c(0, 1))
# filled.contour( seq(0, 7, len = 200), seq(-9.9, 0, len = len.sim), t(rsq), color.palette=topo.colors)
# abline(h = -1)
rsq.mean <- apply(rsq, 1, mean)
# # plot(seq(-9.9, 0, len = len.sim), rsq.mean, type = "l", 
	 # xlab = expression(beta), 
	 # ylab = "average R-square")
r <- seq(-9.9, 0, len = len.sim)[which.max(rsq.mean)]

