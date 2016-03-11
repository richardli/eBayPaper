
## import data
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/DataPreparation.R', chdir = TRUE)
# EBAY[[1]]
###########################################################
## plot typical auction!
#randompick <- sort(sample(seq(1, length(EBAY)), 8))
randompick <- c(2, 8, 58, 71)
par(mfrow = c(2,2))
for(i in 1:8){
	plot(EBAY[[randompick[i]]]$Time, 
	     EBAY[[randompick[i]]]$Price,
	     cex = 0.8,
	     pch = 3, 
	     xlab = "Time", 
	     ylab = "Price", 
	     main = paste("auction", randompick[[i]]))
}
###########################################################
## import reflection density fitting
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/reflection density.R', chdir = TRUE)
# reflect.sm

## introduce the grids on 0 to 7
plotpoints <- seq(0, 7, len = 40000)

## first fit of density with sj selection of bandwidth
dens <- matrix(0, length(EBAY),length(plotpoints))
bw <- rep(0, length(EBAY))
for(i in 1: length(EBAY) ){
	density <- reflect.sm(EBAY[[i]]$Time, plotpoints, bandwidth = "sj") 
	dens[i, ] <- density$yy * length(EBAY[[i]]$Time)	
	bw[i] <- density$bw
}

ave.bw <- mean(bw)
# redo density estimation using mean bandwidth
for(i in 1: length(EBAY) ){
	density <- reflect.sm(EBAY[[i]]$Time, plotpoints, bandwidth = ave.bw) 
	dens[i, ] <- density$yy * length(EBAY[[i]]$Time)	
}
#plot to see the fitted intensity
#par(mfrow = c(2,4),mar=c(3,4,1.5,1.5))
#pick <- c(2, 61, 91,  117, 119, 129, 146, 190)
#pick <- sort(sample(seq(1:length(EBAY)), 8))
# for(i in 1:8){
	# picked <- pick[i]
	# plot(plotpoints, dens[picked, ], 
		 # type = "l",
		 # xlab = "time", 
		 # ylab = "bidding density",
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
## search of r and store the rsq
# len.sim <- 10
# rsq <- matrix(0, len.sim, length(plotpoints))
# for(i in 1:len.sim){
    # r <- seq(-10, 0, len = len.sim)[i] 
	# FuncReg <- reg.ebay(EBAY,plotpoints,  r)
	# rsq[i, ] <- plot.rsq(FuncReg, plotpoints, noplot = FALSE)	
# }
# save(rsq, file = "rsq.rda")
load('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/rsq100.R')
plot(seq(-9.9, 0, len = 101), apply(rsq,1,mean), type = "l",
		xlab = expression(beta), 
		ylab = "Mean adj-R square")
#######################################################################
## set r = -.594 here
r = -0.594
FuncReg <- reg.ebay(EBAY, plotpoints, r, reduced = FALSE)
rsq <- plot.rsq(FuncReg, plotpoints, noplot = FALSE)

pdf("fig-allsig.pdf", width=10, height=12 )
par(mfrow = c(5,2), mar=c(3, 4, 1, 1) + 0.1)

for(i in 1:dim(summary(FuncReg[[100]])$coefficients)[1]){	
	beta <- plot.beta(FuncReg, plotpoints, order = i, noplot = FALSE)
	abline(h = 0.05, col = "red")
}
dev.off()
library(ggplot2)
ppp <- NULL
for(i in 1:dim(summary(FuncReg[[100]])$coefficients)[1]){	
	ppp[[i]] <- plot.beta.coef(FuncReg, plotpoints, order = i, noplot = FALSE)
}
multiplot(ppp[[1]],ppp[[2]],ppp[[3]],ppp[[4]],ppp[[5]],ppp[[6]],ppp[[7]],ppp[[8]],ppp[[9]],ppp[[10]],cols = 5)

## now just extract the coefficients for forecasting
beta.coef <- matrix(0, dim(summary(FuncReg[[10]])$coefficients)[1], length(plotpoints))
for(i in 1:dim(summary(FuncReg[[10]])$coefficients)[1]){	
	beta.coef[i, ] <- plot.beta.coef(FuncReg, plotpoints, order = i, noplot = TRUE)[, 1]
}
##################################################################
#randompick <- sample(seq(1:length(EBAY)), 8)
randompick <- c(2, 41, 50, 185)
g = NULL
for(i in 1:4){
 fitted <- matrix(0, 3, length(FuncReg))
 wtp <- 11.57
 if(data[[randompick[i]]]$Value == 1)  wtp <- 134.65
 for(j in 1:length(FuncReg)){
 	fitted[ , j] <-predict(FuncReg[[j]],
 							data.frame(Self.Exciting = sum(exp(r*(plotpoints[j] - data[[randompick[i]]]$Time[data[[randompick[i]]]$Time <= plotpoints[j]]))), 
 							Price.Relative = wtp - StepCheck(data[[randompick[i]]]$Time, data[[randompick[i]]]$Price, plotpoints[j]), 
 						    Condition = data[[randompick[i]]]$Condition, 
 						    Early.Bidding = data[[randompick[i]]]$Early), interval = "prediction")
 			}	
 			fitted[fitted < 0] <- 0
 intensity.comb <- data.frame(fit = fitted[1,],
 							  fit.low = fitted[2, ], 
 							  fit.up = fitted[3, ], 
 							  truth = dens[randompick[i], ], 
 							  xx = plotpoints)
 g[[i]] <- ggplot(intensity.comb, aes(xx))+
 			geom_line(aes(y = fit), color = "blue")+
 			geom_line(aes(y = truth), color = "red")+
 			geom_ribbon(aes(ymin = fit.low, ymax = fit.up), alpha = 0.2)+
 			xlab("time")+
 			ylab("intensity")+
 			ggtitle(paste("auction", randompick[i]))
# #  plot(plotpoints, fitted[1, ], type = "l", ylim = range(fitted), main = paste("auction ", randompick[i]))
 # lines(plotpoints, fitted[2, ], type = "l", lty = 2)
 # lines(plotpoints, fitted[3, ], type = "l", lty = 2)
 # lines(plotpoints, dens[randompick[i], ], type = "l", col = "red")
 
}
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/regre func.R')
multiplot(g[[1]], g[[2]], g[[3]], g[[4]], cols = 2)
##########################################################
# Forecast!!
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/New MAPE.R', chdir = TRUE)

source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/PriceProcess.R', chdir = TRUE)
# Inc <- findprice(EBAY)
    # all1 <- Inc$IncALL1
    # all2 <- Inc$IncALL2
    # Inc.class <- findprice.cl(EBAY)
    # cl1 <- Inc.class$Inc1.class
    # cl2 <- Inc.class$Inc2.class
# SimSelf(EBAY[[1]], start = 6, end = 7, beta.coef = beta.coef, r = r, plotpoints = plotpoints)

mape(EBAY, beta.coef, r, plotpoints, Start = 6, toend = TRUE, sim = 200)
#######################################################
#plot single auction forecasting
library(ggplot2)
library(MASS)
current <- 6.5
# in 40000, k = 56 is good
k <- sample(seq(1:length(EBAY.testing)), 1)
testing.new <- EBAY.testing[[k]]
# pdf("sim56.pdf", width=0.25*6+6, height=6)
# plot(testing.new$Time, testing.new$Price, 
	 # xlim = c(6, 7), 
	 # ylim = c(testing.new$Price[1], max(testing.new$Price) * 2),
	 # xlab = "Time horizon",
	 # ylab = "Price", 
	 # pch = 3)
# abline(v = current)
simall <- NULL
simall.plus <- NULL
ptm <- proc.time()
for(j in 1:50){
	sim <- SimSelf(start     = current, 
	  			   end       = 7,
	  		       data      = testing.new, 
			       beta.coef = beta.coef,	  					               				   r         = r,     
	  			  plotpoints = plotpoints, 
	  		   	  toend      = TRUE, 
	 			  case       = NULL )
	if(dim(sim)[1] >= 2){
    	simall <- rbind(simall, sim[2:dim(sim)[1], ])  		
		simall.plus <- rbind(simall.plus, 
		                cbind(sim, rep(j, dim(sim)[1])))
		}
   # lines(sim[,1],sim[,2],cex=0.1,col="red")
}

data <- data.frame(a = c(simall.plus[,1], 
                         testing.new$Time[testing.new$Time > 6]), 
                   b = c(simall.plus[, 2], testing.new$Price[testing.new$Time > 6]), 
                   c = c(rep(0, dim(simall.plus)[1]), rep(1, length(testing.new$Time[testing.new$Time > 6]))), 
                   d = c(simall.plus[, 3], rep(100, length(testing.new$Time[testing.new$Time>6]))))
                   
pdf("sim56.pdf", width=0.25*6+6, height=6)
ggplot(data = data, aes(x = a, y = b,group = d))+
    xlim(6, 7)+
    xlab("Time horizon")+
    ylab("Price")+
    geom_point(size=5, colour="black",shape= 18,  data = subset(data, c == 1))+ 
	 geom_line(data = subset(data, c == 0),  colour = "red")

	
	 # ylim = c(testing.new$Price[1], max(testing.new$Price) * 2),
	 # xlab = "Time horizon",
	 # ylab = "Price", 
	 # pch = 3)

dev.off()

data <- data.frame(a = c(simall[,1], testing.new$Time[testing.new$Time > current]), b = c(simall[, 2], testing.new$Price[testing.new$Time > current]), c = c(rep(0, dim(simall)[1]), rep(1, length(testing.new$Time[testing.new$Time > current]))))
## layout settings for ggplot
t2 <- theme(
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
)


## generate the "z" coordinate (density) just for the correct midpoint in the color gradient
z <- kde2d(data$a, data$b)

g <- ggplot(data, aes(x=a, y=b)) +
  stat_density2d(aes(fill=..density..), geom="tile", contour=FALSE, data = subset(data, c==0)) +
  coord_cartesian(xlim = range(data$a), ylim=range(data$b)) +
  xlab("x method") +
  ylab("y method") +
  geom_point(size=5, colour="red",shape= 20,  data = subset(data, c == 1)) + t2
pdf("density.pdf", width=0.25*6+6, height=6)
g
dev.off()
