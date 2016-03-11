
setwd("~/")
install.packages("fda")
## import data
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/DataPreparation.R', chdir = TRUE)
# EBAY[[1]]
#print(version)
## import reflection density fitting
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/reflection density.R', chdir = TRUE)
# reflect.sm

## introduce the grids on 0 to 7
plotpoints <- seq(0, 7, len = 40000)


source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/regre func.R', chdir = TRUE)
#############################################
n=round(length(EBAY)*.7)
a1=sample(c(1:length(EBAY)),n)
a2=rep(0,(length(EBAY)-n))
k=1
for(i in 1:length(EBAY)){
  skip=0
  for(j in 1: length(a1)){
    if(i==a1[j]){
      skip=1
      break
    }
  }
  if(skip==0){
    a2[k]=i
    k=k+1
  }
}
EBAY.training=NULL
for(i in 1:length(a1)){
  EBAY.training[[i]]=EBAY[[a1[i]]]
}
EBAY.testing=NULL
for(i in 1:length(a2)){
  EBAY.testing[[i]]=EBAY[[a2[i]]]
}

binary <- matrix(0, length(EBAY.training), length(plotpoints))
for(i in 1:length(EBAY.training)){
	for(j in 1:length(EBAY.training[[i]]$Time)){
		temp <- trunc(EBAY.training[[i]]$Time[j] / 7 * length(plotpoints))
		binary[i, temp+1] <- 1
	}
}
# ################################################
# ## first fit of density with sj selection of bandwidth
# dens <- matrix(0, length(EBAY.training),length(plotpoints))
# bw <- rep(0, length(EBAY.training))
# for(i in 1: length(EBAY.training) ){
	# density <- reflect.sm(EBAY.training[[i]]$Time, plotpoints, bandwidth = "sj") 
	# dens[i, ] <- density$yy * length(EBAY.training[[i]]$Time)	
	# bw[i] <- density$bw
# }

# ave.bw <- mean(bw)
# # redo density estimation using mean bandwidth
# for(i in 1: length(EBAY.training) ){
	# density <- reflect.sm(EBAY.training[[i]]$Time, plotpoints, bandwidth = ave.bw) 
	# dens[i, ] <- density$yy * length(EBAY.training[[i]]$Time)	
# }
# ##################################################
r = -0.594
FuncReg <- reg.ebay(EBAY.training, plotpoints, r, reduced = FALSE)
rsq <- plot.rsq(FuncReg, plotpoints, noplot = FALSE)
par(mfrow = c(2, 5))

for(i in 1:dim(summary(FuncReg[[10]])$coefficients)[1]){	
	beta <- plot.beta(FuncReg, plotpoints, order = i, noplot = FALSE)
	abline(h = 0.05, col = "red")
}


for(i in 1:dim(summary(FuncReg[[10]])$coefficients)[1]){	
	beta.coef <- plot.beta.coef(FuncReg, plotpoints, order = i, noplot = FALSE)
	abline(h = 0 , col = "blue")
}
## now just extract the coefficients for forecasting
beta.coef <- matrix(0, dim(summary(FuncReg[[10]])$coefficients)[1], length(plotpoints))
for(i in 1:dim(summary(FuncReg[[10]])$coefficients)[1]){	
	beta.coef[i, ] <- plot.beta.coef(FuncReg, plotpoints, order = i, noplot = TRUE)[, 1]
}
print("regression done!")
#save.image("running80000beforeMAPE.RData")
# # ####################################################
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/New MAPE.R', chdir = TRUE)

source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/PriceProcess.R', chdir = TRUE)
# Inc <- findprice(EBAY)
    # all1 <- Inc$IncALL1
    # all2 <- Inc$IncALL2
    # Inc.class <- findprice.cl(EBAY)
    # cl1 <- Inc.class$Inc1.class
    # cl2 <- Inc.class$Inc2.class
# SimSelf(EBAY[[1]], start = 6, end = 7, beta.coef = beta.coef, r = r, plotpoints = plotpoints)

mape1 <- mape(EBAY.testing, data.train = EBAY.training, beta.coef, r, plotpoints, Start = 6, toend = FALSE, sim = 500)
# result of fda in MAPE2nd
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/FDA_Spline.R', chdir = TRUE)
mape1.fda <- MAPE2nd
save.image(paste("treating set", ".RData"))
#load('~/running40000 4 .RData')
mape2 <- mape(EBAY.testing, data.train = EBAY.training, beta.coef, r, plotpoints, Start = 6, toend = TRUE, sim = 5000)
# result of fda in MAPE2nd
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/FDA_Spline_toend.R', chdir = TRUE)
mape2.fda <- MAPE2nd
save.image(paste("treating set", ".RData"))
######################################################################

#load("running40000.RData")
sep = 0.1
setwd("~/Dropbox")
pdf(paste("MAPE40000 step", version, ".pdf"), width = 6, height = 6)
par(mfrow=c(1,1),mar=rep(4,4))
horizon=seq(6,7-sep,by=sep)
plot(horizon+0.1, mape1[,1],type="b",ylim=c(0,.5),xlab="Time horizon",main="MAPE of Forecasting 0.1 day Ahead")
points(horizon+0.1,mape1.fda,type="b",col="red",lty=4,pch=2)
points(horizon+0.1,mape1[,2],type="l",col="grey",lty=2,pch=3)
points(horizon+0.1,mape1[,3],type="l",col="grey",lty=2,pch=3)
legend("topleft", c("SEBAM forecasting","FDA forecasting"),lty=c(1,4),pch=c(1,2),col=c("black","red"))
dev.off()

pdf(paste("MAPE-final", ".pdf"), width = 6, height = 6)
plot(mape2[[1]][,1],type="b",ylim=c(0,.5),xaxt="n",xlab="Time horizon",main="MAPE of Forecasting Final Price")
points(mape2.fda,type="b",col="red",lty=4,pch=2)
points(mape2[[1]][,2],type="l",col="grey",lty=2,pch=3)
points(mape2[[1]][,3],type="l",col="grey",lty=2,pch=3)
axis(side=1,at=c(1,2,3,4,5,6),label=c("-2 h","-1 h","-30 m","-15 m","-5 m","-1 m"))
legend("topright", c("SEBAM forecasting","FDA forecasting"),lty=c(1,4),pch=c(1,2),col=c("black","red"))
dev.off()

# # par(mfrow=c(1,2))
# plot(record.prob[1,],prob,main="Forecasting 2 hr ahead",ylab="C.I",xlab="simulated C.I",type="b",cex=0.5)
# plot(record.prob[2,],prob,main="Forecasting 1 hr ahead",ylab="C.I",xlab="simulated C.I",type="b",cex=0.5)
# plot(record.prob[4,],prob,main="Forecasting 0.5 hr ahead",ylab="C.I",xlab="simulated C.I")
# abline(a=0,b=1)
