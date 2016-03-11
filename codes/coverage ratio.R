
load("~/Dropbox/java_beginner/cheating group ebay.RData")
plotpoints <- seq(0, 7, len = 10000)
# do repeated sampling and plot coverage ratio
n=round(length(EBAY)*.6)
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

dens <- matrix(0, length(EBAY.training),length(plotpoints))
bw <- rep(0, length(EBAY.training))
for(i in 1: length(EBAY.training) ){
	density <- reflect.sm(EBAY.training[[i]]$Time, plotpoints, bandwidth = "sj") 
	dens[i, ] <- density$yy * length(EBAY.training[[i]]$Time)	
	bw[i] <- density$bw
}

ave.bw <- mean(bw)
# redo density estimation using mean bandwidth
for(i in 1: length(EBAY.training) ){
	density <- reflect.sm(EBAY.training[[i]]$Time, plotpoints, bandwidth = ave.bw) 
	dens[i, ] <- density$yy * length(EBAY.training[[i]]$Time)	
}
##################################################
r = -0.594
FuncReg <- reg.ebay(EBAY.training, plotpoints, r, reduced = TRUE)
rsq <- plot.rsq(FuncReg, plotpoints, noplot = FALSE)
par(mfrow = c(2, 5))


## now just extract the coefficients for forecasting
beta.coef <- matrix(0, dim(summary(FuncReg[[10]])$coefficients)[1], length(plotpoints))
for(i in 1:dim(summary(FuncReg[[10]])$coefficients)[1]){	
	beta.coef[i, ] <- plot.beta.coef(FuncReg, plotpoints, order = i, noplot = TRUE)[, 1]
}
print("regression done!")

mape2 <- mape(EBAY.testing, data.train = EBAY.training, beta.coef, r, plotpoints, Start = 6, toend = TRUE, sim = 5000)
