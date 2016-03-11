

#######
sim.one <- function(plotpoints, base.x1,base.x2, base.z1, base.z2, beta, alpha){	
	x1 <- rnorm(1, mean = 40, sd = 5)
	x2 <- rbinom(1, 1, prob = 0.6)
	z1 <- rep(0, length(plotpoints))
	z2 <- rep(0, length(plotpoints))
	z1[1] <- rnorm(1, mean = 100, sd = 10)
	#z2[1] <- rnorm(1, mean = 1, sd = 1)
	arrival <- NULL
	s <- -log(runif(1))
	sumv <- 0
	vt <- rep(0, length(plotpoints))
	for(i in 1:length(plotpoints)){
		vt[i] <- max(0, base.x1(plotpoints[i]) * x1 +
		        		 base.x2(plotpoints[i]) * x2 +
        				 base.z1(plotpoints[i]) * z1[i] )
				         # + base.z2(plotpoints[i]) * z2[i]) 
		if(length(arrival) > 0){
			vt[i] <- max(0, vt[i] + alpha(plotpoints[i]) * sum(exp(-beta * (plotpoints[i] - arrival))))
		}
	
		sumv <- sumv + vt[i] * max(plotpoints) / (length(plotpoints) - 1)
		if(sumv >= s) {
			arrival <- c(arrival, plotpoints[i])
			s <- s - log(runif(1))
			z1[i + 1] <- z1[i] - rnorm(1, mean = 4, sd = 1)
			#z2[i + 1] <- z2[i] + rnorm(1, mean = 1, sd = 1)
		}
		if(sumv < s){
			z1[i + 1] <- z1[i]
			#z2[i + 1] <- z2[i] 
		}
			
	}
return(list(bid = arrival,
			x1 = x1, 
			x2 = x2, 
			z1 = z1, 
			#z2 = z2, 
			vt = vt))
}

# par(mfrow = c(4, 1))
# #plot(plotpoints, vt, type = "l")
# plot(arrival, seq(1:length(arrival)), xlim =c(0,1),type = "b")

# plot(arrival[which(arrival > 0.8)], seq(1:length(which(arrival > 0.8))), xlim = c(0.8, 1),type = "b")

# plot(arrival[which(arrival > 0.9)], seq(1:length(which(arrival > 0.9))), xlim = c(0.9, 1),type = "b")
# plot(arrival[which(arrival > 0.99)], seq(1:length(which(arrival > 0.99))), xlim = c(0.99,1), type = "b")


#########################################################
# Now estimate the function for the above
#### plot function to check setup
plot.curv <- function(func, main = NULL){
	vec <- rep(0, length(plotpoints))
	for(i in 1:length(plotpoints)){
		vec[i] <- func(plotpoints[i])
	}
	plot(plotpoints, vec, type = "l" , main = main)
}
# see if really self-similar
plotpoints <- seq(0, 7, length = 5000)
#baseline <- function(t){ 12 * t^3 - 1* t^2 + 5 * t + 10 * cos(t*2)}
arrival <- NULL
beta = 1
par(mfrow = c(3, 2))
alpha <- function(t){.1*exp(.1*(t-3)^2) + .5}#}
plot.curv(alpha)
#base.x1 <- function(t){  5 * t^3 - 2 * t }
base.x1 <- function(t){1}
#base.x1 <- function(t){2*t^3 - 2.4*t^2 + 0.54*t + 0.2}#2*t^3 - 2.4*t^2 + 0.54*t + 0.2 }
plot.curv(base.x1)
#base.x2 <- function(t){  -exp(2 * t) + 3 }
base.x2 <- function(t){0}# cos(t * 3 + 5) + 1}
plot.curv(base.x2)
#base.z1 <- function(t){ t^3 - 0.05 * t^2 + 0.2 * t + 0.5 * cos(t*2)}
base.z1 <- function(t){ 0 }#* t^2 -0.04*t}
plot.curv(base.z1)
#base.z2 <- function(t){  - 3 * t^3 - cos(t*2)}
# base.z2 <- function(t){ 0 }
# plot.curv(base.z2)

bids <- NULL
for(i in 1:200){
    repeat{
    bids[[i]] <- sim.one(plotpoints, base.x1, base.x2, base.z1, base.z2, beta, alpha)
    if(length(bids[[i]]$bid) >= 10){break} 
    }
}
i <- sample(seq(1:200), 1)
plot(bids[[i]]$bid, seq(1, length(bids[[i]]$bid)))
lambda <- matrix(0, length(bids), length(plotpoints))
bw <- rep(0, length(bids))  
for(i in 1:length(bids)){
	fit  <- reflect.sm(bids[[i]]$bid, plotpoints, bandwidth = "nrd0", end = 7, kernel = "gaussian")
	lambda[i, ] <- fit$yy * length(bids[[i]]$bid)
	bw[i] <- fit$bw
	}
bw.ave <- mean(bw)/10
for(i in 1:length(bids)){
	fit  <- reflect.sm(bids[[i]]$bid, plotpoints, bandwidth = bw.ave, end = 7)
	lambda[i, ] <- fit$yy * length(bids[[i]]$bid)
	}

err <- rep(0, length(plotpoints))
for(i in 1:length(plotpoints)){
	err.list <- rep(0, length(bids))
	for(j in 1:length(bids)){
     	err.list[j] <- bids[[j]]$vt[i] - lambda[j, i]	
	}
	err[i] <- mean(err.list)
}
#par(mfrow = c(1,1))
plot(err)
par(mfrow = c(1,2))
i <- sample(seq(1:length(bids)), 1)
mean(bids[[i]]$vt/length(bids[[i]]$bid))
mean(lambda[i, ]/length(bids[[i]]$bid))
plot(plotpoints, bids[[i]]$vt,type = "l")
lines(plotpoints, lambda[i,], col = "red")
points(bids[[i]]$bid,rep(mean(bids[[i]]$vt), length(bids[[i]]$bid)),col="blue")
plot(plotpoints, lambda[i,], col = "red", type = "l")
points(bids[[i]]$bid,rep(mean(lambda[i,]), length(bids[[i]]$bid)),col="blue")

#coef <- matrix(0, length(plotpoints), 5)
coef <- matrix(0, length(plotpoints), 2)

rsq <- rep(0, length(plotpoints))
for(i in 10:length(plotpoints)){
		Y <- rep(0, length(bids))
		x1 <- rep(0, length(bids))
		x2 <- rep(0, length(bids))
		z1 <- rep(0, length(bids))
		#z2 <- rep(0, length(bids))
		self <- rep(0, length(bids))
		for(j in 1:length(bids)){
#		for(j in 1:50){
			Y[j] <- lambda[j, i]
#			Y[j] <- bids[[j]]$vt[i] #+ rnorm(1, mean = 0, sd = 2)
			x1[j] <- bids[[j]]$x1
			x2[j] <- bids[[j]]$x2
			z1[j] <- bids[[j]]$z1[i]
			#z2[j] <- bids[[j]]$z2[i]
			arr <- bids[[j]]$bid[bids[[j]]$bid <= plotpoints[i] ]
			self[j] <- sum(exp(-beta * (plotpoints[i] - arr)))
		}
#		fit <- lm(Y ~ x1 + x2 +z1 + z2 + self - 1 )
#		fit <- lm(Y ~ x1 + x2 +z1 + self - 1 )
		fit <- lm(Y ~ x1 + self - 1)
		coef[i, ] <- summary(fit)$coefficients[ , 1]
		rsq[i] <- summary(fit)$adj.r.squared
}

#par(mfrow = c(2, 3))
plot(rsq, type = "l")

plot.curv(base.x1, main = "x1")
lines(plotpoints, coef[, 1], type = "l", col = "red")
plot(coef[,1])
plot.curv(alpha, main = "alpha")
lines(plotpoints, coef[, 2], type = "l", col = "red")
plot(coef[,2])


plot.curv(base.x2, main = "x2")
lines(plotpoints, coef[, 2], type = "l", col = "red")

plot.curv(base.z1, main = "z1")
lines(plotpoints, coef[, 3], type = "l", col = "red")

#plot.curv(base.z2, main = "z2")
#lines(plotpoints, coef[, 4], type = "l", col = "red")

#plot.curv(alpha, main = "alpha")
lines(plotpoints, coef[, 4], type = "l", col = "red")


# # bids[[i]] <- sim.one(plotpoints, base.x1, base.x2, base.z1, base.z2, beta, alpha)
# F.smooth <- betafitting(bids[[i]]$bid, seq(1:length(bids[[i]]$bid))/length(bids[[i]]$bid), plotpoints, START = 0,END = 1)
# lambda[[i]] <- F.smooth[, 2] * length(bids[[i]]$bid)
# plot(bids[[i]]$vt -  lambda[[i]], type = "l")
# hist(bids[[i]]$vt - lambda[[i]])

# for(i in 1:50){
	
# bids[[i]] <- sim.one(plotpoints, base.x1, base.x2, base.z1, base.z2, beta, alpha)
# F.smooth <- betafitting(bids[[i]]$bid, seq(1:length(bids[[i]]$bid))/length(bids[[i]]$bid), plotpoints, START = 0,END = 1)
# lambda[[i]] <- F.smooth[, 2] * length(bids[[i]]$bid)
# }
# error <- matrix(0, 6, 50)
# for( i in 1:6){
	# count <- c(100, 1000, 5000, 100000, 15000, 200000)[i]
	# for(j in 1:50) error[i, j] <- bids[[i]]$vt[j] -lambda[[i]][j]
	# hist(error[i, ])
# }
