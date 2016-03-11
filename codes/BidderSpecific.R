StepCheck <- function(time,vector,t){ 
  stage=0
  if(length(time)==1 && time[1]>=t){return(0)}
  if(length(time)==1 && time[1]<t){return(vector[1])}
  for(i in 1 : (length(time)-1)){
    if (t>=time[i] && t<time[i+1]){stage=vector[i]}
  }
  if (t>=time[length(time)]){stage=vector[length(time)]}
  return(stage)
}
StepCheck.linear <- function(time,vector,t){ 
  stage=0
  if(length(time)==1 && time[1]>=t){return(0)}
  if(length(time)==1 && time[1]<t){return(vector[1])}
  for(i in 1 : (length(time)-1)){
    if (t>=time[i] && t<time[i+1]){
    	# linear interpolate
    	stage=vector[i] + (vector[i+1] - vector[i])/(time[i+1] - time[i])*(t - time[i])}
  }
  if (t>=time[length(time)]){stage=vector[length(time)]}
  return(stage)
}

#Function: Stepcheck(time, vector, t), return vector step count at t.
StepCount <- function(time,vector,t){ 
  count <- 0
  if(length(time)==1 && time[1]>=t){return(0)}
  if(length(time)==1 && time[1]<t){return(1)}
  for(i in 1 : (length(time)-1)){
    if (t>=time[i] && t<time[i+1]){count=i}
  }
  if (t>=time[length(time)]){count=length(time)}
  return(count)
}
#######################################################

EBAY.b <- EBAY
id.list <- NULL
time.list <- NULL
bid.list <- NULL
price.list <- NULL
## delet those without bidder information
##
private.list <- NULL
for(i in 1:length(EBAY.b)){
	if(EBAY.b[[i]]$id[2] == 803) private.list <- c(private.list, i)
}
for(i in 1:length(private.list)){
	EBAY.b[[private.list[i] - i + 1]] <- NULL
}
##
for(i in 1:length(EBAY.b)){
	id.list[[i]] <- EBAY.b[[i]]$id[-1]
	time.list[[i]] <- EBAY.b[[i]]$Time[-1]
	bid.list[[i]] <- EBAY.b[[i]]$BidAmount[-1]
	price.list[[i]] <- EBAY.b[[i]]$Price[-1]
}

# Pull out all the first-entering time
time.ini <- NULL
for(i in 1:length(EBAY.b)){
	time.ini <- c(time.ini, time.list[[i]][1])
	for(j in 2:length(id.list[[i]])){
		if(!(id.list[[i]][j] %in% id.list[[i]][1:(j-1)])){
			time.ini <- c(time.ini, time.list[[i]][j])
		}
	}
}

# check if it is reasonable
length(time.ini)
# 1143 new bidder arrival
length(unlist(id.list))
# 2400 bids in total
length(unique(unlist(id.list)))
# 906 unique bidder in total
id.uniq <- NULL
for(i in 1:length(id.list)){
	id.uniq <- c(id.uniq, unique(id.list[[i]]))
}
length(id.uniq)
# 1143 uniq bidder in auctions(same bidder in two auctions regard as two uniq bidder)

## Plot entering time distribution
summary(time.ini)
hist(time.ini, freq= FALSE)
lines(density(time.ini))
###############################################
# plot bidder count break-down by day
num.day <- rep(0, 7)
for(i in 1:length(EBAY.b)){
    num.day[trunc(time.list[[i]][1])+ 1] <- num.day[trunc(time.list[[i]][1]) + 1 ] + 1 
	for(j in 2:length(id.list[[i]])){
		if(!(id.list[[i]][j] %in% id.list[[i]][1:(j-1)])){
			num.day[trunc(time.list[[i]][j]) + 1] <- num.day[trunc(time.list[[i]][j]) + 1] + 1 
		}
	}
}
plot(num.day, type = "l")
num.day
# number of new bidders arrival
# break-down by day: 149, 48, 44, 40, 61, 114, 687

###############################################
## plot bidding time of same bidder
num.bidder <- length(unique(time.ini))
bidding.time <- vector("list", num.bidder)
k <- 1
for(i in 1:length(EBAY.b)){
	pool.order <- NULL
	for(j in 1:length(id.list[[i]])){
		pool.order <- c(pool.order, which(id.list[[i]][1:j] == id.list[[i]][j])[1])
	}
	
	for(j in 1:length(unique(pool.order))){
		temp <- which(pool.order == unique(pool.order)[j])
		bidding.time[[k]] <- time.list[[i]][temp]
		k <- k + 1
	}
}

count.one <- 0
location.one <- NULL
for(i in 1:num.bidder){
	if(length(bidding.time[[i]]) == 1){
		count.one <- count.one + 1
		location.one <- c(location.one, i)	
	} 
}
count.one
# 620 bidders make one bid only

k <- 1
multi.time <- vector("list", (num.bidder-length(location.one)))
for(i in 1:num.bidder){
   if(!(i %in% location.one)){
   	multi.time[[k]] <- bidding.time[[i]]
   	k <- k + 1
   }	
}	
length(multi.time)
# 500 bidders make more than 1 bid

plot(multi.time[[1]], rep(1, length(multi.time[[1]])), xlim = c(0, 7), ylim = c(1, length(multi.time)), cex = 0.5, pch = 10)

for(i in 2: length(multi.time)){
	points(multi.time[[i]], rep(i, length(multi.time[[i]])), cex = 0.5, pch = 10)
	if(i %% 20 == 0) abline(h=i)
}

#############################################
# summary of biding numbers
# percent of bidders with >1 bids
length(multi.time)/length(bidding.time)
# 43.7% of biders make > 1 bids

# distribution of number of bids
count.bid <- rep(0, 33)
count.bid[1] <- count.one
for(i in 1:length(multi.time)){
	count.bid[length(multi.time[[i]])] <- count.bid[length(multi.time[[i]])] + 1
}
count.bid
# percent of responding bid to auto bidding
count.resp <- 0
for(i in 1:length(EBAY.b)){
	for(j in 2:length(id.list[[i]])){
		if(id.list[[i]][j] == id.list[[i]][j-1] &&
		   bid.list[[i]][j-1] < price.list[[i]][j-1]){
		   	count.resp <- count.resp + 1
		   }
	}
}
count.resp/length(unlist(bid.list))
# 26.7% of bids are response to auto bidding

# percent of responding bid to raised price by another bider
count.comp <- 0
for(i in 1:length(EBAY.b)){
	if(length(id.list[[i]]) < 3) next
	for(j in 3:length(id.list[[i]])){
		if(id.list[[i]][j] == id.list[[i]][j-2] &&
		   bid.list[[i]][j-2] == price.list[[i]][j-1]){
		   	count.comp <- count.comp + 1
		   }
		for(k in (j-2):1){
			if(id.list[[i]][j] == id.list[[i]][k] &&
			   time.list[[i]][j] - time.list[[i]][k] <= 1/24 &&
			   id.list[[i]][j] != id.list[[i]][j-1]){
			   	count.comp <- count.comp + 1
			   	next
			   }
		}
	}
}
count.comp/length(unlist(bid.list))
# 10.4% of bids are response to being outbidded within 1 hour
# 27.7% of bids are response to being outbidded within 1 day

###################################
# Seperate EBAY.b into ini and return
#####
ini <- NULL
return <- NULL
k1 <- 0
k2 <- 0
for(i in 1:length(EBAY.b)){
	# find the location of two sets
	bin <- apply(as.matrix(seq(2:length(EBAY.b[[i]]$id))+1), 1, 
		  function(x){EBAY.b[[i]]$id[x] %in% EBAY.b[[i]]$id[1:(x-1)]})
	ini.index <- which(bin == FALSE)
	return.index <- which(bin == TRUE)
	if(length(ini.index) < 2) print(i)
	if(length(ini.index) >= 2){
		k1 <- k1 + 1
		ini[[k1]] <- list(Time = EBAY.b[[i]]$Time[ini.index], 
						F = seq(1:length(ini.index))/length(ini.index),
						Price = EBAY.b[[i]]$Price[ini.index], 
						Pricediff = EBAY.b[[i]]$Pricediff[ini.index], 
						Condition = EBAY.b[[i]]$Condition, 
						Reserve = EBAY.b[[i]]$Reserve, 
						Seller = EBAY.b[[i]]$Seller, 
						Bidder = EBAY.b[[i]]$Bidder[ini.index], 
						id = EBAY.b[[i]]$id[ini.index], 
						Early = EBAY.b[[i]]$Early, 
						Jump = EBAY.b[[i]]$Jump, 
						Value = EBAY.b[[i]]$Value, 
						BidAmount = EBAY.b[[i]]$BidAmount[ini.index] 
						)
	
	}
	
	
	if(length(return.index) >= 2){
		k2 <- k2 + 1
		return[[k2]] <- list(Time = EBAY.b[[i]]$Time[return.index],
						F = seq(1:length(return.index))/length(return.index), 
						Price = EBAY.b[[i]]$Price[return.index], 
						Pricediff = EBAY.b[[i]]$Pricediff[return.index], 
						Condition = EBAY.b[[i]]$Condition, 
						Reserve = EBAY.b[[i]]$Reserve, 
						Seller = EBAY.b[[i]]$Seller, 
						Bidder = EBAY.b[[i]]$Bidder[return.index], 
						id = EBAY.b[[i]]$id[return.index], 
						Early = EBAY.b[[i]]$Early, 
						Jump = EBAY.b[[i]]$Jump, 
						Value = EBAY.b[[i]]$Value, 
						BidAmount = EBAY.b[[i]]$BidAmount[return.index] 
						)
	
	}
}
####### plot ini 
par(mfrow = c(3,4))
for(i in 1:length(ini)){
	plot(ini[[i]]$Time, ini[[i]]$Price)
}
for(i in 1:length(return)){
	plot(return[[i]]$Time, return[[i]]$Price)
}
####### b-spline smoothing?
SplineFit=function(x, y, plotpoints){
	knots <- c(1,2,3,4,5,6,6.25,6.5,6.75,6.8125,6.875,6.9375,7, x)
	numk<-length(knots)
	numplot <- length(plotpoints)
	norder <- 5
	nbasis <- numk + 2
	lambda <- 1
	wbasis <- create.bspline.basis(rangeval=c(0,7),nbasis=nbasis, norder=5)
	bids=rep(0,length(knots))
	for(j in 1:length(knots)) bids[j]=StepCheck.linear(x,y,knots[j])
	Wfd <- Data2fd(knots,bids,basisobj=wbasis)

	growfdPar <- fdPar(Wfd, 3, lambda)	
	mss <-smooth.basis(knots,bids,growfdPar,wtvec=rep(1,length(knots)))
	xfd <- mss$fd
	splineF <- NULL
	splinef <- NULL
	splineF <- eval.fd(plotpoints,xfd)
	splinef <- eval.fd(plotpoints,xfd,Lfd = 1)
	splineff <- eval.fd(plotpoints, xfd, Lfd = 2)
	return(cbind(splineF,splinef,splineff))
}
plotpoints <- seq(0, 7, len = 200)
##############################################################
F.ini.sm <- array(0, dim = c(length(ini), length(plotpoints)))
f.ini.sm <- array(0, dim = c(length(ini), length(plotpoints)))
 
for(i in 1:length(ini)){
	fit <- SplineFit(ini[[i]]$Time, ini[[i]]$F, plotpoints) 
	F.ini.sm[i, ] <- fit[, 1]
	f.ini.sm[i, ] <- fit[, 2]
}
par(mfrow = c(3, 4))
for(i in 1:length(ini)){
	plot(plotpoints, F.ini.sm[i, ], type = "l", col = "red")
	points(ini[[i]]$Time, ini[[i]]$F)	
}
##############################################################

F.return.sm <- array(0, dim = c(length(return), length(plotpoints)))
f.return.sm <- array(0, dim = c(length(return), length(plotpoints)))
 
for(i in 1:length(return)){
	fit <- SplineFit(c(0,return[[i]]$Time), c(0,return[[i]]$F), plotpoints) 
	F.return.sm[i, ] <- fit[, 1]
	f.return.sm[i, ] <- fit[, 2]
}
par(mfrow = c(3, 4))
for(i in 1:length(return)){
	plot(plotpoints, F.return.sm[i, ], type = "l", col = "red")
	points(return[[i]]$Time, return[[i]]$F)	
}
