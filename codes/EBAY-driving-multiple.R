directory <- "~/Dropbox/ebay-beta2"
version <- 1
seed.sample <- 20
Lambda.Type.All <- c("betafitting", "kernel", "var-kernel")
Fit.Type.All <- c( "pointwise", "functional")
############################################################
skipLargeReg <- TRUE
## whether to do FDA approach when forecasting
forecast.fda <- TRUE
############################################################
######################
Lambda.type <- Lambda.Type.All[1]
Fit.type <- Fit.Type.All[1]
h0 <- 1
Find.r <- FALSE
r.import <-  -0.3512812
######################
# # 
# ######################
# Lambda.type <- Lambda.Type.All[2]
# Fit.type <- Fit.Type.All[1]
# h0 <- 1
# Find.r <- FALSE
# r.import <-  -0.6025641 
# ######################


# # # ######################
# Lambda.type <- Lambda.Type.All[2]
# Fit.type <- Fit.Type.All[1]
# h0 <- 0.5
# Find.r <- FALSE
# r.import <-  -1.607692
# # ######################

# # ######################
# Lambda.type <- Lambda.Type.All[2]
# Fit.type <- Fit.Type.All[1]
# h0 <- 0.2
# Find.r <- FALSE
# r.import <-  -4.874359
# # ######################

# # ######################
# Lambda.type <- Lambda.Type.All[3]
# Fit.type <- Fit.Type.All[1]
# h0 <- 1
# Find.r <- FALSE
# r.import <-  -0.6025641 
# # ######################

# # ######################
# Lambda.type <- Lambda.Type.All[3]
# Fit.type <- Fit.Type.All[1]
# h0 <- 0.5
# Find.r <- FALSE
# r.import <-  -1.607692
# # ######################

# # ######################
# Lambda.type <- Lambda.Type.All[3]
# Fit.type <- Fit.Type.All[1]
# h0 <- 0.2
# Find.r <- FALSE
# r.import <-  -4.37195
# ######################

plotpoints <- seq(0, 7, len = 40000)

##############################################################
source('~/Dropbox/Ebay_2012/R_Code_Used_&_Data/EBAY-running-multiple.R', chdir = TRUE)
