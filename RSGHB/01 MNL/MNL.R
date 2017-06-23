setwd('./RSGHB/01 MNL')

library(RSGHB)
library(dplyr)
library(tidyr)
library(dummies)

source("../prepare_cho_RSGHB.R")

# ------------ data ------------

df_long = read.csv("cho.csv", sep=";")
df_long

data = prepare.cho.rsghb("cho.csv", none = T)

ID = data$att[1]

choice = data$cho[-c(1,2)] # delete id & task

att = data$att[-c(1,2)] # delete id & task
att = att[! colnames(att) %in% data$zero.levels] # delete future zero levels 



b = matrix(1:9, ncol=3)
row.names(b) = 1:3
b[rep(row.names(b), 5), ]

# ------------ model ------------

modelname <- "MNL" # used for output
gVarNamesNormal <- colnames(att)

# For each random variable, specify the distribution for its coefficient
# The options are:
# 1. normal
# 2. log-nomal
# 3. negative log-normal
# 4. normal with all values below zero massed at zero
# 5. Johnson SB with a specified min and max
# gDIST must have an entry for each value in gVarNamesNormal

gDIST <- rep(1, dim(att)[2]) # choose distribution

# STARTING VALUES
# The selection of the mean here is important when working with non-normal distributions
svN <- rep(0, dim(att)[2])
                    
# ITERATION SETTINGS
gNCREP    <- 10   # Number of iterations to use prior to convergence
gNEREP    <- 10 	# Number of iterations to keep for averaging after convergence has been reached
gNSKIP    <- 1			# Number of iterations to do in between retaining draws for averaging
gINFOSKIP <- 100    # How frequently to print info about the iteration process

# CONTROL LIST TO PASS TO doHB
control <- list(
     modelname = modelname,
     gVarNamesNormal = gVarNamesNormal,
     gDIST = gDIST,
     svN = svN,
     gNCREP = gNCREP,
     gNEREP = gNEREP,
     gNSKIP = gNSKIP,
     gINFOSKIP = gINFOSKIP,
     nodiagnostics = T,
     gSeed = 1987
)

# ------------ likelihood function ------------

likelihood <- function(fc, b) {
  
  b = as.matrix(b)
  print(dim(b))
  return(b)
}

# Estimate the model
model <- doHB(likelihood, ID, control)

b = as.matrix(runif(23,-1,1))
att_m = as.matrix(att)
dim(att_m)
dim(att_m %*% b)

# ------------ output ------------

# Plot model statistics
plot(model)
# plot(model, type = "A")

# Save in CSV format (Sawtooth-esque)
writeModel(model)

# Save model object
# save(model, file = paste0(model$modelname, ".RData"))


