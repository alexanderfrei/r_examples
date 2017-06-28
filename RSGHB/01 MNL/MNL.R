setwd('./RSGHB/01 MNL')

# ------------ data ------------

library(RSGHB)
library(splitstackshape)
library(data.table)

source("../prepare_cho_RSGHB.R")

# ------------ data ------------

data = prepare.cho.rsghb("cho.csv", none = T)

# make design ID * tasks

unique_ids = unique(data$cho[1])
n_task = max(data$cho[2])
ids = rep(unique_ids, n_task)

choicedata = data.frame(ID = ids[order(ids),], 
                        task = rep(1:n_task, length(unique_ids)))


cho = data$cho[-c(1,2,3)] # choices

# att
att = data$att[-c(1,2,3)] 
att = att[! colnames(att) %in% data$zero.levels] # delete future zero levels 
n_item = max(data$att$item) # items num
ncol = dim(att)[2]

nrow = length(unique(choicedata$ID)) * length(unique(choicedata$task))

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

gDIST <- rep(1, ncol) # choose distribution

# STARTING VALUES
# The selection of the mean here is important when working with non-normal distributions
svN <- rep(0, ncol)
                    
# ITERATION SETTINGS
gNCREP    <- 10000   # Number of iterations to use prior to convergence
gNEREP    <- 10000 	# Number of iterations to keep for averaging after convergence has been reached
gNSKIP    <- 1			# Number of iterations to do in between retaining draws for averaging
gINFOSKIP <- 100    # How frequently to print info about the iteration process

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

id_x_task = data$att$ID * data$att$task
v = data.table(task = id_x_task)

likelihood <- function(fc, b) {
  
  # prepare betas
  b = as.data.frame(b)
  b$freq = n_item
  betas = expandRows(b, "freq")
  betas['freq'] = NULL
  
  # exp(v)

  v$exp_v = exp(apply(betas * att, 1, sum))

  # exp(v) * cho

  prod_cho = v$exp_v * cho
  colnames(prod_cho) = "prod_cho"
  v = cbind(v, prod_cho)

  # aggregate p = exp(v) * cho / sum(exp(v))

  p = v[, list(p=sum(prod_cho) / sum(exp_v)), by = 'task']

  return(p$p)
  
}

# Estimate the model
model <- doHB(likelihood, choicedata, control)

# ------------ output ------------

# Save in CSV format (Sawtooth-esque)
writeModel(model)

# ------------ compare ------------

uti = read.csv("sawtooth_utilities.csv", sep = "\t")
uti = uti[3:26]
uti = as.data.frame(apply(uti, 2, function(x) as.numeric(gsub(",", ".", as.character(x)))))

df = read.csv("MNL_B.csv", sep = ",")
B = cbind(df[,2:23], 
          att1_23=0, # add zero level 
          none=df[,'none'])

B = B - apply(B, 1, mean)
mean(sapply(seq.int(dim(B)[1]), function(i) cor(t(B[i,]), t(uti[i,]))))


