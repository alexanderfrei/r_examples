library("BayesLCA")
set.seed(123)

tau <- c(0.4, 0.6)
theta <- rbind(rep(c(0.8, 0.2), each = 2), rep(c(0.2, 0.8), each = 2))

X <- rlca(500, itemprob = theta, classprob = tau)  ## Generate data
x <- data.blca(X)  ## conveniently format data

data("Alzheimer")  ## load dataset
Alzheimer
alz <- data.blca(Alzheimer)

alz
