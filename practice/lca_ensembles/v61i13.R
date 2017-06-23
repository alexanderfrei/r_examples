########### Section 2.1 - Load library, data

library("BayesLCA")
set.seed(123)

tau <- c(0.4, 0.6)
theta <- rbind(rep(c(0.8, 0.2), each = 2), rep(c(0.2, 0.8), each = 2))

X <- rlca(500, itemprob = theta, classprob = tau)  ## Generate data
x <- data.blca(X)  ## conveniently format data

data("Alzheimer")  ## load dataset
alz <- data.blca(Alzheimer)



############### Section 3 - EM algorithm

## Section 3.1. - Synthetic Data

fit1 <- blca(X, 2, method = "em")
fit1
fit2 <- blca.em(x, 2, alpha = theta * 5, beta = (1 - theta) * 5, delta = tau * 5)
fit2$classprob
fit2$itemprob
summary(fit2)

######### Fig. 1 - Figures/emplot1.pdf and Figures/emplot2.pdf 1a Figures/emplot1.pdf
plot(fit1, which = 1)
## 1b Figures/emplot2.pdf
plot(fit1, which = 2)

## Section 3.2 - Examples of local maxima being reached
sj3.em <- blca.em(alz, 3)
sj31.em <- blca.em(alz, 3, restarts = 20)

######### Fig. 2 - Figures/emplot3b.pdf
plot(sort(sj31.em$lpstarts), sequence(table(round(sj31.em$lpstarts, 2))), main = "Converged Values", 
  xlab = "Log-Posterior", ylab = "Frequency")


## Standard error estimation
blca.em.sd(fit1, x)
blca.em.sd(sj31.em, alz)
sj3.boot <- blca.boot(alz, fit = sj31.em, B = 1000, relabel = TRUE)
sj3.boot

######### Fig. 3 - Figures/emplot4.pdf

par(mfrow = c(3, 2))
plot(sj3.boot, which = 3)
par(mfrow = c(1, 1))


################# Section 4 - Gibbs Sampling
fit2 <- blca.gibbs(x, 2)
fit2

######### Fig. 4 - Figures/gibbsplot1.pdf
plot(fit2, which = 3)


## Label Switching example
sj3.gibbs <- blca.gibbs(alz, 3, relabel = FALSE)

######### Fig. 5 - Figures/gibbsplot2.pdf
par(mfrow = c(4, 2))
plot(sj3.gibbs, which = 5)

sj30.gibbs <- blca.gibbs(alz, 3, relabel = TRUE)

## MCMC diagnostics

# library(coda)
raftery.diag(as.mcmc(sj30.gibbs))
sj31.gibbs <- blca.gibbs(alz, 3, burn.in = 150, thin = 1/10, iter = 50000)

######### Fig. 6 - Figures/labelswapfix.pdf
plot(sj3.gibbs, which = 5)

######### Fig. 7 - Figures/gibbsplot4.pdf
plot(sj3.gibbs, which = 6)

#################### Section 5 - Variational Bayes
fit3 <- blca.vb(x, 2)
sj3.vb <- blca.vb(alz, 3)

######### Fig. 8 - Figures/vbplot1.pdf
plot(fit3, which = 3)

## Standard error comparison with Gibbs sampler

sj3.vb$itemprob.sd
sj31.gibbs$itemprob.sd

## Model selection via overfitting example
sj10.vb <- blca.vb(alz, 10, delta = 1/10)
sj10.vb$classprob

######### Fig. 9 - Figures/vbplot2.pdf
plot(sj10.vb, which = 5)

##### Section 6 - Miscellaneous Functions

## Different starting values
set.seed(123)
test1 <- blca.em(alz, 3, start.vals = "across", restarts = 1)
set.seed(123)
test2 <- blca.em(alz, 3, start.vals = "single", restarts = 1)

Z1 <- Zscore(x$data, classprob = tau, itemprob = theta)
fit.true <- blca.em(x, 2, start.vals = Z1)

Z2 <- t(apply(sj31.gibbs$Z, 1, rmultinom, size = 1, n = 1))
sj3new.gibbs <- blca.gibbs(alz, 3, iter = 50000, start.vals = Z2, accept = 1/10, 
  burn.in = 0)

## Model Selection - EM algorithm
sj1 <- blca.em(alz, 1)
sj2.em <- blca.em(alz, 2)

c(sj1$BIC, sj2.em$BIC, sj31.em$BIC)
c(sj1$AIC, sj2.em$AIC, sj31.em$AIC)

## Model Selection - Gibbs sampler
sj2.gibbs <- blca.gibbs(alz, 2, accept = 1/10, iter = 50000, burn.in = 150)

c(2 * sj1$logpost, sj2.gibbs$DIC, sj31.gibbs$DIC) 
