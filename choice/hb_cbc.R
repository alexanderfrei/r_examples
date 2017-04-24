cbc.df <- read.csv("data/cbc.csv", 
                   colClasses = c(seat = "factor", price = "factor"))


# ------- Hierarchical Bayes multinomial logit -----------
# requires the selected alternative to be stored
# as an integer number on the first row
choice <- rep(0, nrow(cbc.df))
choice[cbc.df[,"alt"]==1] <- cbc.df[cbc.df[,"choice"]==1,"alt"]

cbc.coded <- model.matrix(~ seat + eng + cargo + price, data = cbc.df)
cbc.coded <- cbc.coded[, -1] # remove the intercept

choicemodelr.data <- cbind(cbc.df[, 1:3], cbc.coded, choice)

# additional info about respondent (demos)
carpool <- cbc.df$carpool[cbc.df$ques==1 & cbc.df$alt==1]=="yes"
carpool <- as.numeric(carpool)
choicemodelr.demos <- as.matrix(carpool, nrow=length(carpool))

library(ChoiceModelR)
hb.post <- choicemodelr(data=choicemodelr.data, xcoding=rep(1, 7), 
                        demos=choicemodelr.demos, 
                        mcmc=list(R=20000, use=10000),
                        options=list(save=TRUE))

# Model parameters
hb.post$compdraw[[567]]$mu
# With demos
hb.post$deltadraw[567,]

# cov diagonals describe the variance
crossprod(hb.post$compdraw[[567]]$rooti)

# Individual-level betas
head(hb.post$betadraw[,,567])

str(hb.post$betadraw)
beta.post.mean <- apply(hb.post$betadraw, 1:2, mean)
dim(beta.post.mean)
beta.post.mean
# check uncertainty with confidence intervals
beta.post.q05 <- apply(hb.post$betadraw, 1:2, quantile, probs=c(0.05))
beta.post.q95 <- apply(hb.post$betadraw, 1:2, quantile, probs=c(0.95))
rbind(q05=beta.post.q05[1,], mean=beta.post.mean[1,], q95=beta.post.q95[1,])

# Prediction using individual-level draws
predict.hb.mnl <- function(betadraws, data, hb) {
  # Function for predicting shares from a hierarchical multinomial logit model 
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to 
  #       predict shares.  Same format at the data used to estimate model. 
  data.model <- model.matrix(~ seat + eng + cargo + price, data = data)
  data.model <- data.model[,-1] # remove the intercept
  nresp <- dim(betadraws)[1]
  ndraws <- dim(hb$betadraw)[3]
  shares <- array(dim=c(nresp, nrow(data), ndraws))
  for (d in 1:ndraws) {
    for (i in 1:nresp) {
      utility <- data.model%*%betadraws[i,,d]
      shares[i,,d] = exp(utility)/sum(exp(utility))
    }
  }
  shares.agg <- apply(shares, 2:3, mean)
  cbind(share=apply(shares.agg, 1, mean), 
        pct=t(apply(shares.agg, 1, quantile, probs=c(0.05, 0.95))), 
        data)
}

predict.hb.mnl(hb.post$betadraw, new.data, hb.post)

# simulating
new.data[2,1] <- 7
predict.hb.mnl(hb.post$betadraw, new.data, hb.post)
