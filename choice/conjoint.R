library(MASS)

# long format data
cbc.df <- read.csv("data/cbc.csv", 
                   colClasses = c(seat = "factor", price = "factor"))
#####

attrib <- list(seat = c("6", "7", "8"),
               cargo = c("2ft", "3ft"),
               eng = c("gas", "hyb", "elec"),
               price = c("30", "35", "40"))

# Choice data descriptives
library(dplyr)
cbc.df %>% filter(resp.id == 1)

xtabs(choice ~ price, data=cbc.df)
xtabs(choice ~ cargo, data=cbc.df)

###### Fitting a choice model with mlogit
library(mlogit)
cbc.mlogit <- mlogit.data(data=cbc.df, choice="choice", shape="long", 
                          varying=3:6, alt.levels=paste("pos",1:3), 
                          id.var="resp.id")
# o + mean without intercept 
# intercept in cbc is position of answer and should be unsignificant
m1 <- mlogit(choice ~ 0 + seat + cargo + eng + price, data = cbc.mlogit)
summary(m1)

# using numeric instead of factor if possible
m3 <- mlogit(choice ~ 0 + seat + cargo + eng 
                      + as.numeric(as.character(price)), 
             data = cbc.mlogit)
summary(m3)
lrtest(m1, m3)

###### Willingness to pay
# prices were recorded in 1,000s of dollars
coef(m3)["cargo3ft"]/(-coef(m3)["as.numeric(as.character(price))"]) * 1000
# 2700$ for cargo3ft: mean in general, that customer prefer
# cargo2ft to cargo3ft if price delta beetween more 2700$

###### Predicting shares
predict.mnl <- function(model, data) {
  # Function for predicting shares from a multinomial logit model 
  # recode factors to 0/1, -1 mean without intercept
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  # deterministic portion of utility
  utility <- data.model%*%model$coef
  # exp to convert logit share to share 
  share <- exp(utility)/sum(exp(utility))
  cbind(share, data)
}

# choose 6 random combinations
new.data <- expand.grid(attrib)[c(8, 1, 3, 41, 49, 26), ]
predict.mnl(m3, new.data)
predict.mnl(m1, new.data)

###### Share sensitivity plot
sensitivity.mnl <- function(model, attrib, base.data, competitor.data) {
  # Function for creating data for a share-sensitivity chart
  # model: mlogit object returned by mlogit() function
  # attrib: list of vectors with attribute levels to be used in sensitivity
  # base.data: data frame containing baseline design of target product
  # competitor.data: data frame contining design of competitive set
  data <- rbind(base.data, competitor.data)
  base.share <- predict.mnl(model, data)[1,1]
  share <- NULL
  for (a in seq_along(attrib)) {
    for (i in attrib[[a]]) {
      data[1,] <- base.data
      data[1,a] <- i
      share <- c(share, predict.mnl(model, data)[1,1])
    }
  }
  data.frame(level=unlist(attrib), share=share, increase=share-base.share)
}


base.data <- expand.grid(attrib)[c(8), ]
competitor.data <- expand.grid(attrib)[c(1, 3, 41, 49, 26), ]
tradeoff <- sensitivity.mnl(m1, attrib, base.data, competitor.data)

barplot(tradeoff$increase, horiz=FALSE, names.arg=tradeoff$level,
        ylab="Change in Share for Baseline Product")

# ------- Mixed logit model ------------------------------
# Model estimation
m1.rpar <- rep("n", length=length(m1$coef))
names(m1.rpar) <- names(m1$coef)
m1.rpar
m1.hier <- mlogit(choice ~ 0 + seat + eng + cargo + price, 
                  data = cbc.mlogit, 
                  panel=TRUE, rpar = m1.rpar, correlation = FALSE)
summary(m1.hier)

# strongly recommend include correlations in all random coefficients
m2.hier <- update(m1.hier, correlation = TRUE)
summary(m2.hier)
cov2cor(cov.mlogit(m2.hier))

# Simulating shares
predict.hier.mnl <- function(model, data, nresp=1000) {
  # Function for predicting shares from a hierarchical multinomial logit model 
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to 
  #       predict shares.  Same format at the data used to estimate model. 
  # Note that this code assumes all model parameters are random
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  coef.Sigma <- cov.mlogit(model)
  coef.mu <- model$coef[1:dim(coef.Sigma)[1]]
  draws <- mvrnorm(n=nresp, coef.mu, coef.Sigma)
  shares <- matrix(NA, nrow=nresp, ncol=nrow(data))
  for (i in 1:nresp) {
    utility <- data.model%*%draws[i,]
    share = exp(utility)/sum(exp(utility))
    shares[i,] <- share
  }
  cbind(colMeans(shares), data)
}

predict.hier.mnl(m2.hier, data=new.data)


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
