setwd("C://R workspace//r_examples//practice//bayes")
library(bayesm)

cho <- read.csv("..//data//cho_cbc.csv", sep=';')

source("..//wide cbc format.R")
df <- wide.cbc.format(file="..//data//cho_cbc.csv", csv=F)
df.noref <- as.data.frame(df$df.noref)
head(df.noref, 61)

lgtdata <- NULL
ids <- unique(df.noref[1])[,1]
for (i in 1:length(ids)){
  resp <- df.noref[df.noref[1]==ids[i],]
  X <- resp[,-c(1,2,3,length(resp))]
  y <- resp$item[resp$cho==1]
  lgtdata[[i]] <- list(y=y,X=as.matrix(X))
}

lgtdata[[1]]

mcmc <- list(R=20000,keep=1)
out <- rhierMnlRwMixture(Data=list(p=7,lgtdata=lgtdata),Prior=list(ncomp=1),Mcmc=mcmc)

plot(out$loglike,type="l")
trace<-t(apply(out$betadraw,c(2,3),mean))
matplot(trace, type="l")
utilities <- apply(out$betadraw, 1:2, mean)
utilities <- cbind(utilities, 0)

write.csv2(utilities, "utilities.csv")

