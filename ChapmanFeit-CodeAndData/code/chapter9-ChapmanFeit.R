
##### 
##### Hierarchical linear model
#####

### Ratings-based conjoint analysis data


########
# alternative to load the data
# ==> BUT as always we recommend to create it instead, as below
conjoint.df <- read.csv("http://goo.gl/G8knGV")
# OR:
# conjoint.df <- read.csv(paste("http://r-marketing.r-forge.r-project.org/",
#                               "data/rintro-chapter9conjoint.csv", sep=""))
conjoint.df$speed  <- factor(conjoint.df$speed)
conjoint.df$height <- factor(conjoint.df$height)
summary(conjoint.df)
########



###
### Construct the data
###
set.seed(12814)
resp.id <- 1:200 # respondent ids
nques <- 16 # number of conjoint ratings per respondent
speed <- sample(as.factor(c("40", "50", "60", "70")), size=nques, replace=TRUE)
height <- sample(as.factor(c("200", "300", "400")), size=nques, replace=TRUE)
const <- sample(as.factor(c("Wood", "Steel")), size= nques, replace=TRUE)
theme <- sample(as.factor(c("Dragon", "Eagle")), size=nques, replace=TRUE)

profiles.df <- data.frame(speed, height, const, theme)
profiles.model <- model.matrix(~ speed + height + const + theme, 
                               data=profiles.df)

library(MASS)     # a standard library in R
weights <- mvrnorm(length(resp.id), 
                   mu=c(-3, 0.5, 1, 3, 2, 1, 0, -0.5),
                   Sigma=diag(c(0.2, 0.1, 0.1, 0.1, 0.2, 0.3, 1, 1)))

# create df to hold the data
# better would be to preallocate; but for small data set, building up is OK
conjoint.df <- NULL   # make sure there's no data yet

# create per-respondent ratings
for (i in seq_along(resp.id)) {
  # create one respondent's ratings of the 16 items, plus error
  utility <- profiles.model %*% weights[i, ] + rnorm(nques)  # preference
  rating <- as.numeric(cut(utility, 10))   # put on a 10-point scale
  conjoint.resp <- cbind(resp.id=rep(i, nques), rating, profiles.df)
  # and add that respondent to the total data set
  conjoint.df <- rbind(conjoint.df, conjoint.resp)
}  

### END: constructing the data



### Hierarchical model
###
summary(conjoint.df)
by(conjoint.df$rating, conjoint.df$height, mean)


# basic linear model
ride.lm <- lm(rating ~ speed + height + const + theme, data=conjoint.df)
summary(ride.lm)

# hierarchical model
library(lme4)

# 0 = HLM with intercept only
# model with random intercept by respondent = (1 | resp.id)
ride.hlm1 <- lmer(rating ~ speed + height + const + theme + (1 | resp.id),  
                  data=conjoint.df)

summary(ride.hlm1)

fixef(ride.hlm1)
head(ranef(ride.hlm1)$resp.id)
head(coef(ride.hlm1)$resp.id)

# model with random intercept & slope by respondent = (predictors | resp.id)
#
# consider 1M+ iterations; using 100K for somewhat faster time (~5 min)
# WARNING: slow, takes several minutes!
#
# note: set.seed not needed for lmer 
# (cf. https://stat.ethz.ch/pipermail/r-sig-mixed-models/2011q1/015263.html)
ride.hlm2 <- lmer(rating ~ speed + height + const + theme + 
                   (speed + height + const + theme | resp.id),
                 data=conjoint.df,
                 control=lmerControl(optCtrl=list(maxfun=100000)))  # default = 10000

# population estimate
fixef(ride.hlm2)

# individual estimates, 1 row per respondent
# just the random (individual variation) part
head(ranef(ride.hlm2)$resp.id)
# the total effect for each respondent (fixed + random)
head(coef(ride.hlm2)$resp.id)

# the coefficients are the fixed effects + individual (random) effects
# demonstrating this for an arbitrary respondent (id #196)
fixef(ride.hlm2) + ranef(ride.hlm2)$resp.id[196, ]
coef(ride.hlm2)$resp.id[196, ]


####
#### Hierarchical Bayes linear model, metric conjoint analysis
####

### LOAD conjoint.df DATA AS ABOVE

# standard lm with MCMC
library(MCMCpack)    # install if needed
set.seed(97439)
ride.mc1 <- MCMCregress(rating ~ speed + height + const + theme, 
                        data=conjoint.df)
summary(ride.mc1)


# hierarchical lm with MCMC
# WARNING: SLOW! Takes approx. 3 minutes on 2014 Macbook Air
#
set.seed(97439)
ride.mc2 <- MCMChregress(fixed = rating ~ speed + height + const + theme, 
                         random = ~ speed + height + const + theme, 
                         group="resp.id", data=conjoint.df, r=8, R=diag(8))

str(ride.mc2)

#Try it!: dimnames(b2$mcmc)

# overall estimates
summary(ride.mc2$mcmc[ ,1:8])

# estimates for one respondent (respondent 196)
summary(ride.mc2$mcmc[ , grepl(".196", colnames(ride.mc2$mcmc), fixed=TRUE)])

# overall estimates ... again
summary(ride.mc2$mcmc[ ,1:8])

# estimates for wood construction
ride.constWood <- summary(ride.mc2$mcmc[ , grepl("b.constWood", 
                                                colnames(ride.mc2$mcmc))] 
                          + ride.mc2$mcmc[ , "beta.constWood"])

hist(ride.constWood$statistics[,1], 
     main="Preference for Wood vs. Steel", 
     xlab="Rating points", ylab="Count of Respondents", xlim=c(-4,4))

# 60 mph 
ride.speed60 <- summary(ride.mc2$mcmc[,grepl("b.speed60", 
                                            colnames(ride.mc2$mcmc))] 
                        + ride.mc2$mcmc[,"beta.speed60"])

hist(ride.speed60$statistics[,1], 
     main="Preference for 60mph vs. 40mph", 
     xlab="Rating points", ylab="Count of Respondents", xlim=c(-4,4))



summary(ride.mc2$mcmc[,c("beta.constWood", "VCV.constWood.constWood", 
                   "beta.speed60","VCV.speed60.speed60")])


#### Reflections on Model Comparison

# now that we have models from 2 models, we might compare the fixed effects
fix.hlm <- fixef(ride.hlm2)
fix.hb  <- colMeans(ride.mc2$mcmc[ , 1:8])

plot(fix.hlm, fix.hb)
abline(0,1)

# or compare random effects (in this case, for one respondent)
# in general, would want to compare full coefficients (fixed + random)
#
# but in this case, the fixed are nearly identical between the two,
# so we'll omit those for convenience
#
# LME random effects for ID #196
ranef(ride.hlm2)$resp.id[196, ]

# MCMC random effects for ID #196
colMeans(ride.mc2$mcmc[ , grepl(".196", colnames(ride.mc2$mcmc), 
                                fixed=TRUE)])

# compare them graphically: 
# .. plot the distribution of the MCMC draws of the random effects for ID 196
# .. and then add distribution for the LME random effects for ID 196
# .. doing this for only the first 4 of the 7 non-intercept parameters


par(mfrow=c(2,2))       # make a 2x2 plot surface
plot.xlim <- c(-3, 3)   # define limits for the x-axis

for (i in 2:5) {        # first four parameters only, for convenience
  # plot the MCMC density for random effect i
  mcmc.col <- which(grepl(".196", colnames(ride.mc2$mcmc), fixed=TRUE))[i]
  plot(density(ride.mc2$mcmc[ , mcmc.col]), xlab="", 
       ylim=c(0, 1.4), xlim=plot.xlim,
       main=paste("HB & lmer density:",
                  colnames(ride.mc2$mcmc)[mcmc.col] ))
  # add the HLM density for random effect i
  hlm2.est <- ranef(ride.hlm2)$resp.id[196, i]               # mean estimate
  hlm2.sd <-  sqrt(attr(ranef(ride.hlm2, condVar=TRUE)$resp.id, 
                        "postVar")[ , , 196][i, i])
  seq.pts <- seq(from=plot.xlim[1], to=plot.xlim[2], length.out=1000) # range
  # .. find density at x-axis points using dnorm() and add that to the plot
  points(seq.pts, dnorm(seq.pts, mean=hlm2.est, sd=hlm2.sd), 
         col="red", pch=20, cex=0.4)                      
  legend("topright", legend=c("red = lmer", "black = HB"), 
         text.col=c("red", "black"))
}

