sat.df <- read.csv("data/park.csv")

####
# check basic data suitability
library(gpairs)
gpairs(sat.df)
# fix distance
sat.df$logdist <- log(sat.df$distance)
  
# Fitting a model with a single predictor
lm(overall~rides, data=sat.df)
# -94.962 + 1.703*95
m1 <- lm(overall~rides, data=sat.df)
plot(overall~rides, data=sat.df,
     xlab="Satisfaction with Rides", ylab="Overall Satisfaction")
abline(m1, col='blue')
summary(m1)

####
# confidence intervals
confint(m1)

####
x <- rnorm(500)
y <- x^2 + rnorm(500)
toy.model <- lm(y~x)

#### residuals
plot(y~x)
abline(toy.model, col='blue')
plot(toy.model$fitted.values, toy.model$residuals)
# check model assumptions
par(mfrow=c(2,2))
plot(m1)

# Fitting a model with many predictors
m2 <- lm(overall ~ rides + games + wait + clean, data=sat.df)
summary(m2)
# plot(m2)
par(mfrow=c(1,1))
library(coefplot)
coefplot(m2, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Rating of Feature", 
         xlab="Association with Overall Satisfaction")

#### Comparing Models
plot(sat.df$overall, fitted(m1), col='red',
     xlim=c(0,100), ylim=c(0,100),
     xlab="Actual Overall Satisfaction", ylab="Fitted Overall Satisfaction")
points(sat.df$overall, fitted(m2), col='blue')
legend("topleft", legend=c("model 1", "model 2"), 
       col=c("red", "blue"), pch=1)
anova(m1, m2)

##### Prediction
coef(m2)%*%c(1,100, 100, 100, 100)
predict(m2, sat.df[1:10,])
fitted(m2)[1:10]

##### standardizing
scale(sat.df$rides)
sat.std <- sat.df[ , -3]  # sat but remove distance
sat.std[ , 3:8] <- scale(sat.std[ , 3:8])
head(sat.std)

##### Handling factors
m3 <- lm(overall ~ rides + games + wait + clean + 
                   weekend + logdist + num.child, 
         data = sat.std)
summary(m3)

sat.std$num.child.factor <- factor(sat.std$num.child)
m4 <- lm(overall ~ rides + games + wait + clean + 
                   weekend + logdist + num.child.factor, 
         data=sat.std)
summary(m4)

sat.std$has.child <- factor(sat.std$num.child > 0)
m5 <- lm(overall ~ rides + games + wait + clean + logdist + has.child, 
         data=sat.std)
summary(m5)

##### Interaction
m6 <- lm(overall ~ rides + games + wait + clean + 
                   weekend + logdist + has.child + 
                   rides:has.child + games:has.child + wait:has.child +
                   clean:has.child + 
                   rides:weekend + games:weekend + wait:weekend + clean:weekend, 
         data=sat.std)
summary(m6)


##### reduced model
m7 <- lm(overall ~ rides + games + wait + clean + logdist + has.child + 
                   wait:has.child,
         data=sat.std)
summary(m7)

anova(m5, m7)
plot(m7)
library(coefplot)
coefplot(m7, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Rating of Feature", 
         xlab="Association with Overall Satisfaction")

####
# Bayesian Linear Model
library(MCMCpack)
m7.bayes <- MCMCregress(overall ~ rides + games + wait + clean + logdist + 
                          has.child + wait:has.child,
                        data=sat.std)
summary(m7.bayes)
