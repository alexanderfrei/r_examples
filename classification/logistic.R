cust.df <- read.csv("data/cust_lm.csv")

######
###### Logistic Regression
######

pass.df <- read.csv("data/pass.csv")
pass.df$Promo <- factor(pass.df$Promo, levels=c("NoBundle", "Bundle"))

pass.tab <- c(242, 639, 38, 359, 284, 27, 449, 223, 83, 278, 49, 485)
dim(pass.tab) <- c(3, 2, 2)
class(pass.tab) <- "table"
dimnames(pass.tab) <- list(Channel=c("Mail", "Park", "Email"),
                           Promo=c("Bundle", "NoBundle"),
                           Pass=c("YesPass", "NoPass") )

library(vcdExtra)   
pass.df <- expand.dft(pass.tab)
str(pass.df)
table(pass.df$Pass, pass.df$Promo)
# swap factor levels
pass.df$Promo <- factor(pass.df$Promo, levels=c("NoBundle", "Bundle"))
table(pass.df$Pass, pass.df$Promo)

###
### Logistic regression with glm()

# initial logistic regression model
pass.m1 <- glm(Pass ~ Promo, data=pass.df, family=binomial)
summary(pass.m1)

# odds ratio for sales
exp(coef(pass.m1))
# confidence intervals
exp(confint(pass.m1))
table(pass.df$Pass, pass.df$Channel)

# visualization
library(vcd)
doubledecker(table(pass.df))

# Model 2: add the effect of channel
pass.m2 <- glm(Pass ~ Promo + Channel, data=pass.df, family=binomial)
summary(pass.m2)

# updated coefs and odds ratios
exp(coef(pass.m2))
exp(confint(pass.m2))

# Model 3: add the interaction of promotion and channel
pass.m3 <- glm(Pass ~ Promo + Channel + Promo:Channel, 
               data=pass.df, family=binomial)
summary(pass.m3)

# updated coefs and odds ratios
exp(confint(pass.m3))


###### plot the coefs
library(coefplot)
coefplot(pass.m2, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         title="Coefficients for Season Pass by Factor", ylab="Factor")

#### plot the odds ratio confidence intervals
####
pass.ci <- data.frame(confint(pass.m2))     # coef confidence intervals
pass.ci$X50 <- coef(pass.m2)                # add the midpoint estimate

# plot odds
library(ggplot2)
pass.ci$Factor <- rownames(pass.ci)           # for ggplot2 to use in its model
pass.ci

# ggplot of odds ratios
# first: a plot by factor (x=) of the midpoint (y), high (ymax) and low (ymin)
p <- ggplot(pass.ci[-1, ], 
            aes(x=Factor, y=exp(X50), ymax=exp(X97.5..), ymin=exp(X2.5..)))

# ... displaying those elements as points & error bars
p <- p + geom_point(size=4) + geom_errorbar(width=0.25)

# ... adding a vertical line at an odds ratio of 1.0 (no change)
p <- p + geom_hline(yintercept=1, linetype="dotted", size=1.5, color="red")

# now plot it with titles
p + ylab("Likehood by Factor (odds ratio, main effect)") +
  ggtitle(paste("95% CI: Card sign up odds by factor")) + coord_flip()

##########################################################
