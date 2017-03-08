cust.df <- read.csv("data/cust_lm.csv")

###### Collinearity
summary(cust.df)

library(corrplot)
library(dplyr)
df <- cust.df %>% select(-email)

int.na.median.fill <- function(x){ 
  if (is.integer(x)) {
    x[is.na(x)] <- median(x, na.rm=T)
  }
  return(x)
}

cust.df.filled <- as.data.frame(lapply(df, int.na.median.fill))
corrplot.mixed(cor(cust.df.filled),tl.pos = "lt",diag = "l")

### Automatic data transformation
autoTransform <- function(x) { 
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

cust.df.bc <- cust.df[complete.cases(cust.df), -1]
cust.df.bc <- subset(cust.df.bc, online.spend > 0)
numcols <- which(colnames(cust.df.bc) != "email")
cust.df.bc[ , numcols] <- lapply(cust.df.bc[ , numcols], autoTransform )

gpairs(cust.df.bc)    # output not shown in the book

#### lm to predict online spend, after transform
spend.m2 <- lm(online.spend ~ ., data=cust.df.bc)
spend.m3 <- lm(online.spend ~ online.trans, data=cust.df.bc)
anova(spend.m3, spend.m2)

library(car)
vif(spend.m2) # variance inflation factor, if > 5 mean collinearity
# vif estimates shared variance of variable with other variables

# solution 1: omit covariates
#
spend.m4 <- lm(online.spend ~ . -online.trans -store.trans, 
               data=cust.df.bc)
vif(spend.m4)
summary(spend.m4)

# solution 2: principal components
#
pc.online <- prcomp(cust.df.bc[ , c("online.visits", "online.trans")])
cust.df.bc$online <- pc.online$x[ , 1]

pc.store <- prcomp(cust.df.bc[ , c("store.trans", "store.spend")])
cust.df.bc$store <- pc.store$x[ , 1]

spend.m5 <- lm(online.spend ~ email + age + credit.score + 
                 distance.to.store + sat.service + sat.selection + 
                 online + store, 
               data=cust.df.bc)

summary(spend.m5)
vif(spend.m5)
