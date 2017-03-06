cust.df <- read.csv("data/cust.csv")

# Unit sales, revenue, household income, price
# log(x)
# Distance 
# 1/x, 1/x^2, log(x)
# Market or preference share based on a utility value 
# e^x / (1+e^x)
# Right-tailed distributions (generally)
# √x or log(x)
# Left-tailed distributions (generally) 
# x^2

####
# Transformations

cor(cust.df$distance.to.store, cust.df$store.spend)
cor(1/cust.df$distance.to.store, cust.df$store.spend)
cor(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)

plot(cust.df$distance.to.store, cust.df$store.spend)
plot(1/cust.df$distance.to.store, cust.df$store.spend)
plot(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)

####
# Box-Cox transformation

library(car)

# estimation of transformation parameter
powerTransform(cust.df$distance.to.store)
lambda <- coef(powerTransform(1/cust.df$distance.to.store))

# 'Normalized' variance with Box-Cox
bcPower(cust.df$distance.to.store, lambda)

par(mfrow=c(1,2))
hist(cust.df$distance.to.store, 
     xlab="Distance to Nearest Store", ylab="Count of Customers", 
     main="Original Distribution")
hist(bcPower(cust.df$distance.to.store, lambda),
     xlab="Box-Cox Transform of Distance", ylab="Count of Customers", 
     main="Transformed Distribution")

l.dist  <- coef(powerTransform(cust.df$distance.to.store))
l.spend <- coef(powerTransform(cust.df$store.spend+1))

plot(cust.df$distance.to.store, cust.df$store.spend)

cor(bcPower(cust.df$distance.to.store, l.dist), 
    bcPower(cust.df$store.spend+1, l.spend))
