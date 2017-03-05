store.df <- read.csv("data/to_describe.csv")  

####################################
# descriptives

# freq table
table(store.df$p1price)
# crosstab
table(store.df$p1price, store.df$p1prom)

## Descriptive summaries
summary(store.df, digits=2)

library(psych)
describe(store.df)
write.file(describe(store.df), file="output.txt")

# write output to file
out <- capture.output(describe(store.df))
cat("", out, file="output/describe.txt", fill = T)

####################################

############
# simulating data 

k.stores <- 20    # 20 stores
k.weeks <- 104    # 2 years of data each
store.df <- data.frame(matrix(NA, ncol=10, nrow=k.stores*k.weeks))
names(store.df) <- c("storeNum", "Year", "Week", "p1sales", "p2sales", 
                     "p1price", "p2price", "p1prom", "p2prom", "country")
store.num <- 101:(100+k.stores)
(store.cty <- c(rep("US", 3), rep("DE", 5), rep("GB", 3), rep("BR", 2),
                rep("JP", 4), rep("AU", 1), rep("CN", 2)))

#rep: each - rep in row, times - mixing rep
store.df$storeNum <- rep(store.num, each=k.weeks)
store.df$country  <- rep(store.cty, each=k.weeks)
rm(store.num, store.cty)    # clean up
(store.df$Week <- rep(1:52, times=k.stores*2))
(store.df$Year  <- rep(rep(1:2, each=k.weeks/2), times=k.stores))

# make factors
store.df$storeNum <- factor(store.df$storeNum)
store.df$country  <- factor(store.df$country)
str(store.df)

# check df
head(store.df, 20)  
tail(store.df, 20)  
library(car)
some(store.df, 10)

# random seed
set.seed(98250)

# binom distribution
store.df$p1prom <- rbinom(n=nrow(store.df), size=1, p=0.1)  # 10% promoted
store.df$p2prom <- rbinom(n=nrow(store.df), size=1, p=0.15) # 15%

# create rows with fixed values by sample(), 
# replace = T: allow repeating - otherwise mistake if size > n
store.df$p1price <- sample(x=c(2.19, 2.29, 2.49, 2.79, 2.99), 
                           size=nrow(store.df), replace=TRUE)
store.df$p2price <- sample(x=c(2.29, 2.49, 2.59, 2.99, 3.19), 
                           size=nrow(store.df), replace=TRUE)

# poisson (counts) distribution
tmp.sales1 <- rpois(nrow(store.df), lambda=120)  
tmp.sales2 <- rpois(nrow(store.df), lambda=100)  

# scale sales according to the ratio of log(price)
tmp.sales1 <- tmp.sales1 * log(store.df$p2price) / log(store.df$p1price)
tmp.sales2 <- tmp.sales2 * log(store.df$p1price) / log(store.df$p2price)

# final sales get a 30% or 40% lift when promoted
store.df$p1sales <- floor(tmp.sales1 * (1 + store.df$p1prom*0.3))
store.df$p2sales <- floor(tmp.sales2 * (1 + store.df$p2prom*0.4))
head(store.df)

# to write it out to a CSV
# write.csv(store.df, file="data/to_describe.csv",row.names=FALSE)
#



### qq check for normality
qqnorm(store.df$p1sales)
qqline(store.df$p1sales)

### is more normal with log()
qqnorm(log(store.df$p1sales))
qqline(log(store.df$p1sales))


### ecdf cumulative distribution plot

plot(ecdf(store.df$p1sales),
     main="Cumulative distribution of P1 Weekly Sales",
     ylab="Cumulative Proportion",
     xlab=c("P1 weekly sales, all stores", "90% of weeks sold <= 171 units"),
     yaxt="n")
axis(side=2, at=seq(0, 1, by=0.1), las=1, 
     labels=paste(seq(0,100,by=10), "%", sep=""))
# add lines for 90%
abline(h=0.9, lty=3)
abline(v=quantile(store.df$p1sales, pr=0.9), lty=3)



### by() and aggregate()
by(store.df$p1sales, store.df$storeNum, mean)
by(store.df$p1sales, list(store.df$storeNum, store.df$Year), mean)

aggregate(store.df$p1sales, by=list(country=store.df$country), sum)


############
# world map 

p1sales.sum <- aggregate(store.df$p1sales, 
                         by=list(country=store.df$country), sum)

# install.packages(c("rworldmap", "RColorBrewer"))
library(rworldmap)
library(RColorBrewer)

# create map
p1sales.map <- joinCountryData2Map(p1sales.sum, joinCode = "ISO2", 
                                   nameJoinColumn = "country")

mapCountryData(p1sales.map, nameColumnToPlot="x", 
               mapTitle="Total P1 sales by Country",
               colourPalette=brewer.pal(7, "Blues"), 
               catMethod="fixedWidth", addLegend=F)
