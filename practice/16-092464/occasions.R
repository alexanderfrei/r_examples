setwd('practice/16-092464')

library("arules")
library("arulesViz")

library(plyr)
library(car)
library(dplyr)

library(psych)

df <- read.csv("segm.csv", sep=";")

colnames(df)[1] <- "daytime"
df$transport <- NULL

# categorical -> factors
df$daytime <- mapvalues(as.factor(df$daytime), from = c("1", "2", "3", "4"), to = c("night","morning","day", "evening")) 

multi <- grep("(a5_|a51_|a7_).*", colnames(df))
df[multi] <- data.frame(apply(df[multi], 2, as.logical)) # as logical!

df$a6from_1 <- as.factor(car::recode(df$a6from_1, "1='From_home'; 2='From_workplace'; 3:hi='From_other'"))
df$a6to_1 <- as.factor(car::recode(df$a6to_1, "1='To_home'; 2='To_workplace'; 3:hi='To_other'"))
df$a8_1 <- as.factor(car::recode(df$a8_1, "1='Me'; 2='Partner'; 3:hi='Pay_other'"))

# continuos NA to median
df$a91_1.2[is.na(df$a91_1.2)] <- median(df$a91_1.2, na.rm = T)
df$a91_1.2 <- as.factor(car::recode(df$a91_1.2, "0:14='0-14m'; 
                                    15:29='15-29m'; 
                                    30:hi='30m+'"))

# rules
dim(df)[1] * 0.005
trans <- as(df, "transactions")
rules <- apriori(trans, 
                 parameter = list(support=0.01, minlen=2, maxlen=4, confidence=0.7))
rules.sorted <- sort(rules, by="support")
inspect(subset( rules.sorted, subset = rhs %pin% "a5_" & (!lhs %pin% "a5_")))

# rdf <- as.data.frame(
# inspect(subset( rules.sorted, subset = rhs %pin% "a5_1.9" & (!lhs %pin% "a5_")))
# )



# itemFrequencyPlot(df.trans, topN=30,  cex.names=.8)
# as(trans[1:10,], "matrix")


# trans_bus <- subset(trans, items %in% "a5_1.1")
# itemFrequencyPlot(trans_bus, topN = 10, population = trans, lift=F, cex.names=1)
# discretize(1:100, method="frequency", 3)


