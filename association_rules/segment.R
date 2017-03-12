library("arules")
library("arulesViz")

######## try with segment data
seg.df <- read.csv("data/seg.csv")
summary(seg.df)

# recode continuous and counts to be all factors
seg.fac <- seg.df
summary(seg.fac)

seg.fac$age <- cut(seg.fac$age, 
                   breaks=c(0,25,35,55,65,100), 
                   labels=c("19-24", "25-34", "35-54", "55-64", "65+"), 
                   right=FALSE, ordered_result=TRUE)
summary(seg.fac$age)

seg.fac$income <- cut(seg.fac$income, 
                      breaks=c(-100000, 40000, 70000, 1000000),
                      labels=c("Low", "Medium", "High"),
                      right=FALSE, ordered_result=TRUE)

seg.fac$kids <- cut(seg.fac$kids, 
                    breaks=c(0, 1, 2, 3, 100),
                    labels=c("No kids", "1 kid", "2 kids", "3+ kids"),
                    right=FALSE, ordered_result=TRUE)

summary(seg.fac)

#### exploring segment associations

seg.trans <- as(seg.fac, "transactions")

# find some initial rules
seg.rules <- apriori(seg.trans, parameter=list(support=0.1, conf=0.4, 
                                               target="rules"))
summary(seg.rules)
plot(seg.rules)

# examine some of the higher-lift rules
seg.hi <- head(sort(seg.rules, by="lift"), 35)
inspect(seg.hi)
plot(seg.hi, method="graph", control=list(type="items"))  # note, plot orientation is random

# continue down the list by lift
seg.next <- sort(seg.rules, by="lift")[36:60]
plot(seg.next, method="graph", control=list(type="items"))  # not shown

# convert to df, write
write(seg.rules, "data/seg.rules.csv", sep=",")
rules.df <- read.csv("data/seg.rules.csv")

## partial rule matching
seg.sub <- head(sort(subset(seg.rules, 
                            subset=(rhs %pin% "Urban" | rhs %pin% "subscribe") & lift > 1), 
                     by="lift"), 100)
summary(seg.sub)

# grouped plot
plot(seg.sub, method="grouped")