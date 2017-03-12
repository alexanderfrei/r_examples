
library("arules")
library("arulesViz")

load("data/titanic.raw.rdata")

titanic.raw

rules <- apriori(titanic.raw,
  parameter = list(minlen=2, supp=0.005, conf=0.8),
  appearance = list(rhs=c("Survived=No", "Survived=Yes"), 
  default="lhs"),
  control = list(verbose=F))

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

### vis

plot(rules) # scatterplot

subrules2 <- head(sort(rules, by="lift"), 10) # get 10 rules with gihest lift 
plot(subrules2, method="graph") # graph

plot(rules, method="grouped") # matrix

# saveAsGraph(head(sort(rules, by="lift"),20), file="rules.graphml")

####################################################################

library("arules")
library("arulesViz")

# retail.raw <- readLines("http://goo.gl/FfjDAO")
retail.raw <- readLines("http://r-marketing.r-forge.r-project.org/data/retail.dat")
summary(retail.raw)        # off by 1 from Brijs et al, but Brijs says is OK

retail.list <- strsplit(retail.raw, " ")
names(retail.list) <- paste("Trans", 1:length(retail.list), sep="")
str(retail.list)

library(car)
some(retail.list)
rm(retail.raw)

# convert to transaction object from arules
retail.trans <- as(retail.list, "transactions")
summary(retail.trans)
rm(retail.list)

retail.rules <- apriori(retail.trans, parameter=list(supp=0.001, conf=0.4))
library(arulesViz)
plot(retail.rules)

# subset of rules: find top 50 sorted by lift
retail.hi <- head(sort(retail.rules, by="lift"), 50)
inspect(retail.hi)

# graph plot
plot(retail.hi, method="graph", control=list(type="items"))

################### itemMatrix S4 object
data(Adult)
Adult.largeIncome <- Adult [Adult %in% "income=large"]
itemFrequencyPlot(Adult.largeIncome)
