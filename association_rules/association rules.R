library("arules")
library("arulesViz")

load("data/titanic.raw.rdata")

str(titanic.raw)

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
