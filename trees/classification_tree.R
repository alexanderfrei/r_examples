library(rpart)
library(rpart.plot)
library(rattle)

fit <- rpart(Kyphosis ~ Age + Number + Start,
             method="class", data=kyphosis)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
prp(fit)
rpart.plot(fit, main="Classification Tree for Kyphosis")
fancyRpartPlot(fit)

# create attractive postscript plot of tree 
pdf(file = "graph/result/tree.pdf")
prp(fit)
rpart.plot(fit, main="Classification Tree for Kyphosis")
dev.off()

