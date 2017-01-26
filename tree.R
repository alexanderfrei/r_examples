ibrary(tree)
library(rpart)
library(rpart.plot)

iris.tree <- tree(iris[,5] ~ ., iris[,-5])
plot(iris.tree)
text(iris.tree)

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

