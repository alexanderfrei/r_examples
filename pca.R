# cmdscale
library(graphics)
loc <- cmdscale(eurodist)
x <- loc[, 1]
y <- -loc[, 2]
plot(x, y, type = "n", xlab = "", ylab = "", asp = 1, axes = FALSE)
text(x, y, rownames(loc), cex = 0.6)


iris.c <- cmdscale(iris.dist)

# pca

iris.pca <- princomp(scale(iris[,1:4])) # count cumulative variance
vars <- iris.pca$sdev^2 
props <- vars / sum(vars)
cumsum(props)

library('ade4') # pca in ade4 package
iris.dudi <- dudi.pca(iris[,1:4], scannf=FALSE)
s.class(iris.dudi$li, iris[,5])
iris.between <- bca(iris.dudi, iris[,5], scannf=FALSE)
randtest(iris.between) # test 