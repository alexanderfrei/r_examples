# cmdscale

# example
library(graphics)
loc <- cmdscale(eurodist)
x <- loc[, 1]
y <- -loc[, 2]
plot(x, y, type = "n", xlab = "", ylab = "", asp = 1, axes = FALSE)
text(x, y, rownames(loc), cex = 0.6)

# plot with abbreviation

library(cluster) # for daisy dist
iris.dist <- daisy(iris[,1:4], metric="manhattan")
iris.c <- cmdscale(iris.dist)
plot(iris.c[,1:2], type="n", xlab="Dim. 1", ylab="Dim. 2")
text(iris.c[,1:2], labels=abbreviate(iris[,5],1, method="both.sides"))

# plot with abbreviation and density
library(KernSmooth)
est <- bkde2D(iris.c[,1:2], bandwidth=c(.7,1.5))
plot(iris.c[,1:2], type="n", xlab="Dim. 1", ylab="Dim. 2")
text(iris.c[,1:2], labels=abbreviate(iris[,5],1, method="both.sides"))
contour(est$x1, est$x2, est$fhat, add=TRUE,
        drawlabels=FALSE, lty=3)
