# hclust dendro

iriss <- iris[seq(1,nrow(iris),5),]
iriss.dist <- daisy(iriss[,1:4])
iriss.h <- hclust(iriss.dist, method="ward")
plot(iriss.h, labels=abbreviate(iriss[,5],1,
                                method="both.sides"), main="")
plot(silhouette(cutree(iriss.h, 3), iriss.dist)) # silhouette

# bootstrap pvclust
library(pvclust)
irisst <- t(iriss[,1:4])
colnames(irisst) <- paste(abbreviate(iriss[,5], 3),
                          colnames(irisst))
iriss.pv <- pvclust(irisst, method.dist="manhattan",
                    method.hclust="ward.D", nboot=100)
plot(iriss.pv, col.pv=c(1,0,0), main="")
pvrect(iriss.pv, alpha=0.95)
seplot(iriss.pv)

# fuzzy 

iris.f <- fanny(iris[,1:4], 3)
plot(iris.f, which=1, main="")
data.frame(sp=iris[,5], iris.f$membership)
