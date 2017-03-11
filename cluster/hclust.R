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

################################
eg.raw <- read.csv("data/seg.csv")
seg.df  <- seg.raw[ , -7]

# a simple function to report means by group
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}
seg.summ(seg.df, seg.raw$Segment)

library(cluster)
seg.dist <- daisy(seg.df)
seg.hc <- hclust(seg.dist, method="ward.D2")
plot(seg.hc)
# zoom in on just part of it
plot(cut(as.dendrogram(seg.hc), h=0.7)$lower[[1]])

# examine cophenetic correlation
cor(cophenetic(seg.hc), seg.dist)

# 4 clusters
plot(seg.hc)
rect.hclust(seg.hc, k=4, border="red")
seg.hc.segment <- cutree(seg.hc, k=4)
table(seg.hc.segment)
seg.summ(seg.df, seg.hc.segment)
