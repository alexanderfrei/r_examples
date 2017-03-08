brand.ratings <- read.csv("data/brands_pca.csv")

# Rescaling the data
brand.sc <- brand.ratings
brand.sc[, 1:9] <- scale(brand.ratings[, 1:9])

# aggregate personality attributes by brand
brand.mean <- aggregate(. ~ brand, data=brand.sc, mean)
brand.mean[,1]
rownames(brand.mean) <- brand.mean[, 1] # use brand for the row names
brand.mean <- brand.mean[, -1]          # remove brand name column

#### Principal components

brand.mu.pc <- prcomp(brand.mean, scale=TRUE)
summary(brand.mu.pc)
biplot(brand.mu.pc, main="Brand positioning", cex=c(1.5, 1))
plot(brand.mu.pc, type="l")

#### Exploratory Factor Analysis

library(nFactors)
nScree(brand.sc[, 1:9]) # factors num
factanal(brand.sc[, 1:9], factors=3) # varimax rotation. minimum correlation

# oblique rotation, axes not perpendicular
library(GPArotation)
brand.fa.ob <- factanal(brand.sc[, 1:9], factors=3, rotation="oblimin")

# heatmap
library(gplots)
library(RColorBrewer)
heatmap.2(brand.fa.ob$loadings, 
          col=brewer.pal(8, "Greens"), trace="none", key=T, dend="none",
          Colv=FALSE, cexCol = 1.2,
          main="\n\n\n\n\nFactor loadings for brand adjectives")

# plot the structure - path diagram
library(semPlot)
# white = omit
semPaths(brand.fa.ob, what="est", residuals=FALSE,
         cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),
         edge.label.cex=0.75, nCharNodes=7)

# use regression scores
brand.fa.ob <- factanal(brand.sc[, 1:9], factors=3, rotation="oblimin", 
                        scores="Bartlett")
brand.scores <- data.frame(brand.fa.ob$scores)
brand.scores$brand <- brand.sc$brand
head(brand.scores)

brand.fa.mean <- aggregate(. ~ brand, data=brand.scores, mean)
rownames(brand.fa.mean) <- brand.fa.mean[, 1]
brand.fa.mean <- brand.fa.mean[, -1]
names(brand.fa.mean) <- c("Leader", "Value", "Latest")
brand.fa.mean

heatmap.2(as.matrix(brand.fa.mean), 
          col=brewer.pal(9, "Greens"), trace="none", key=T, dend="none",
          cexCol=1.2, main="\n\n\n\n\n\nMean factor score by brand")


#### Multidimensional scaling
# metric MDS
brand.dist <- dist(brand.mean)
brand.mds <- cmdscale(brand.dist)
plot(brand.mds, type="n")
text(brand.mds, rownames(brand.mds), cex=2)

# non-metric (rank) MDS alternative 
brand.rank <- data.frame(lapply(brand.mean, function(x) ordered(rank(x))))

library(cluster)
brand.dist.r <- daisy(brand.rank, metric="gower")
brand.mds.r <- isoMDS(brand.dist.r)
brand.mds.r
plot(brand.mds.r$points, type="n")
text(brand.mds.r$points, levels(brand.sc$brand), cex=1.5)

