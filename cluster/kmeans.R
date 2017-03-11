library(cluster)

seg.raw <- read.csv("data/seg.csv")
seg.df  <- seg.raw[ , -7]

# a simple function to report means by group
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}
seg.summ(seg.df, seg.raw$Segment)

#### K-MEANS
# convert factor variables to numeric (kmeans requires)
seg.df.num <- seg.df
seg.df.num$gender    <- ifelse(seg.df$gender=="Male", 0, 1)
seg.df.num$ownHome   <- ifelse(seg.df$ownHome=="ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe=="subNo", 0, 1)
str(seg.df.num)

set.seed(96743)
seg.k <- kmeans(seg.df.num, centers=4)
seg.summ(seg.df, seg.k$cluster)
boxplot(seg.df.num$income ~ seg.k$cluster, ylab="Income", xlab="Cluster")
clusplot(seg.df, seg.k$cluster, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="K-means cluster plot")
