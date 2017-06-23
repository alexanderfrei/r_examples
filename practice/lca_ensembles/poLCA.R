seg.raw <- read.csv("data/seg.csv")
seg.df  <- seg.raw[ , -7]

describe(seg.df)

# a simple function to report means by group
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}
seg.summ(seg.df, seg.raw$Segment)

##############################
#### poLCA (category clust)

seg.df.cut <- seg.df
seg.df.cut$age    <- factor(ifelse(seg.df$age < median(seg.df$age), 1, 2))
seg.df.cut$income <- factor(ifelse(seg.df$income < median(seg.df$income),
                                   1, 2))
seg.df.cut$kids   <- factor(ifelse(seg.df$kids < median(seg.df$kids), 1, 2))
summary(seg.df.cut)

seg.df.cut

# create a model formula
seg.f <- with(seg.df.cut, 
              cbind(age, gender, income, kids, ownHome)~subscribe)


# fit the model
library(poLCA)
set.seed(02807)

seg.LCA3 <- poLCA(seg.f, data=seg.df.cut, nclass=3)

seg.LCA3$

seg.LCA4 <- poLCA(seg.f, data=seg.df.cut, nclass=4)

seg.LCA4$bic
seg.LCA3$bic


# examine the solutions
# 3 clusters
seg.summ(seg.df, seg.LCA3$predclass)
table(seg.LCA3$predclass)
clusplot(seg.df, seg.LCA3$predclass, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="LCA plot (K=3)")

# 4 clusters
seg.summ(seg.df, seg.LCA4$predclass)
table(seg.LCA4$predclass)
clusplot(seg.df, seg.LCA4$predclass, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="LCA plot (K=4)")

# compare 3-cluster and 4-cluster solutions
table(seg.LCA3$predclass, seg.LCA4$predclass)
library(mclust)
mapClass(seg.LCA3$predclass, seg.LCA4$predclass)
adjustedRandIndex(seg.LCA3$predclass, seg.LCA4$predclass)

# compare to known segments
table(seg.raw$Segment, seg.LCA4$predclass)
adjustedRandIndex(seg.raw$Segment, seg.LCA4$predclass)

