ibrary(cluster)
library(mclust)

seg.raw <- read.csv("data/seg.csv")
seg.df  <- seg.raw[ , -7]

# a simple function to report means by group
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}
seg.summ(seg.df, seg.raw$Segment)

#### MCLUST
# convert factor variables to numeric 
seg.df.num <- seg.df
seg.df.num$gender    <- ifelse(seg.df$gender=="Male", 0, 1)
seg.df.num$ownHome   <- ifelse(seg.df$ownHome=="ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe=="subNo", 0, 1)

seg.mc <- Mclust(seg.df.num)
summary(seg.mc)

seg.mc4 <- Mclust(seg.df.num, G=4)
summary(seg.mc4)

# compare 2 models
BIC(seg.mc, seg.mc4)

seg.summ(seg.df, seg.mc$class)
clusplot(seg.df, seg.mc$class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="Model-based cluster plot")

