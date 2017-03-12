library(cluster)
library(mclust)

seg.raw <- read.csv("data/seg.csv")
seg.df  <- seg.raw[ , -7]

# a simple function to report means by group
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}
seg.summ(seg.df, seg.raw$Segment)

### NAIVE BAYES
set.seed(04625)
train.prop  <- 0.65
train.cases <- sample(nrow(seg.raw), nrow(seg.raw)*train.prop)
seg.df.train <- seg.raw[train.cases, ]
seg.df.test  <- seg.raw[-train.cases, ]

library(e1071)
(seg.nb <- naiveBayes(Segment ~ ., data=seg.df.train))
(seg.nb.class <- predict(seg.nb, seg.df.test))

# frequencies in predicted data
prop.table(table(seg.nb.class))

# plot it
clusplot(seg.df.test[, -7], seg.nb.class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, 
         main="Naive Bayes classification, holdout data")

# compare to known segments (which we can do with this test data)
mean(seg.df.test$Segment==seg.nb.class)

# adjusted for chance
library(mclust)
adjustedRandIndex(seg.nb.class, seg.df.test$Segment)

table(seg.nb.class, seg.df.test$Segment)
# summary data for proposed segments in the test data
seg.summ(seg.df.test, seg.nb.class)
# summary data for the known segments in the test data
seg.summ(seg.df.test, seg.df.test$Segment)

# predict raw probabilities
predict(seg.nb, seg.df.test, type="raw")
