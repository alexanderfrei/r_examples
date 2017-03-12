library(cluster)
library(mclust)

seg.raw <- read.csv("data/seg.csv")
seg.df  <- seg.raw[ , -7]

# a simple function to report means by group
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}
seg.summ(seg.df, seg.raw$Segment)

#### random forest
library(randomForest)
set.seed(98040)
(seg.rf <- randomForest(Segment ~ ., data=seg.df.train, ntree=3000))

# predict the test data for random forest
seg.rf.class <- predict(seg.rf, seg.df.test)

# plot the solution
library(cluster)
clusplot(seg.df.test[, -7], seg.rf.class, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="Random Forest classification, holdout data")

# get the individual prediction distribution
seg.rf.class.all <- predict(seg.rf, seg.df.test, predict.all=TRUE)

# look at the distribution for the first 5 test data cases
apply(seg.rf.class.all$individual[1:5, ], 1, table) / 3000

# summaries for the proposed and actual segments
seg.summ(seg.df.test, seg.rf.class)
seg.summ(seg.df.test, seg.df.test$Segment)

# confusion matrix in test data
mean(seg.df.test$Segment==seg.rf.class)
table(seg.df.test$Segment, seg.rf.class)

library(mclust)
adjustedRandIndex(seg.df.test$Segment, seg.rf.class)

### random forest variable importance
set.seed(98040)
(seg.rf <- randomForest(Segment ~ ., data=seg.df.train, ntree=3000,
                        importance=TRUE))

importance(seg.rf)
varImpPlot(seg.rf, main="Variable importance by segment")

library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(seg.rf)[ , 1:4]), 
          col=brewer.pal(9, "Blues"), 
          dend="none", trace="none", key=T,
          margins=c(10, 10),
          main="Variable importance"
          )


#### predict subscription status
#### using random forest

set.seed(92118)
train.prop  <- 0.65
train.cases <- sample(nrow(seg.df), nrow(seg.df)*train.prop)
sub.df.train <- seg.raw[train.cases, ]
sub.df.test  <- seg.raw[-train.cases, ]

# see how differentiated the subscribers are, in the training data
clusplot(sub.df.train[, -6], sub.df.train$subscribe, color=TRUE, shade=TRUE, 
         labels=4, lines=0, main="Subscriber clusters, training data")

library(randomForest)
set.seed(11954)
(sub.rf <- randomForest(subscribe ~ ., data=sub.df.train, ntree=3000))

# try again with more trees, and balanced classes using sampsize
set.seed(11954)
(sub.rf <- randomForest(subscribe ~ ., data=sub.df.train, ntree=3000, 
                       sampsize=c(25, 25)) )

# predict the holdout data
sub.rf.sub <- predict(sub.rf, sub.df.test)
# confusion matrix
table(sub.rf.sub, sub.df.test$subscribe)

library(mclust)
adjustedRandIndex(sub.rf.sub, sub.df.test$subscribe)

library(psych)
cohen.kappa(cbind(sub.rf.sub, sub.df.test$subscribe))
