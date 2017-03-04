library(randomForest)
set.seed(17)
iris.rf <- randomForest(iris.train[,5] ~ ., data=iris.train[,1:4])
iris.rfp <- predict(iris.rf, iris.unknown[,1:4])
table(iris.rfp, iris.unknown[,5])

iris.urf <- randomForest(iris[,-5])
MDSplot(iris.urf, iris[,5])
