library(MASS)
iris.train <- iris[seq(1,nrow(iris),5),]
iris.unknown <- iris[-seq(1,nrow(iris),5),]
iris.lda <- lda(iris.train[,1:4], iris.train[,5])
iris.ldap <- predict(iris.lda, iris.unknown[,1:4])$class
table(iris.ldap, iris.unknown[,5])

# check misclass
misclass <- function(pred, obs) {
  tbl <- table(pred, obs)
  sum <- colSums(tbl)
  dia <- diag(tbl)
  msc <- (sum - dia)/sum * 100
  m.m <- mean(msc)
  cat("Classification table:", "\n")
  print(tbl)
  cat("Misclassification errors:", "\n")
  print(round(msc, 1))
}
misclass(iris.ldap, iris.unknown[,5])

ldam <- manova(as.matrix(iris.unknown[,1:4]) ~ iris.ldap)
summary(ldam, test="Wilks") # оценка качества разделения
iris.lda2 <- lda(iris[,1:4], iris[,5])
iris.ldap2 <- predict(iris.lda2, dimen=2)$x
plot(iris.ldap2, type="n", xlab="LD1", ylab="LD2")
text(iris.ldap2, labels=abbreviate(iris[,5], 1,
                                   method="both.sides"))

