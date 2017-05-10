library(corrplot)
library(gplots)
library(psych)
library(heplots)

df <- RootStock
str(df)
col.names = c('Tree.Number', 'Trunk.Girth.4.Years', 'Ext.Growth.4.Years', 'Trunk.Girth.15.Years', 'Weight.Above.Ground.15.Years')
colnames(df) <- col.names
root <- df

# corr matrix
R <- cor(root[,2:5])

# replace diag of R with squared multiple correlation; solve(R): inverse corr matrix
R.smc <- (1 - 1 / diag(solve(R)))
diag(R) <- R.smc
round(R, 2)

# eigen, m=2
r.eigen <- eigen(R)
r.eigen$values
r.eigen$values > 0

# C * D**(1/2)
r.lambda <- as.matrix(r.eigen$vectors[,1:2]) %*% diag(sqrt(r.eigen$values[1:2]))
r.lambda

# communalities, specific variances and complexity of the factor loadings
r.h2 <- rowSums(r.lambda^2)
r.u2 <- 1 - r.h2
com <- rowSums(r.lambda^2)^2 / rowSums(r.lambda^4)

cor.pa <- data.frame(cbind(round(r.lambda, 2), round(r.h2, 2), round(r.u2, 3), round(com, 1)))
colnames(cor.pa) <- c('PA1', 'PA2', 'h2', 'u2', 'com')
cor.pa


# psych

root.cor.fa <- fa(root[,2:5], nfactors = 2, rotate = 'none', fm = 'pa', max.iter = 1)
root.cor.fa

root.cor.fa.v <- fa(root[,2:5], nfactors = 2, rotate = 'varimax', fm = 'pa', max.iter = 1)
root.cor.fa.v
