library(corrplot)
library(gplots)
library(psych)
library(heplots)

df <- RootStock
str(df)
col.names = c('Tree.Number', 'Trunk.Girth.4.Years', 'Ext.Growth.4.Years', 'Trunk.Girth.15.Years', 'Weight.Above.Ground.15.Years')
colnames(df) <- col.names
root <- df

# cov
S <- cov(root[,2:3])
# eigen
S.eigen <- eigen(S)

# m = 2: number of factors
plot(S.eigen$values, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', main = 'Scree Graph', type = 'b', xaxt = 'n')
axis(1, at = seq(1, 4, by = 1))

# C[p x m]: orthogonal matrix of eigenvectors
C <- as.matrix(S.eigen$vectors[,1:2])

# D[m x m]: diagonal matrix of eigenvalues
D <- matrix(0, dim(C)[2], dim(C)[2])
diag(D) <- S.eigen$values[1:2]

# Get unrotated loadings C * (D**1/2)
S.loadings <- C %*% sqrt(D)
S.loadings

# communalties (factor variance)
S.h2 <- rowSums(S.loadings^2)
S.h2

# unique variance (aka residuals)
S.u2 <- diag(S) - S.h2
S.u2

# proportion of loadings
prop.loadings <- colSums(S.loadings^2)
prop.var <- cbind(prop.loadings[1] / sum(S.eigen$values), 
                  prop.loadings[2] / sum(S.eigen$values))

# % of variance explained by factors
sum(prop.var) * 100

# proportion of variance explained by the loadings
prop.exp <- cbind(prop.loadings[1] / sum(prop.loadings), 
                  prop.loadings[2] / sum(prop.loadings))


############################################################################

# sum of squares by columns == eigenvalues for m factors
# colSums(S.loadings^2)
# S.eigen$values[1:2]

# PCA is an eigen vectors for 2 factors
# root.pca <- prcomp(root[,2:5])$rotation[,1:2] # Perform PCA on the rootstock data and take the resulting first two PCs
# S.eigen$vectors[,1:2]
# root.pca

# corrplot.mixed(corr=cor(df.fa), upper = "pie", lower = "number",
#                diag = "u",
#                col = colorpanel(10, "blue", "grey", "red"))

############################################################################
# psych

root.fa.covar <- principal(root[,2:5], nfactors = 2, rotate = 'none', covar = TRUE)
root.fa.covar


