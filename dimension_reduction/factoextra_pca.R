# https://www.r-bloggers.com/factoextra-r-package-easy-multivariate-data-analyses-and-elegant-visualization/

library("factoextra")
data("decathlon2")
df <- decathlon2[1:23, 1:10]

# pca
library("FactoMineR")
res.pca <- PCA(df,  graph = FALSE)

# Visualize eigenvalues/variances
get_eig(res.pca)
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

# Extract the results for variables
var <- get_pca_var(res.pca)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

# Extract the results for individuals
ind <- get_pca_ind(res.pca)

# biplot
fviz_pca_biplot(res.pca, repel = TRUE)

# Compute PCA on the iris data set
# The variable Species (index = 5) is removed
# before PCA analysis
iris.pca <- PCA(iris[,-5], graph = FALSE)

# Visualize
# Use habillage to specify groups for coloring
fviz_pca_ind(iris.pca,
             label = "none", # hide individual labels
             habillage = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)




