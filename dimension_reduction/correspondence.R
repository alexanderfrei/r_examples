library("FactoMineR")
library("factoextra")

# load contingency table
data(housetasks)

# baloon plot
library("gplots")
dt <- as.table(as.matrix(housetasks))
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = F)

# mosaic plot
library("graphics")
mosaicplot(dt, shade = TRUE, las=2,
           main = "housetasks", cex.axis = 1.2)

# CA
res.ca <- CA(housetasks, graph = FALSE)
summary(res.ca, nb.dec = 2, ncp = 2, nbelements=Inf)

# scree plot 
fviz_screeplot(res.ca)

### symmetric biplot 
# Symmetric plot represents the row and column profiles simultaneously in a common space 
# The distance between any row and column items is not meaningful! 
# You can only make a general statements about the observed pattern.
# In order to interpret the distance between column and row points, 
#   the column profiles must be presented in row space or vice-versa.
# This type of map is called asymmetric biplot and is discussed at the end of this article.
fviz_ca_biplot(res.ca, ggtheme = theme_gray())

### rows contribution
# The row variables with the larger value, contribute the most to the definition of the dimensions.
row <- get_ca_row(res.ca)
row$contrib
library("corrplot")
corrplot(row$contrib, is.corr=FALSE)
# Contributions of rows on Dim.1
fviz_contrib(res.ca, choice = "row", axes = 1, top = 10, ggtheme = theme_gray())
# Aplha gradient
fviz_ca_row(res.ca, alpha.row="contrib", col.row = "black") + theme_minimal()
# Select the top 5 contributing rows
fviz_ca_row(res.ca, alpha.row="contrib", select.row=list(contrib=5), ggtheme = theme_gray())

### Cos2
# The cos2 measures the degree of association between rows/columns and a particular axis [0,1].
# Note that, all row points except Official are well represented by the first two dimensions. 
# This implies that the position of the point corresponding the item Official on the scatter plot 
#   should be interpreted with some caution.
fviz_cos2(res.ca, choice = "row", axes = 1:2)

### Column contrib
fviz_contrib(res.ca, choice = "col", axes = 1:2)
fviz_cos2(res.ca, choice = "col", axes = 1:2)

### assymetric biplot
fviz_ca_biplot(res.ca, map ="rowprincipal", arrow = c(F, F), ggtheme = theme_gray())

### contribution biplot
# Points that contribute very little to the solution, 
#   are close to the center of the biplot and are relatively unimportant to the interpretation.
# contribution of rows to the axis: Repairs + Holidays + Laundry
fviz_ca_biplot(res.ca, map ="colgreen", arrow = c(F, F), ggtheme = theme_gray())
# contribution of cols to the axis: Husband + Wife + Jointly
fviz_ca_biplot(res.ca, map ="rowgreen", arrow = c(F, F), ggtheme = theme_gray())


#####################################################################################
#####################################################################################
# supplementary columns

data(children)
# NA cells -> supplementary
res.ca <- CA (children, row.sup = 15:18, col.sup = 6:8, graph = FALSE)

# Active rows are in blue
# Supplementary rows are in darkblue
# Columns are in red
# Supplementary columns are in darkred
fviz_ca_biplot(res.ca) + theme_minimal()

# hide sup rows and cols
fviz_ca_biplot(res.ca, invisible = c("row.sup", "col.sup") ) + theme_minimal()

# only rows/columns
fviz_ca_row(res.ca) + theme_minimal()
fviz_ca_col(res.ca) + theme_minimal()

#####################################################################################
#####################################################################################
# dimension description

res.desc <- dimdesc(res.ca, axes = c(1,2))
res.desc$`Dim 1`
