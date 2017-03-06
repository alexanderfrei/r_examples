cust.df <- read.csv("data/cust.csv")

# Scatterplots
pairs(cust.df)

library(car)   
scatterplotMatrix(formula = ~ age + credit.score + email +
                    distance.to.store + online.visits + online.trans + 
                    online.spend + store.trans + store.spend, 
                  data=cust.df, diagonal="histogram")

# Correlation matrix
cor(cust.df[, c(2, 3, 5:12)])

library(corrplot)    # for correlation plot
library(gplots)      # for color interpolation

# corrplot.mixed(corr=cor(cust.df[ , c(2, 3, 5:12)], use="complete.obs"),diag = "n",
#                upper="ellipse", 
#                tl.pos="lt", 
#                col = colorpanel(50, "blue", "grey", "red"),
#                tl.col="black")
corrplot.mixed(corr=cor(cust.df[ , c(2, 3, 5:12)], use="complete.obs"),
               diag = "n",
               tl.pos="lt", 
               tl.col="black",
               col = colorpanel(5, "blue", "grey", "red"),)
# ?corrplot.mixed
