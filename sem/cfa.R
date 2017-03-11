piesSimData <- read.csv("data/piesSimData.csv")
library(lavaan)
library(semTools)
library(semPlot)
library(car)
library(RColorBrewer)

# visually examine the relationships

scatterplotMatrix(piesSimData[, c(1, 2, 4, 5, 8, 9)], diag="histogram",
                  col=brewer.pal(3, "Paired"), ellipse=TRUE )

# check basic EFA structure (reality check)
factanal(piesSimData, factors=3)

### CFA (simple hierarchical model)
piesModel <- " General =~ i1 + i2 + i3
Feature =~ i4 + i5 + i6  + i7
Image   =~ i8 + i9 + i10 + i11
PIES =~ General + Feature + Image "
pies.fit <- cfa(piesModel, data=piesSimData)
summary(pies.fit, fit.measures=TRUE)
semPaths(pies.fit, what="est", fade=FALSE, residuals=FALSE,
         edge.label.cex=0.75)

# a 1-factor version: single PIES factor for all items
piesModelNH1 <- " PIES =~ i1 + i2 + i3 + i4 + i5 + i6  + 
i7 + i8 + i9 + i10 + i11 "
pies.fit.NH1 <- cfa(piesModelNH1, data=piesSimData)

# ~~ = correlation restriction
piesModelNH3 <- " General =~ i1 + i2 + i3
Feature =~ i4 + i5 + i6  + i7
Image   =~ i8 + i9 + i10 + i11
General ~~ 0.1*Feature
General ~~ 0.1*Image
Feature ~~ 0.1*Image "

pies.fit.NH3 <- cfa(piesModelNH3, data=piesSimData)

# comparing
summary(compareFit(pies.fit.NH1, pies.fit.NH3, pies.fit))

# plot model structure
semPaths(pies.fit, what="mod", fade=FALSE, residuals=FALSE, 
         structural=FALSE, fixedStyle=c("black",1), freeStyle=c("black",1))

