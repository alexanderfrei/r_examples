piesSimData <- read.csv("data/piesSimData.csv")

library(lavaan)
library(semTools)
library(semPlot)
library(semPLS)
library(car)
library(RColorBrewer)

####################################
#### semPLS (Partial Least Squares)
# (if general SEM not work)

set.seed(90704)
satSimData2 <- satSimData[sample(nrow(satSimData), 50), ] # N = 50
describe(satSimData2)

# try to fit CB-SEM to it ... it fails
sat.fit2 <- sem(satModel, data= satSimData2, std.lv=TRUE)
# and we see extreme values of coefficients
summary(sat.fit2, fit.measures=TRUE)

# the measurement model (manifest variables)
# in "from, to" format: Col1 == "arrow from", Col2 == "arrow to"
satPLSmm <- matrix(c(
  "Quality", "q1",
  "Quality", "q2",
  "Quality", "q3",
  "Cost",    "c1",
  "Cost",    "c2",
  "Cost",    "c3",
  "Value",   "v1",
  "Value",   "v2",
  "Value",   "v3",
  "CSat",    "cs1",
  "CSat",    "cs2",
  "CSat",    "cs3",
  "Repeat",  "r1",
  "Repeat",  "r2",
  "Repeat",  "r3" ), ncol=2, byrow=TRUE)


# specify the structural model (latent variable relationships)
# in "from, to" format: Col1 == "arrow from", Col2 == "arrow to"
satPLSsm <- matrix(c(
  "Quality", "CSat",
  "Quality", "Value",
  "Cost",    "Value",
  "Cost",    "Repeat",
  "Value",   "CSat",
  "CSat",    "Repeat" ), ncol=2, byrow=TRUE)

satPLS.mod <- plsm(data=satSimData2, strucmod=satPLSsm, measuremod=satPLSmm)
satPLS.fit <- sempls(model=satPLS.mod, data=satSimData2)

# the manifest variable loadings (measurement model)
plsLoadings(satPLS.fit)

# examine the structural coefficients (structural model)
pathCoeff(satPLS.fit)

# export the structural paths & coefficients in .dot file
# graphviz

pathDiagram(satPLS.fit, file = "graphics/result/satPLSstruc", full = FALSE, digits = 2,
            edge.labels = "values", output.type = "graphics", 
            graphics.fmt = "pdf")

# R-squared for the latent variables
rSquared(satPLS.fit)

# now what if we use the FULL N=200 dataset (instead of N=50)?
satPLS.modF <- plsm(data=satSimData, strucmod=satPLSsm, measuremod=satPLSmm)
satPLS.fitF <- sempls(model=satPLS.mod, data=satSimData)

# check its coefficients and fit
pathCoeff(satPLS.fitF)

# bootstrap PLS for coefficients confidence intervals
# check coefs stability
set.seed(04460)
satPLS.bootF <- bootsempls(satPLS.fitF, nboot=500, start="ones")
# summary(satPLS.bootF, type = "bca", level = 0.9)

# coefficients are stable:
parallelplot(satPLS.bootF, reflinesAt = 0, alpha=0.8,
  varnames=attr(satPLS.bootF$t, "path")[16:21],
  main="Path coefficients in 500 PLS bootstrap iterations (N=200)")
