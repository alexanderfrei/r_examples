piesSimData <- read.csv("data/piesSimData.csv")
library(lavaan)
library(semTools)
library(semPlot)
library(car)
library(RColorBrewer)

####################################
##### path analysis (general SEM)
# specifying Quality =~ 0 * Cost because of odd results otherwise;
# Also we want to take them as separate, orthogonal influences here
satModel <- " Quality =~ CSat + Value + q1 + q2 + q3  + 0*Cost
Cost    =~ Value + Repeat + c1 + c2 + c3
Value   =~ CSat + v1 + v2 + v3
CSat    =~ Repeat + cs1 + cs2 + cs3
Repeat  =~ r1 + r2 + r3 "

### load the data if you prefer
satSimData <- read.csv("data/satSimData.csv")
summary(satSimData)

# basic psychometrics
alpha(satSimData)

sat.fit <- sem(satModel, data= satSimData, std.lv=TRUE) # standardized latent variables
summary(sat.fit, fit.measures=TRUE)

semPaths(sat.fit, what="est", fade=FALSE, residuals=FALSE, 
         layout="tree", structural=TRUE, nCharNodes=7, edge.label.cex=1)

# now define a simpler test model
# let each latent only influence one other latent
satAltModel <- " Quality =~ CSat  + q1 + q2 + q3 + 0*Cost
                 Cost    =~ Value + c1 + c2 + c3
                 Value   =~ CSat  + v1 + v2 + v3
                 CSat    =~ Repeat + cs1 + cs2 + cs3
                 Repeat  =~ r1 + r2 + r3 "

satAlt.fit <- sem(satAltModel, data=satSimData, std.lv=TRUE)
semPaths(satAlt.fit, what="est", fade=FALSE, residuals=FALSE, 
         layout="tree", structural=TRUE, nCharNodes=7, edge.label.cex=1)

summary(compareFit(sat.fit, satAlt.fit, nested=TRUE))

semPaths(sat.fit, what="mod", fade=FALSE, residuals=FALSE, 
         layout="tree", structural=TRUE, nCharNodes=7,
         fixedStyle=c("white",1))     # using this to omit the path fixed to 0
