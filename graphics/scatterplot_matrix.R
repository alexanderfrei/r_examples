# with trend
crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.csv",
                  sep = ",", header = T)
crime2 <- crime[!(crime$state %in% c("District of Columbia","United States")),]
pairs(crime2[,2:9], panel = panel.smooth)

# with density in diag
sat.df <- read.csv("data/park.csv")
library(car)
scatterplotMatrix(sat.df)

# with hist 
library(gpairs)
gpairs(sat.df)