crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv",
                  sep = "\t", header = T)

radius <- sqrt( crime$population / pi)

symbols(crime$murder,crime$burglary,circles = radius,inches = 0.4,
        fg = "white", bg = "red", xlab = "Убийства", ylab = "Кражи со взломом")
text(crime$murder,crime$burglary,crime$state, cex = 0.5)

