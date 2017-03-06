unemp <- read.csv("http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv",
                  sep=",")
scatter.smooth(x=1:length(unemp$Value),y = unemp$Value,
               degree = 2, span = 0.5, col = "grey")
