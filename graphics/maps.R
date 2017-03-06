
##################################################

# world map
store.df <- read.csv("data/to_describe.csv")  
p1sales.sum <- aggregate(store.df$p1sales, 
                         by=list(country=store.df$country), sum)

library(rworldmap)
library(RColorBrewer)
p1sales.map <- joinCountryData2Map(p1sales.sum, joinCode = "ISO2", 
                                   nameJoinColumn = "country")

mapCountryData(p1sales.map, nameColumnToPlot="x", 
               mapTitle="Total P1 sales by Country",
               colourPalette=brewer.pal(7, "Blues"), 
               catMethod="fixedWidth", addLegend=F)

##################################################



