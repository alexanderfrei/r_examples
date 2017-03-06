posts <- read.csv("http://datasets.flowingdata.com/post-data.txt")

library("portfolio")
map.market(id=posts$id,
           area=posts$views, 
           group=posts$category,
           color=posts$comments,
           main = "Flowing Data Map")

library(treemap)
library(RColorBrewer)
pdf('graphics/result/treemap.pdf')
treemap(posts,
        title = "Cross post category by ID, rectangle size - number of comments",
        title.legend = "Number of views",
        index=c("category","id"),
        vSize="comments",
        vColor="views",
        n = 4,
        type="manual",
        palette=brewer.pal(4,"OrRd"),
        bg.labels = "transparent", 
        align.labels = c("left", "top"),
        #border.col = "white",
        #fontcolor.labels = "white"
        )
dev.off()
# ?treemap
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE)
# library(colorspace)
# pal <- choose_palette()

