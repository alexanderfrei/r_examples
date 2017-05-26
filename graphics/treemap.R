posts <- read.csv("http://datasets.flowingdata.com/post-data.txt")

library("portfolio")
map.market(id=posts$id,
           area=posts$views, 
           group=posts$category,
           color=posts$comments,
           main = "Flowing Data Map")

library(treemap)
library(RColorBrewer)

pdf('graphics/result/treemap.pdf', width = 10, height = 8)

treemap(posts,
        index=c("category","id"),
        vSize="comments", vColor="views",
        n = 4,
        type="manual",
        palette=brewer.pal(4,"OrRd"),
        title = "Cross post category by ID, rectangle size - number of comments",
        title.legend = "Number of views",
        bg.labels = "transparent", 
        align.labels = c("left", "top"),
        overlap.labels = 0,
        #border.col = "white",
        #fontcolor.labels = "white"
        )

dev.off()


?treemap

