setwd('../.././networks')

####################################################################################  

nodes <- read.csv("./Data files/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("./Data files/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL

####################################################################################  
# library

library("visNetwork") 
library("htmlwidgets")

#################################################################################### 

nodes$shape <- "dot"  
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$title <- nodes$media # Text on click
nodes$label <- nodes$type.label # Node label
nodes$size <- nodes$audience.size # Node size
nodes$borderWidth <- 2 # Node border width

nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]
nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"

net1 = visNetwork(nodes, links)
net1
saveWidget(net1, "network1.html")

#################################################################################### 

links$width <- 1+links$weight/8 # line width
links$color <- "gray"    # line color  
links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
links$smooth <- FALSE    # should the edges be curved?
links$shadow <- FALSE    # edge shadow

net2 = visNetwork(nodes, links)
net2
saveWidget(net2, "network2.html")

#################################################################################### 

links$arrows <- "" 
links$width  <- 1

net3 = visNetwork(nodes, links,  height="1000px", width="100%") %>% 
  visOptions(highlightNearest = TRUE, selectedBy = "type.label")
net3 
saveWidget(net3, "network3.html")

#################################################################################### 

# visPhysics(net, solver= "barnesHut", 
#            barnesHut = list("gravitationalConstant"=-20000), 
#            maxVelocity = 10, timestep = 0.2)
visPhysics(net, enabled = F)

####################################################################################  

?visPhysics # Physics
?visOptions # available options 
?visLayout  # available layouts
?visGroups  # using node groups
?visLegend  # adding a legend
