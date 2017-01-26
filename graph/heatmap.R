player <- read.csv("http://datasets.flowingdata.com/ppg2008.csv",  header = T)
player <- player[order(player$PTS, decreasing = F),]
row.names(player) <- player$Name
player <- player[,2:20]
player <- data.matrix(player)

library("RColorBrewer")

pdf("graph/result/heatmap.pdf", family="URWPalladio", encoding="CP1251.enc")
heatmap <- heatmap(player,Rowv = NA, Colv = NA, col = brewer.pal(8, "Blues"), 
                   scale = "column", margins = c(5,10))
dev.off()

