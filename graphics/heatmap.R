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

# gplots heatmap with dendro
brand.ratings <- read.csv("data/brands_pca.csv")
brand.sc <- brand.ratings
brand.sc[, 1:9] <- scale(brand.ratings[, 1:9]) # rescaling
brand.mean <- aggregate(. ~ brand, data=brand.sc, mean) # agg means by brands
rownames(brand.mean) <- brand.mean[, 1] # use brand for the row names
brand.mean <- brand.mean[, -1]          # remove brand name column

library(gplots)
heatmap.2(as.matrix(brand.mean), 
          col=brewer.pal(9, "GnBu"), trace="none", 
          main="Brand attributes")
