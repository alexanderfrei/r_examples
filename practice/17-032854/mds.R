setwd('./practice/17-032854/')

########################################################################################################################

toLib <- c("ggplot2", "cluster", "MASS", "ggrepel")
lapply(toLib, library, character.only = TRUE)
library(dplyr)

df = read.csv2('pct.csv', )
df = sapply(df, function(f) as.numeric(levels(f))[f])

sku = c('Pedigree Denta Tubos Junior for puppies','Pedigree Denta Stix for small breeds',
                 'Pedigree Denta Stix for large breeds','Purina Pro Plan Dental Pro Bar for adult dogs',
                 'Titbit Chewing snack "Dent" for medium breeds veal','Derevenskie lakomstva Zubochistiki for small breeds beef','Derevenskie lakomstva Zubochistiki "Calcium" for large breeds','TitBit Beef skin strips','TitBit Bovine root dogodent ','TitBit Dried mutton shin ','TitBit Mutton ear','8in 1 Bone for medium and large breeds ','TitBit Biscuits with chicken','Purina Pro Plan Biscuits with salmon and rice','Mnyams Chicken strips with chondroitin','Organix "Chicken dumbbells" ','Derevenskie lakomstva Lamb medallions for mini-breeds','TitBit Beef fillet strips ','Molina Chewing sausages with chicken','8in 1 Minis  Duck and plum with millet','Molina Meat hearts with multivitamins','Pedigree Jumbone mini beef','Pedigree Rodeo for adult dogs of all breeds','Pedigree Meaty Rolls Markies','Royal Canin Nutritional Supplement Educ ','8in 1 Training Pro Energy')

rownames(df) = sku
colnames(df) = sku

# library(RColorBrewer)
# RColorBrewer::brewer.pal.info
# heatmap(df, col=brewer.pal(name = 'PuBuGn', n = 6))

########################################################################################################################
# dendro

hc <- hclust(dist)

library("ape")

png("dendro.png", width = 1100, height = 800, )
plot(as.phylo(hc), cex = 1.4, label.offset = 1, font = 1)
dev.off()

########################################################################################################################
# mds

dist = dist(df)
mds = as.data.frame(cmdscale(dist))
mds$c3 = cutree(hc,3)
mds$c4 = cutree(hc,4)
mds$c5 = cutree(hc,5)

mds$cluster = mds$c3
p <- ggplot(mds, aes(V1, V2, label = rownames(mds), colour = factor(cluster))) + 
  geom_point() +
  geom_label_repel(aes(fill=factor(cluster)), colour="white", segment.colour="black", size = 5) + 
  theme_gray() + 
  theme(legend.position="none", 
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), 
        panel.border=element_blank())
# geom_text_repel(colour = "blue") +
p

########################################################################################################################
# output всего

library(WriteXLS)
to_xl = mds[,1:2]
WriteXLS(to_xl, "coords_total.xlsx", row.names = T)

# png("mds.png", width = 1000, height = 800, )
# p
# dev.off()

########################################################################################################################
# strata

df = read.csv2('pct_strata.csv')

df.msk = df[,1:26]
df.500 = df %>% select(ends_with(".1"))
df.100 = df %>% select(ends_with(".2"))

plot.dendro  <- function(x, names, title, dendro_name){
  
  x = sapply(x, function(f) as.numeric(levels(f))[f])
  rownames(x) = colnames(x) = names
  dist = dist(x)
  hc <- hclust(dist)
  
  png(dendro_name, width = 1100, height = 800, )
  plot(as.phylo(hc), cex = 1.4, label.offset = 1, font = 1, main = title)
  dev.off()
  
}

plot.dendro(df.msk, sku, "Moscow", "dendro_msk.png")
plot.dendro(df.500, sku, "500+", "dendro_500.png")
plot.dendro(df.100, sku, "100+", "dendro_100.png")

