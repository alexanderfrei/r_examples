setwd('./practice/17-032854/')

########################################################################################################################

toLib <- c("ggplot2", "cluster", "MASS", "ggrepel", "dplyr", "readxl")
lapply(toLib, library, character.only = TRUE)

df = read.csv2('pct.csv', )
df = sapply(df, function(f) as.numeric(levels(f))[f])

sku = c('Pedigree Denta Tubos Junior for puppies','Pedigree Denta Stix for small breeds',
                 'Pedigree Denta Stix for large breeds','Purina Pro Plan Dental Pro Bar for adult dogs',
                 'Titbit Chewing snack "Dent" for medium breeds veal','Derevenskie lakomstva Zubochistiki for small breeds beef','Derevenskie lakomstva Zubochistiki "Calcium" for large breeds','TitBit Beef skin strips','TitBit Bovine root dogodent ','TitBit Dried mutton shin ','TitBit Mutton ear','8in 1 Bone for medium and large breeds ','TitBit Biscuits with chicken','Purina Pro Plan Biscuits with salmon and rice','Mnyams Chicken strips with chondroitin','Organix "Chicken dumbbells" ','Derevenskie lakomstva Lamb medallions for mini-breeds','TitBit Beef fillet strips ','Molina Chewing sausages with chicken','8in 1 Minis  Duck and plum with millet','Molina Meat hearts with multivitamins','Pedigree Jumbone mini beef','Pedigree Rodeo for adult dogs of all breeds','Pedigree Meaty Rolls Markies','Royal Canin Nutritional Supplement Educ ','8in 1 Training Pro Energy')
sku2 = c('1. Pedigree Denta Tubos Junior for puppies','2. Pedigree Denta Stix for small breeds','3. Pedigree Denta Stix for large breeds','4. Purina Pro Plan Dental Pro Bar for adult dogs','5. Titbit Chewing snack "Dent" for medium breeds veal','6. Derevenskie lakomstva Zubochistiki for small breeds beef','7. Derevenskie lakomstva Zubochistiki "Calcium" for large breeds','8. TitBit Beef skin strips','9. TitBit Bovine root dogodent ','10. TitBit Dried mutton shin ','11. TitBit Mutton ear','12. 8in 1 Bone for medium and large breeds ','13. TitBit Biscuits with chicken','14. Purina Pro Plan Biscuits with salmon and rice','15. Mnyams Chicken strips with chondroitin','16. Organix "Chicken dumbbells" ','17. Derevenskie lakomstva Lamb medallions for mini-breeds','18. TitBit Beef fillet strips ','19. Molina Chewing sausages with chicken','20. 8in 1 Minis  Duck and plum with millet','21. Molina Meat hearts with multivitamins','22. Pedigree Jumbone mini beef','23. Pedigree Rodeo for adult dogs of all breeds','24. Pedigree Meaty Rolls Markies','25. Royal Canin Nutritional Supplement Educ ','26. 8in 1 Training Pro Energy')
sku3 = 1:26; sku = sku3
colnames(df) = rownames(df) = sku

# library(RColorBrewer)
# RColorBrewer::brewer.pal.info
# heatmap(df, col=brewer.pal(name = 'PuBuGn', n = 6))

########################################################################################################################
# dendro

hc <- hclust(dist)
library("ape")

# png("dendro.png", width = 1100, height = 800, )
# plot(as.phylo(hc), cex = 1.4, label.offset = 1, font = 1)
# dev.off()

########################################################################################################################
# mds

dist = dist(df)
mds = as.data.frame(cmdscale(dist))
mds$c3 = cutree(hc,3)
mds$c4 = cutree(hc,4)
mds$c5 = cutree(hc,5)
mds$c6 = cutree(hc,6)
mds$c7 = cutree(hc,7)


plot.mds = function(mds, cluster, plotname){
  mds$cluster = mds[[cluster]]
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  p = ggplot(mds, aes(V1, V2, label = rownames(mds), colour = factor(cluster))) + 
    geom_point() +
    geom_label_repel(aes(fill=factor(cluster)), colour="white", segment.colour="black", size = 5) + 
    theme_gray() + 
    scale_fill_manual(values=cbbPalette) + 
    theme(legend.position="none", 
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(), 
          panel.border=element_blank())
  ggsave(filename=plotname, plot=p, width = 8, height = 8)
}

plot.mds(mds, 'c3', "mds3.png")
plot.mds(mds, 'c4', "mds4.png")
plot.mds(mds, 'c5', "mds5.png")
plot.mds(mds, 'c6', "mds6.png")
plot.mds(mds, 'c7', "mds7.png")

########################################################################################################################
# output всего

# library(WriteXLS)
# to_xl = mds[,1:2]
# WriteXLS(to_xl, "coords_total.xlsx", row.names = T)
# 
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

########################################################################################################################
# mds2


#### strata
df.strata = read_excel("pct_mds2.xlsx", sheet = "strata", col_names = F)
df.strata = df.strata %>% select(-1,-2)
df.strata.msk = df.strata[1:26,]
df.strata.500 = df.strata[27:52,]
df.strata.100 = df.strata[53:78,]

#### buyer
df.buyer = read_excel("pct_mds2.xlsx", sheet = "buyer", col_names = F)
df.buyer = df.buyer %>% select(-1,-2)
df.buyer.heavy = df.buyer[1:26,]
df.buyer.medium = df.buyer[27:52,]
df.buyer.light = df.buyer[53:78,]

#### dog size
df.size = read_excel("pct_mds2.xlsx", sheet = "dog_size", col_names = F)
df.size = df.size %>% select(-1,-2)
df.size.small = df.size[1:26,]
df.size.medium = df.size[27:52,]
df.size.large = df.size[53:78,]

#### plot mds 

pal <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
sku = 1:26

plot.mds.clust  <- function(x, clust_num, title, names=sku, pallette=pal){
  
  x = as.data.frame(x)
  rownames(x) = colnames(x) = names
  
  dist = dist(x)
  hc = hclust(dist)
  mds = as.data.frame(cmdscale(dist))
  mds$cluster = cutree(hc, clust_num)
  
  p = ggplot(mds, aes(V1, V2, label = rownames(mds), colour = factor(cluster))) +
    geom_point(color = "gray25") +
    geom_label_repel(aes(fill=factor(cluster)), colour="white", segment.colour="black", size = 5, show.legend = F) +
    theme_gray() +
    scale_fill_manual(values=pallette) +
    theme(legend.position = "none", 
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.border=element_blank(), 
          plot.title = element_text(hjust = 0.5)) + 
    ggtitle(title)

  plotname = paste("mds_", title, sep="")
  p
}

#### multiplot 

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#### build multiplots 

build.plots = function(cl){
  p1 = plot.mds.clust(df.strata.msk, cl, "Moscow")
  p2 = plot.mds.clust(df.strata.500, cl, "500+")
  p3 = plot.mds.clust(df.strata.100, cl, "100+")
  p4 = plot.mds.clust(df.buyer.heavy, cl, "heavy buyer")
  p5 = plot.mds.clust(df.buyer.medium, cl, "medium buyer")
  p6 = plot.mds.clust(df.buyer.light, cl, "light buyer")
  p7 = plot.mds.clust(df.size.small, cl, "small dog")
  p8 = plot.mds.clust(df.size.medium, cl, "medium dog")
  p9 = plot.mds.clust(df.size.large, cl, "large dog")
  
  png(paste(as.character(cl),"_clust.png", sep=""), width = 1200, height = 900)
  multiplot(p1, p4, p7, p2, p5, p8, p3, p6, p9, cols=3)
  dev.off()
}

cl = 3; build.plots(cl)
cl = 6; build.plots(cl)
cl = 7; build.plots(cl)
cl = 4; build.plots(cl)
