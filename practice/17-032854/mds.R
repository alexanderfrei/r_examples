setwd('./practice/17-032854/')

########################################################################################################################

toLib <- c("ggplot2", "cluster", "MASS", "ggrepel")
lapply(toLib, library, character.only = TRUE)

df = read.csv2('pct.csv', )
df = sapply(df, function(f) as.numeric(levels(f))[f])

rownames(df) = c('Pedigree Denta Tubos Junior for puppies','Pedigree Denta Stix for small breeds',
                 'Pedigree Denta Stix for large breeds','Purina Pro Plan Dental Pro Bar for adult dogs',
                 'Titbit Chewing snack "Dent" for medium breeds veal','Derevenskie lakomstva Zubochistiki for small breeds beef','Derevenskie lakomstva Zubochistiki "Calcium" for large breeds','TitBit Beef skin strips','TitBit Bovine root dogodent ','TitBit Dried mutton shin ','TitBit Mutton ear','8in 1 Bone for medium and large breeds ','TitBit Biscuits with chicken','Purina Pro Plan Biscuits with salmon and rice','Mnyams Chicken strips with chondroitin','Organix "Chicken dumbbells" ','Derevenskie lakomstva Lamb medallions for mini-breeds','TitBit Beef fillet strips ','Molina Chewing sausages with chicken','8in 1 Minis  Duck and plum with millet','Molina Meat hearts with multivitamins','Pedigree Jumbone mini beef','Pedigree Rodeo for adult dogs of all breeds','Pedigree Meaty Rolls Markies','Royal Canin Nutritional Supplement Educ ','8in 1 Training Pro Energy')

########################################################################################################################
# mds

dist = dist(df)
mds = as.data.frame(cmdscale(dist))

p <- ggplot(mds, 
            aes(V1, V2, 
                label = rownames(mds))) + 
  geom_point() +
  xlab("X") + 
  ylab("Y") +
  geom_text_repel(colour = "darkblue")
p

########################################################################################################################
# dendro

library("ape")
hc <- hclust(dist)
plot(as.phylo(hc), cex = 1.4, label.offset = 1, font = 1)


