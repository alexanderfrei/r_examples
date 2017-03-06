
education <- read.csv("http://datasets.flowingdata.com/education.csv",header = T)
summary(education)

library(lattice)

reading_colors <- c()
for (i in 1:length(education$state)){
  if (education$reading[i] > 523) {
    col <- "green2"
  } else{
    col <- "deepskyblue2"
  }
  reading_colors <- c(reading_colors,col)
}

pdf("graphics/result/parallel.pdf", family="URWPalladio", encoding="CP1251.enc",
    width = 10, height = 7)
parallelplot(education[,2:7], horizontal.axis = F, col=reading_colors)
dev.off()
