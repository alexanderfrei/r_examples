hotdogs <- read.csv("data/hot-dog-contest-winners.csv")

barplot(hotdogs$Dogs.eaten)
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col = "blue",
        border = NA, xlab = "Year", ylab = "Hotgods and buns eated")

fill_colors <- c()
for (i in 1:length(hotdogs$Country)){
  if (hotdogs$Country[i] == "United States") {
    fill_colors <- c(fill_colors, "#821122")
  } else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}

pdf("graph/result/barplots.pdf", family="URWPalladio", encoding="CP1251.enc")
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col = fill_colors, space=0.5,
        border = NA, xlab = "Year", ylab = "Hotgods and buns eated",
        main = "Результаты Нейтановского турнира\n по поеданию хот-догов, 1980-2010")
dev.off()


