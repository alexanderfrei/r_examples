hotdog_places <- read.csv("http://datasets.flowingdata.com/hot-dog-places.csv",
                          sep = ",", header = T)
years <- c()
for (i in 0:10){
  if (i %/% 10 == 0) {
    years <- c(years, paste("200",i, sep=""))
  }
  if (i %/% 10 == 1) {
    years <- c(years, paste("20",i, sep=""))
  }
}
names(hotdog_places) <- years
hotdog_matrix <- as.matrix(hotdog_places)

barplot(hotdog_matrix, space=0.5,
        border = NA, xlab = "Год", ylab = "HDB",
        main = "Результаты Нейтановского турнира\n по поеданию хот-догов, 2000-2010")
