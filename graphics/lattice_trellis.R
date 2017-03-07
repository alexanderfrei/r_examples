library(lattice)
library(RColorBrewer)

pal = brewer.pal(2, 'Dark2')
my.settings <- list(
  superpose.polygon=list(col=pal, border="transparent")
)

xyplot(Sepal.Length ~ Petal.Length + Petal.Width | Species,
       data=iris, 
       auto.key=list(space="top", columns=2, points=F, rectangles=T),
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       grid = TRUE, 
       par.settings = my.settings)
       # type = c("p", "smooth"), col.line = "darkorange", lwd = 3)

####### by 2 factors
seg.df <- read.csv("data/seg.csv")
histogram(~subscribe | Segment + ownHome, data=seg.df)