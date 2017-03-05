tvs <- read.table("http://datasets.flowingdata.com/tv_sizes.txt", 
                  sep = "\t", header = T)

tvs <- tvs[tvs$size < 80,]
tvs <- tvs[tvs$size > 10,]
breaks = seq(10,80, by = 5)

pdf("graphics/result/hist8.pdf", family="URWPalladio", encoding="CP1251.enc")
par(mfrow = c(4,2))
for (i in seq(2002,2009)) {
  hist(tvs[tvs$year == i,]$size, 
       main = i, breaks = breaks, xlab = "",ylab = "",
       col = "darkolivegreen2")
}
dev.off()