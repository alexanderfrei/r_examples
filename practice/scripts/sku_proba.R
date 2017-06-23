library(dplyr)
library(data.table)

sku  <- read.csv("data/cho_turf.csv",sep = ";")
colnames(sku)[1] <- "Id"

count_freq <- function(x, v) {
  x <- ifelse(x==v,1,0)
  sum(x)}

long_df <- sku %>%
  group_by(Id, att1) %>%
  summarise(proba = count_freq(cho, 1) / n(),
            cho = count_freq(cho, 1),
            n = n())

out <- dcast(setDT(long_df), formula = Id ~ att1, value.var = c("proba","cho","n"))
write.csv2(out, "cho_proba.csv")
