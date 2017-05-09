setwd("W://iMQ//X_Marketing//!!!Projects 2017//=PEPSI//Pepsi_17-018883-01_CSD competitive turf//4. DATA (coding, de, dp)//programming//tr//cbc//batch_lc15")
library(dplyr)

rm(uti, share, out)

name <- "cho_full_batch_"
n.files <- 45

uti <- read.csv(paste(name,as.character(1),"1_15_groups_individual_utilities.csv", sep=""), sep=";")
resp.id <- uti$п.їRespondent

uti <- as.data.frame(apply(uti, 2, function(x) as.numeric(gsub(",", ".", as.character(x)))))
uti <- select(uti, matches("(Level)|(None)|(none)"))
share <- exp(uti)
share <- 100 * share / rowSums(share)

out <- as.data.frame(matrix(0, dim(uti)[1], dim(uti)[2]))
colnames(out) <- colnames(uti)
out <- out + share

read_LC <- function(n_iter){
  uti <- read.csv(paste(name, as.character(n_iter), "_15_groups_individual_utilities.csv", sep=""), sep=";")
  uti <- as.data.frame(apply(uti, 2, function(x) as.numeric(gsub(",", ".", as.character(x)))))
  uti <- select(uti, matches("(Level)|(None)|(none)"))
  share <- exp(uti)
  share <- 100 * share / rowSums(share)
  share
}

out.all <- out
for (i in 2:n.files){
  out.i <- read_LC(i)
  out.all <- rbind(out.all, out.i)
  out.mean <- out + out.i
}

mean_uti <- log(out.mean / n.files)
mean_uti <- cbind(resp.id, mean_uti)
out.all <- cbind(resp.id, out.all)

write.csv2(mean_uti, "merge/mean_uti.csv")
write.csv2(out.all, "merge/all_uti.csv")

