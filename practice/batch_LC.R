setwd("W://iMQ//X_Marketing//!!!Projects 2017//=PEPSI//Pepsi_17-018883-01_CSD competitive turf//4. DATA (coding, de, dp)//programming//rus//sim//cbc_all//LC//lc2")
library(dplyr)

rm(uti, share, out)

uti <- read.csv(paste("lc15_",as.character(1),"_15_groups_individual_utilities.csv", sep=""), sep=";")
uti <- as.data.frame(apply(uti, 2, function(x) as.numeric(gsub(",", ".", as.character(x)))))
uti <- select(uti, starts_with("Level"))
share <- exp(uti)
share <- 100 * share / rowSums(share)

out <- as.data.frame(matrix(0, dim(uti)[1], dim(uti)[2]))
colnames(out) <- colnames(uti)
out <- out + share

read_LC <- function(n_iter, out_matrix){
  uti <- read.csv(paste("lc15_",as.character(n_iter),"_15_groups_individual_utilities.csv", sep=""), sep=";")
  uti <- as.data.frame(apply(uti, 2, function(x) as.numeric(gsub(",", ".", as.character(x)))))
  uti <- select(uti, starts_with("Level"))
  share <- exp(uti)
  share <- 100 * share / rowSums(share)
  out_matrix + share
}

for (i in 2:40){
  out <- read_LC(i, out)
}

mean_uti <- log(out / 40)

write.csv(mean_uti, "mean/uti.csv")

