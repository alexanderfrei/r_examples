setwd('practice/16-092464')
path <- "W:/iMQ/Projects/_PROJECTS/Uber/Uber Moscow U&A/4. DATA (coding, de, dp)/Programming/"

########################################################################################################

library(cluster) 
library("factoextra")
library("FactoMineR")

df <- read.csv("segm.csv", sep=";")
colnames(df)[1] <- "daytime"
df$transport <- NULL
df$InterviewID <- NULL

########################################################################################################

dummy <- colnames(df[grepl("(a51|a7).*",colnames(df))])


as.data.frame.matrix(table(df$a5_1.1, df[c("a5_1.2","a5_1.3")]))[2]

df[c("a5_1.2","a5_1.3")]

test_df <- as.data.frame.matrix(table(df$daytime, df$a6to_1))
test_df <- t(apply(test_df, 1, prop.table))


res.pca <- PCA(test_df,  graph = FALSE)
fviz_pca_biplot(res.pca, repel = TRUE)

