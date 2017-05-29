setwd("./practice")

library(dplyr)

df = read.csv2("./data/sku_prod_price.csv")
df = as.data.frame(sapply(df, 
                          function(f) as.numeric(levels(f))[f]))
df
cols = colnames(df)
# df = df[,grepl(pattern = "X.*", cols)]
df = df[1:21]
lin_coef = c(-1,-0.666666667,-0.333333333,0,0.333333333,0.666666667,1)
ncol = length(lin_coef); nrow = dim(df)[1]

out.df = NULL
for (col in colnames(df)){
  tmp_df = data.frame(matrix(nrow = nrow, ncol = ncol))
  tmp_cols = vector()
  for (i in 1:ncol){
    tmp_cols = c(tmp_cols, paste(col, as.character(i), sep = "."))
  }
  colnames(tmp_df) = tmp_cols
  for (i in 1:ncol){
    tmp_df[i] = df[[col]] * lin_coef[i]
  }
  out.df = c(out.df, tmp_df)
}

out.df = as.data.frame(out.df)

write.csv2(out.df, "./output/sku_prod_price.csv")
