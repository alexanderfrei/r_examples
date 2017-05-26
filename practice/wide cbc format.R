wide.cbc.format <- function(cont_list=NULL, file="data.csv", none=T, csv=T) {
  
  library(stringr)
  library(dummies)
  library(dplyr)

  data <- read.csv(file, sep=';')
  
  # continuous att
  cont_list <- cont_list
  cont_att <- paste("att", cont_list, sep = "")
  
  # to/not to one hot vectors
  cols <- colnames(data)
  one_hot <- cols[grep("att.*",cols)]
  one_hot <- one_hot[!one_hot %in% cont_att]
  
  # one hot encoding
  data <- as.data.frame(dummy.data.frame(data, names=one_hot, sep="_"))
  df.full <- data[!grepl("att.*_0", colnames(data))]
  colnames <- colnames(df.full)
  
  # get last level in categorical
  att <- colnames[grep("att._.*",colnames)]
  drop_match <- str_match_all(att, "(att.)(.*)")
  
  to_drop <- NULL
  for (i in 2:length(drop_match)){
    cur_var <- unlist(drop_match[i-1])[2]
    next_var <- unlist(drop_match[i])[2]
    if (cur_var != next_var) {
      to_drop <- c(to_drop, unlist(drop_match[i-1])[1])
    }
    if (i == length(drop_match)) {
      to_drop <- c(to_drop, unlist(drop_match[i])[1])
    }
  }
  
  # оставляем последний уровень первого атрибута, если есть ноне
  if (none) to_drop <- to_drop[-grep("att1.*", to_drop)]
  
  # replace att with x
  colnames(df.full) <- gsub("att", "x", colnames)
  df.noref <- df.full[! colnames %in% to_drop]
  
  
  
  # write output
  if (csv) {
    write.table(df.full, "data_full.csv", sep=';', row.names=F)
    write.table(df.noref, "data_no_reference.csv", sep=';', row.names=F)
  }
  else {
    list("df.full"=df.full, "df.noref"=df.noref)
  }
}

# setwd("c:\\R workspace\\r_examples\\practice\\data")
# data <- read.csv("data.csv", sep=';')
# head(data)
# df_new <- wide.cbc.format(file="data.csv", csv=F)
# dim(df_new$df.full)
# wide.cbc.format(1)
# wide.cbc.format(1, none=F)
# wide.cbc.format(c(1,2))

