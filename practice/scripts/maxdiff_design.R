setwd("./practice")

maxdiff_design = function(file, n_item, out_file, sep=','){
  # конвертация формата максдиффа в наш обычный дизайн 
  # file: csv
  # n_item: число айтемов на экране 
  # out_file: выходной csv с разделителем ;
  # sep: разделитель в csv файле (; или ,). По умолчанию запятая.
  
  library(tidyr)
  library(dplyr)
  
  df = read.csv(file, sep = sep)
  
  colnames(df)[1:2] = c("Rot", "Task")
  items = colnames(df[3:(3+n_item-1)])
  
  df.long = df %>% gather_('Item', 'att1', items)
  df.long['Item'] = apply(df.long['Item'], 1, function(s) {as.numeric(gsub('Item', '', s))} )
  df.sort = df.long %>% arrange(Rot, Task, Item)
  
  write.csv2(df.sort, out_file, row.names = F)
  
}

maxdiff_design("data/pepsi_Design.csv", 3, "data/pepsi_Design_out.csv")


