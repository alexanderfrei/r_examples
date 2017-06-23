
library(tidyr)

cbc_design = read.csv("data/design/cbc_design.csv")
mx_design = read.csv2("data/design/maxdiff_design.csv")
base = read.csv("data/design/base_id.csv")

base = base[1:3]
colnames(base) = c("id","cbc_id","mx_id")

turf_des = cbc_design %>% unite(turf_des, Task, Item) %>% spread(turf_des, att1, sep = "_")
mx_des = mx_design %>% unite(mx_des, Task, Item) %>% spread(mx_des, att1, sep = "_")
mx_des
nrow = dim(base)[1]

df<-NULL
for (i in 1:nrow){
  row = turf_des[base[i, "cbc_id"], -1]
  df = rbind(df, row)
}

df1<-NULL
for (i in 1:nrow){
  row = mx_des[base[i, "mx_id"], -1]
  df1 = rbind(df1, row)
}

write.csv2(cbind(base, df, df1), "data/design/exp_base.csv", row.names = F)
