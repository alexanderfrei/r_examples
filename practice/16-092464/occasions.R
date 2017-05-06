setwd('practice/16-092464')

library("arules")
library("arulesViz")
library(dummies)

library(plyr)
library(car)
library(dplyr)
library(psych)

library("factoextra")
library("FactoMineR")

df <- read.csv("segm.csv", sep=";")

##################################################################################################
### clean data

colnames(df)[1] <- "daytime"
df$transport <- NULL
df$InterviewID <- NULL

##################################################################################################
### dummy

dummy <- colnames(df[grepl("(a5_|a51|a7).*",colnames(df))])
for (qt in dummy){
  levels(df[[qt]]) <-  c("No","Yes")
  df[[qt]] <- ifelse(df[[qt]] == "Yes", T, F)
}

##################################################################################################
### union transports by type

# df$transport <- as.factor(recode(df$transport, c(1,2,3)='Land transport'; 4='Metro'; 6='Personal car';
#              c(5,7,8,10)='Other'; 9='Taxi'"))

df$land_transport <- df$a5_1.1 | df$a5_1.2 | df$a5_1.3
df$metro <- df$a5_1.4
df$train <- df$a5_1.5 | df$a5_1.8
df$personal_car <- df$a5_1.6
df$company_car <- df$a5_1.7
df$taxi <- df$a5_1.9

df[grepl("(a5_).*",colnames(df))] <- NULL

##################################################################################################
### factors

df$a8_1 <- as.factor(car::recode(df$a8_1, "c('Child','Colleague','Friends','Parents','Relatives')='Other'"))
df$trip_duration <- 
  as.factor(car::recode(df$trip_duration, "c('106-120 min','91-105 min','More than 2 hours')='90 min+'"))


recode.small.val <- function(var, val){
  library(car)
  
  levels(var) <- gsub("'","",levels(var))
  freq <- as.data.frame(table(var))
  
  lev <- 
    paste("c('", 
          paste(gsub("'","",as.character(freq[freq$Freq < val,]$var)), collapse = "','"),
          "')", 
          collapse="",sep="")
  ret <- as.factor(car::recode(var, paste(lev, "='Other'")))
  
}

df$a6from_1 <- recode.small.val(df$a6from_1, 50)
df$a6to_1 <- recode.small.val(df$a6to_1, 50)

# dummy(data=df, x="a8_1", sep = ".")

##################################################################################################
### transport pca

# fac <- c("a6from_1", "a6to_1", "a8_1", "trip_duration", "daytime")
# dm <- grepl("(a51_|a7_).*",colnames(df))
# 
# prep_pca <- function(transport){
#   base <- sum(df[[transport]])
#   fac_ <- unlist(apply(df[df[[transport]], fac], 2, function(x) table(x) / base))
#   dm_ <-  unlist(apply(df[df[[transport]], dm], 2, function(x) table(x)[2] / base))
#   fac_[is.na(fac_)] <- 0
#   dm_[is.na(dm_)] <- 0
#   c(fac_,dm_)
# }
# 
# land_transport <- prep_pca("land_transport")
# metro <- prep_pca("metro")
# train <- prep_pca("train")
# personal_car <- prep_pca("personal_car")
# company_car <- prep_pca("company_car")
# taxi <- prep_pca("taxi")
# 
# transport.pca <- rbind(land_transport, metro, train, personal_car, company_car, taxi)
# 
# res.pca <- PCA(transport.pca,  graph = FALSE)
# pdf(file = "biplot_transport.pdf", width = 20, height = 20)
# fviz_pca_biplot(res.pca, repel = T, geom = c("point","text"))
# dev.off()


##################################################################################################
### mine occasions

# trans <- as(df, "transactions")
# 
# mine.rules <- function(var, sup, conf=0.8){
#   rules <- apriori(trans, 
#                    appearance = list(rhs = var, default="lhs"), 
#                    parameter = list(support=sup, minlen=4, maxlen=7, confidence=conf))
#   rules.sorted <- sort(rules, by="confidence")
#   subset.matrix <- is.subset(rules.sorted, rules.sorted)
#   subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
#   redundant <- colSums(subset.matrix, na.rm=T) >= 1
#   rules.pruned <- rules.sorted[!redundant]
#   inspect(rules.pruned)
# }
# 
# n_occ = 3
# mined <- data.frame(matrix(nrow = n_occ))
# mined$land <- as.data.frame(mine.rules("land_transport", 0.02))$lhs[1:n_occ]
# mined$metro <- as.data.frame(mine.rules("metro", 0.03))$lhs[1:n_occ]
# mined$train <- as.data.frame(mine.rules("train", 0.003))$lhs[1:n_occ]
# mined$car <- as.data.frame(mine.rules("personal_car", 0.009))$lhs[1:n_occ]
# mined$company_car <- as.data.frame(mine.rules("company_car", 0.003))$lhs[1:n_occ]
# mined$taxi <- as.data.frame(mine.rules("taxi", 0.003))$lhs[1:n_occ]
# mined[1] <- NULL
# 
# str(mined)
# write.csv2(mined, "rules.csv")

##################################################################################################
### occasions pca

dm <- grepl("^(a51_|a7_|land_transport|metro|company_car|taxi|personal_car|train).*",colnames(df))
fac <- c("a6from_1", "a6to_1", "a8_1", "trip_duration", "daytime")

df$occ.land.1 <- df$a7_1.5 & df$a7_1.25 & df$a8_1=="Me"
df$occ.land.2 <- df$a51_1.1&df$a7_1.5&df$a7_1.25
df$occ.land.3 <- df$a51_1.1&df$a8_1=="Me"&df$trip_duration=="61-75 min"&df$metro

df$occ.metro.1 <- df$a51_1.1&df$trip_duration=="76-90 min"&df$land_transport
df$occ.metro.2 <- df$a8_1=="Me"&df$trip_duration=="76-90 min"&df$land_transport
df$occ.metro.3 <- df$daytime=="Morning time"&df$a6from_1=="Home"&df$a6to_1=="Workplace"&df$a7_1.5&df$a8_1=="Me"

df$occ.train.1 <- df$a6to_1=="Workplace"&df$a7_1.6&df$a7_1.8&df$trip_duration=="90 min+"&df$metro
df$occ.train.2 <- df$a51_1.1&df$a6to_1=="Workplace"&df$a7_1.6&df$a7_1.8&df$trip_duration=="90 min+"
df$occ.train.3 <- df$a7_1.6&df$a7_1.8&df$a7_1.7&df$trip_duration=="90 min+"&df$metro

df$occ.car.1 <- df$daytime=="Evening time"&df$a51_1.2&df$a7_1.18
df$occ.car.2 <- df$a51_1.2&df$a7_1.25&df$a8_1=="Partner"
df$occ.car.3 <- df$daytime=="Evening time"&df$a6to_1=="Home"&df$a8_1=="Partner"

df$occ.company_car.1 <- df$daytime=="Day time"&df$a51_1.9&df$a6from_1=="Workplace"&df$a8_1=="Employer"
df$occ.company_car.2 <- df$a51_1.9&df$a6from_1=="Workplace"&df$a8_1=="Employer"

df$occ.taxi.1 <- df$a51_1.2&df$a7_1.24&df$a8_1=="Me"
df$occ.taxi.2 <- df$daytime=="Night time"&df$a51_1.2&df$a6to_1=="Home"&df$a8_1=="Me"
df$occ.taxi.3 <- df$a6from_1=="Friends/relatives"&df$a6to_1=="Home"&df$a7_1.24&df$a8_1=="Me"

# чудесные базы по оккейженам
# apply(occ.df, 2, sum) 

my.rbind <- function(df, vec2){
  rbind.fill(df,as.data.frame(t(vec2)))
}

occ.df <- as.data.frame(t(prep_pca("occ.land.1")))
occ.qt <- colnames(df[grepl("^occ.*",colnames(df))])[-1]

for (qt in occ.qt) {
  occ.df <- my.rbind(occ.df,prep_pca(qt))
}

occ.df[is.na(occ.df)] <- 0
row.names(occ.df) <- c("LAND TYPE: a7_1.5, a7_1.25, a8_1=Me","LAND TYPE: a51_1.1, a7_1.5, a7_1.25","LAND TYPE: a51_1.1, a8_1=Me, trip_duration=61-75 min, metro","METRO: a51_1.1, trip_duration=76-90 min, land_transport","METRO: a8_1=Me, trip_duration=76-90 min, land_transport","METRO: daytime=Morning time, a6from_1=Home, a6to_1=Workplace, a7_1.5, a8_1=Me","TRAIN: a6to_1=Workplace, a7_1.6, a7_1.8, trip_duration=90 min+, metro","TRAIN: a51_1.1, a6to_1=Workplace, a7_1.6, a7_1.8, trip_duration=90 min+","TRAIN: a7_1.6, a7_1.8, a7_1.7, trip_duration=90 min+, metro","CAR: daytime=Evening time, a51_1.2, a7_1.18","CAR: a51_1.2, a7_1.25, a8_1=Partner","CAR: daytime=Evening time, a6to_1=Home, a8_1=Partner","COMPANY CAR: daytime=Day time, a51_1.9, a6from_1=Workplace, a8_1=Employer","COMPANY CAR: a51_1.9, a6from_1=Workplace, a8_1=Employer","TAXI: a51_1.2, a7_1.24, a8_1=Me","TAXI: daytime=Night time, a51_1.2, a6to_1=Home, a8_1=Me","TAXI: a6from_1=Friends/relatives, a6to_1=Home, a7_1.24, a8_1=Me")
occ.pca <- PCA(occ.df,  graph = FALSE)

pdf(file = "occasions.pdf", width = 25, height = 25)
fviz_pca_biplot(occ.pca, repel = T, geom = c("point","text"))
dev.off()

write.csv2(t(occ.df), "occasions_pct_biplot.csv")

##################################################################################################
### cluster pca
### возьмем кластеры из окружения, они уже там

df$hclust_4 <- hclust.4
df <- cbind(df[!names(df) %in% "hclust_4"], 
            apply(dummy(data = df, x = "hclust_4", sep = "."), 2, as.logical))
df$hclust_5 <- hclust.5
df <- cbind(df[!names(df) %in% "hclust_5"], 
            apply(dummy(data = df, x = "hclust_5", sep = "."), 2, as.logical))
df$hclust_6 <- hclust.6
df <- cbind(df[!names(df) %in% "hclust_6"], 
            apply(dummy(data = df, x = "hclust_6", sep = "."), 2, as.logical))


make.pca.cluster <- function(df, out.df, pattern) {
  cl.qt <- colnames(df[grepl(pattern,colnames(df))])
  for (qt in cl.qt) {
    out.df <- my.rbind(out.df,prep_pca(qt))
  }
  out.df[is.na(out.df)] <- 0
  out.df
}

cl4.df <- as.data.frame(t(prep_pca("hclust_4.1")))
cl5.df <- as.data.frame(t(prep_pca("hclust_5.1")))
cl6.df <- as.data.frame(t(prep_pca("hclust_6.1")))

cl4.df <- make.pca.cluster(df, cl4.df, "^hclust_4.[2-4]")
cl5.df <- make.pca.cluster(df, cl5.df, "^hclust_5.[2-5]")
cl6.df <- make.pca.cluster(df, cl6.df, "^hclust_6.[2-6]")

cl4.pca <- PCA(cl4.df,  graph = FALSE)
cl5.pca <- PCA(cl5.df,  graph = FALSE)
cl6.pca <- PCA(cl6.df,  graph = FALSE)

pdf(file = "biplot_cluster_4.pdf", width = 20, height = 20)
fviz_pca_biplot(cl4.pca, repel = T, geom = c("point","text"))
dev.off()

pdf(file = "biplot_cluster_5.pdf", width = 20, height = 20)
fviz_pca_biplot(cl5.pca, repel = T, geom = c("point","text"))
dev.off()

pdf(file = "biplot_cluster_6.pdf", width = 20, height = 20)
fviz_pca_biplot(cl6.pca, repel = T, geom = c("point","text"))
dev.off()

write.csv2(t(cl4.df), "cluster4_pct_biplot.csv")
write.csv2(t(cl5.df), "cluster5_pct_biplot.csv")
write.csv2(t(cl6.df), "cluster6_pct_biplot.csv")

