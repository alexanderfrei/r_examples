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


fac <- c("a6from_1", "a6to_1", "a8_1", "trip_duration", "daytime")
dm <- grepl("(a51_|a7_).*",colnames(df))

# dummy(data=df, x="a8_1", sep = ".")

##################################################################################################
### mine rules

trans <- as(df, "transactions")

mine.rules <- function(var, sup, conf=0.5){
  rules <- apriori(trans, 
                   appearance = list(rhs = var, default="lhs"), 
                   parameter = list(support=sup, minlen=4, maxlen=7, confidence=conf))
  rules.sorted <- sort(rules, by="confidence")
  subset.matrix <- is.subset(rules.sorted, rules.sorted)
  subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
  redundant <- colSums(subset.matrix, na.rm=T) >= 1
  rules.pruned <- rules.sorted[!redundant]
  inspect(rules.pruned)
}

land.rules <- as.data.frame(mine.rules("land_transport", 0.05))$lhs[1:10]
metro.rules <- as.data.frame(mine.rules("metro", 0.1))$lhs[1:10]
train.rules <- as.data.frame(mine.rules("train", 0.004))$lhs[1:10]
car.rules <- as.data.frame(mine.rules("personal_car", 0.01))$lhs[1:10]
company.rules <- as.data.frame(mine.rules("company_car", 0.003))$lhs[1:10]
taxi.rules <- as.data.frame(mine.rules("taxi", 0.003))$lhs[1:10]

land.rules

##################################################################################################
### transport pca

prep_pca <- function(transport){
  base <- sum(df[[transport]])
  fac_ <- unlist(apply(df[df[[transport]], fac], 2, function(x) table(x) / base))
  dm_ <-  unlist(apply(df[df[[transport]], dm], 2, function(x) table(x)[2] / base))
  fac_[is.na(fac_)] <- 0
  dm_[is.na(dm_)] <- 0
  c(fac_,dm_)
}

land_transport <- prep_pca("land_transport")
metro <- prep_pca("metro")
train <- prep_pca("train")
personal_car <- prep_pca("personal_car")
company_car <- prep_pca("company_car")
taxi <- prep_pca("taxi")

transport.pca <- rbind(land_transport, metro, train, personal_car, company_car, taxi)
res.pca <- PCA(transport.pca,  graph = FALSE)
fviz_pca_biplot(res.pca, repel = TRUE)
