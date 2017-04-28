setwd('practice/16-092464')

library(dplyr)
library(dummies)
library(car)

df <- read.csv("segm.csv", sep=';')
colnames(df)[1] <- "interval"

## a6from/to

df$a6from_1 <- recode(df$a6from_1, "NA=0; 3:hi=3")
df$a6to_1 <- recode(df$a6to_1, "NA=0; 3:hi=3")

## a8

df$a8_1 <- recode(df$a8_1, "NA=0; 2:hi=2")

## a9

df$a91_1.2 <- recode(df$a91_1.2, "NA=0; 0:9=1; 10:19=2; 20:39=3; 40:hi=4")

## make dummies

df <- dummy.data.frame(df, names=c("interval","a6from_1", "a6to_1", "a8_1", "a91_1.2"), sep=".")

to_drop <- c("a91_1.2.0", "a6from_1.0", "a6to_1.0", "a7_1.98", "a8_1.0")
df[to_drop] <- NULL

# library(corrplot)    
# library(gplots)      
# corrplot.mixed(corr=cor(df),
#                lower="circle",
#                tl.pos="lt", 
#                tl.col="black",
#                col = colorpanel(5, "blue", "grey", "red"),)

## берем только поездки с 1 видом транспорта для классификации
tr_exist <- 
  df %>%
  select(starts_with("a5_")) %>%
  apply(1,sum) == 1 
  
df <- df[tr_exist,]

df <- 
  df %>%
  select(-starts_with("a5_"))

write.csv(df, "for_rf.csv")

table(df$transport)
