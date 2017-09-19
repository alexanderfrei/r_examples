seg.df <- read.csv("data/seg.csv")

# recode continuous and counts to be all factors
seg.fac <- seg.df

seg.fac$age <- cut(seg.fac$age, 
                   breaks=c(0,25,35,55,65,100), 
                   labels=c("19-24", "25-34", "35-54", "55-64", "65+"), 
                   right=FALSE, ordered_result=TRUE)
summary(seg.fac$age)

