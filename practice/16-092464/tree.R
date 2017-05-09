setwd('practice/16-092464')

library(rpart)
library(rpart.plot)
library(car)

df <- read.csv("segm.csv", sep=";")
colnames(df)[1] <- "daytime"

df$transport <- ifelse(is.na(df$transport), 0, df$transport)
df$transport <- as.factor(recode(df$transport, "0='Several types'; c(1,2,3)='Land transport'; 4='Metro'; 6='Personal car';
             c(5,7,8,10)='Other'; 9='Taxi'"))

# удалим класс прочего транспорта, потому что дерево на верхних уровнях его не видит
df <- df[df$transport != 'Other', ]
df$transport <- droplevels(df$transport)

# имена для дамми переменных
df$a51.By_myself <- df$a51_1.1
df$a7.Other_types_of transports were unavailable <- df$a7_1.10
df$a7.I_was_drunk <- df$a7_1.24
df$a7.Departure_spot_was_near_me <- df$a7_1.5
df$a7.I_wanted_to_relax <- df$a7_1.21
df$a7.I_did_not_want_to_walk <- df$a7_1.25
df[c("a51_1.1","a7_1.10","a7_1.24","a7_1.5","a7_1.25","a7_1.21")] <- NULL

dummy <- colnames(df[grepl("(a51|a7).*",colnames(df))])
for (qt in dummy){
  levels(df[[qt]]) <-  c("No","Yes")
}

df$a6from_1 <- as.factor(recode(df$a6from_1, "c('Other','Other city')='Other'"))
df$a6to_1 <- as.factor(recode(df$a6to_1, "c('Other','Other city')='Other'"))

formula <- paste("transport ~ daytime + trip_duration + a6from_1 + a6to_1 + a8_1",
                 paste(dummy, collapse = " + "),
                 sep = " + ")
fit <- rpart(formula, 
             method="class", data=df,
             control=rpart.control(cp=0.004))

# plotcp(fit) # visualize cross-validation results 
# summary(fit) # detailed summary of splits

# pdf 
pdf(file = "tree.pdf", width = 22, height = 18)
rpart.plot(fit, 
           extra=104, 
           box.palette="GnBu",
           branch.lty=3, nn=TRUE, cex=1)
dev.off()

