store.df <- read.csv("data/to_describe.csv")  

boxplot(store.df$p2sales, 
        xlab="Weekly sales", ylab="P2",
        main="Weekly sales of P2, All stores", 
        horizontal=TRUE)

boxplot(store.df$p2sales ~ store.df$storeNum, horizontal=TRUE,
        ylab="Store", xlab="Weekly unit sales", las=1,
        main="Weekly Sales of P2 by Store")

# add axis

boxplot(p2sales ~ p2prom, data=store.df, horizontal=TRUE, yaxt="n", 
        ylab="P2 promoted in store?", xlab="Weekly sales",
        main="Weekly sales of P2 with and without promotion")

axis(side=2, at=c(1,2), labels=c("No", "Yes"))
