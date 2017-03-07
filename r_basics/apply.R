store.df <- read.csv("data/to_describe.csv")

# apply()
apply(store.df[, 2:9], MARGIN=2, FUN=mean)
apply(store.df[, 2:9], 1, mean)
apply(store.df[, 2:9], 2, function(x) { mean(x) - median(x) } )

## creating a summary data frame using apply()
mysummary2.df <- data.frame(matrix(NA, nrow=2, ncol=2))
names(mysummary2.df) <- c("Median Sales", "IQR")
rownames(mysummary2.df) <- names(store.df)[4:5] # names from the data frame
mysummary2.df[, "Median Sales"] <- apply(store.df[, 4:5], 2, median)
mysummary2.df[, "IQR"]          <- apply(store.df[, 4:5], 2, IQR)
mysummary2.df

#### get all numeric columns
store.df[sapply(store.df,is.numeric)]

