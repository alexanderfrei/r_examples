library(dplyr)
library(ggplot2)

prob=out[,c(2:16)]

lc <- read.csv("data/cho15_15_groups_individual_utilities.csv", sep=";")
hb <- read.csv("data/cho_utilities.csv", sep=";")

colnames(lc)[1] <- "Id"
id <- lc[1]
# head(cbind(hb[1],id))

hb_uti <- select(hb, starts_with("Level")) 
prob_hb <- exp(hb_uti)
prob_hb <- 100 * prob_hb / rowSums(prob_hb)

hb_min <- apply(prob_hb, 1, min)
hb_max <- apply(prob_hb, 1, max)


lc_uti <- select(lc, starts_with("Level")) 
prob_lc <- exp(lc_uti)
prob_lc <- 100 * prob_lc / rowSums(prob_lc)

lc_min <- apply(prob_lc, 1, min)
lc_max <- apply(prob_lc, 1, max)

max_df <- cbind(id, lc_max, hb_max, lc_min, hb_min)

qplot(hb_max,geom="density")
qplot(lc_max,geom="density")

max_df %>%
  arrange(desc(hb_max))


library(car)

lambda_range <- seq(0.1, 0.9, 0.1)
err <- vector("list", length(lambda_range)) 
i  <- 1
for (l in lambda_range){
  pred <- yjPower(max_df$hb_max, lambda=l)
  err[[i]] <- sum(sqrt((pred - max_df[,'lc_max']) ^ 2))
  i <- i + 1
}
err
lambda_range[8]

max_hb_transformed <- bcPower(max_df$hb_max, lambda=lambda_range[8])
min_hb_transformed <- bcPower(max_df$hb_min, lambda=lambda_range[8])
df_transformed <- cbind(max_df, max_hb_transformed, min_hb_transformed)
df_transformed

hb_uti_transformed <- apply(prob_hb, 2, function(x) basicPower(x, 0.9))
qplot(apply(hb_uti_transformed, 1, max), geom="density")

max(apply(hb_uti_transformed, 1, max))

plot(max_df$hb_max)
max_df[max_df$hb_max>70,]


cbind(arrange(max_df, desc(hb_max))$hb_max, arrange(max_df$lc_max, desc(lc_max))$lc_max)

cbind(arrange(max_df, desc(hb_max))$hb_max, arrange(max_df, desc(lc_max))$lc_max)



