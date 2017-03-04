library("dplyr")

data <- read.csv("data/correl.csv")

cor_1 <- data %>%
  filter(brands %in% c(15,16,17,18)) %>%
  slice(1:100) %>%
  select(a13_2_1.1:a8_1) %>%
  glimpse()

corr <- as.data.frame(cor(cor_1))  # pearson binary x value
corr$nm <- rownames(corr)
corr <- corr %>% 
  select(a8_1,nm) %>%
  filter(a8_1 < 1)

barplot(corr$a8_1, names.arg = corr$nm, las=2)

# pairs(cor_1)

