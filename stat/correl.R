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

cust.df <- read.csv("data/cust.csv")

# corrplot

# library(corrplot)
# corrplot.mixed(corr=cor(cust.df[ , c(2, 3, 5:12)], use="complete.obs"),
#                diag = "n",
#                tl.pos="lt", 
#                tl.col="black",
#                col = colorpanel(5, "blue", "grey", "red"),)

# Polychoric correlations
resp <- !is.na(cust.df$sat.service)
cor(cust.df$sat.service[resp], cust.df$sat.selection[resp]) 

library(psych)
polychoric(cbind(cust.df$sat.service[resp], 
                 cust.df$sat.selection[resp]))

