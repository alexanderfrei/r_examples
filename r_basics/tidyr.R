library(dplyr)
library(tidyr)

id <- 1:100
x1 <- rnorm(100, mean = 4.5, sd = 0.2)
x2 <- rnorm(100, mean = 4, sd = 0.2)
x3 <- rnorm(100, mean = 3.5, sd = 0.2)
df <- data_frame(id,x1,x2,x3)

# GATHER varstocases spss analog
df_gt <- df %>% gather(x,v,x1:x3,factor_key = T)
levels(df_gt$x)
df_gt

# SPREAD casestovars analog
df_sp <- df_gt %>% spread(x, v)

