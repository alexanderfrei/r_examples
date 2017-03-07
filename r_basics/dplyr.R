seg.df <- read.csv("data/seg.csv")
library(dplyr)

# FILTER rows by condition
# &=, |or !not %in% operators
seg.df %>% filter(Segment=="Moving up", subscribe=="subNo") %>% head

# SUMMARISE
seg.df %>% summarise(mean = mean(income), sum = sum(income))
summarise_at(seg.df, vars(income,age), funs(n(), mean, median))
# custom fun with .
summarise_at(seg.df, vars(income,age),
             funs(n(), missing = sum(is.na(.)), mean(., na.rm = TRUE), median(.,na.rm = TRUE)))

summarise_all(seg.df["gender"], funs(nlevels(.), sum_na=sum(is.na(.))))

# SLICE rows by number
slice(seg.df, 1:100)

# GROUP_BY
grouped <- seg.df %>% 
  group_by(ownHome, Segment) %>% 
  summarise(income = mean(income))

# SELECT columns
# : for range subset
seg.df %>% select(gender:kids)
# name search helpers:
# ends_with() = Select columns that start with a character string
# ends_with() = Select columns that end with a character string
# contains() = Select columns that contain a character string
# matches() = Select columns that match a regular expression
seg.df %>%   select(starts_with("gen")) %>%   head(2)
seg.df %>%   select(ends_with("ome")) %>%   head(2)
seg.df %>%   select(contains("e")) %>%   head(2)
seg.df %>%   select(matches("e$")) %>%   head(2)

# reorder columns with everything()
select(seg.df, gender, age, kids, everything())

#RENAME column
seg.df %>% rename(sex=gender) %>% head

#ARRANGE sort
seg.df %>% arrange(desc(income),ownHome) %>% head

#DISTINCT delete doubles
seg.df %>% select(gender) %>% distinct
seg.df %>% distinct(gender,subscribe,Segment) 

#MUTATE add column
seg.df %>% mutate(home_travel = ifelse(
  ownHome=="ownYes" & Segment=="Travelers",1,0)) %>% 
  filter(Segment=="Travelers")

#JOIN
# left_join(x, y, by = NULL)
# semi_join(x, y, by = NULL)
# anti_join(x, y, by = NULL)

id <- 1:5
x <- seq(12,20,2)
y <- seq(120,200,20)
df1 <- data_frame(id,x)
df2 <- data_frame(id,y)
inner_join(df1,df2, by="id")

#SAMPLE_FRAQ SAMPLE_N random sampling
sample_frac(seg.df, 0.01)
sample_n(seg.df, 20)

