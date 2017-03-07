seg.df <- read.csv("data/seg.csv")

#### chi quare test for factors
# two-way chi-square
chisq.test(table(seg.df$subscribe, seg.df$ownHome))

# two-way chi-square without correction (matches traditional formula)
chisq.test(table(seg.df$subscribe, seg.df$ownHome), correct=FALSE)

# two-way with simulation to establish p value
chisq.test(table(seg.df$subscribe, seg.df$ownHome), sim=TRUE, B=10000)

#### t-test
t.test(income ~ ownHome, data=seg.df)
t.test(income ~ ownHome, data=subset(seg.df, Segment=="Travelers"))