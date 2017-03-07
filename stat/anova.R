seg.df <- read.csv("data/seg.csv")

### ANOVA
seg.aov.own <- aov(income ~ ownHome, data=seg.df)
anova(seg.aov.own)
seg.aov.seg <- aov(income ~ Segment, data=seg.df)
anova(seg.aov.seg)

# two-way aov
anova(aov(income ~ Segment + ownHome, data=seg.df))
anova(aov(income ~ Segment * ownHome, data=seg.df))

# compare models (nested)
anova(aov(income ~ Segment, data=seg.df),
      aov(income ~ Segment + ownHome, data=seg.df))

### Visualize ANOVA group means
library(multcomp)
# -1 deleting intercept
seg.aov <- aov(income ~ -1 + Segment, data=seg.df)
glht(seg.aov)
par(mar=c(6,10,2,2))
plot(glht(seg.aov), xlab="Income", main="Average Income by Segment (95% CI)")

### stepwise ANOVA

seg.aov.step <- step(aov(income ~ ., data=seg.df))
anova(seg.aov.step)

#### *Bayesian ANOVA
set.seed(96761)
library(BayesFactor)
seg.bf1 <- lmBF(income ~ Segment, data=seg.df)
seg.bf2 <- lmBF(income ~ Segment + ownHome, data=seg.df)
seg.bf1 / seg.bf2

seg.bf.chain <- posterior(seg.bf1, 1, iterations = 10000)

# plot the trace for posterior draw chain
plot(seg.bf.chain[, 1:6])   
summary(seg.bf.chain)
head(seg.bf.chain)

seg.bf.chain[1:4, 1:5]
seg.bf.chain[1:4, 2:5] + seg.bf.chain[1:4, 1]

seg.bf.chain.total <- seg.bf.chain[, 2:5] + seg.bf.chain[, 1]
seg.bf.ci <- t(apply(seg.bf.chain.total, 2, 
                     quantile, pr=c(0.025, 0.5, 0.975)))
seg.bf.ci

### plot the credible intervals
library(ggplot2)y
seg.bf.df <- data.frame(seg.bf.ci)
seg.bf.df$Segment <- rownames(seg.bf.df)
# basic plot with segment, 50% and 95% limits
p <- ggplot(seg.bf.df, aes(x=Segment, y=X50., ymax=X97.5., ymin=X2.5.))
p <- p + geom_point(size=4) + geom_errorbar(width=0.2) + ylab("Income")
p + ggtitle("95% CI for Mean Income by Segment") + coord_flip()






###################################
VES.BR <- sample(70:90, 30, replace=TRUE)
VES.BL <- sample(69:79, 30, replace=TRUE)
VES.SH <- sample(70:80, 30, replace=TRUE)
ROST.BR <- sample(160:180, 30, replace=TRUE)
ROST.BL <- sample(155:160, 30, replace=TRUE)
ROST.SH <- sample(160:170, 30, replace=TRUE)
data <- data.frame(CVET=rep(c("br", "bl", "sh"), each=30),
                   VES=c(VES.BR, VES.BL, VES.SH),
                   ROST=c(ROST.BR, ROST.BL, ROST.SH))

boxplot(data$ROST ~ data$CVET)

# parametric anova
rost.cvet <- aov(data$ROST ~ data$CVET)
rost.cvet.hsd <- TukeyHSD(rost.cvet)
plot(rost.cvet.hsd) # non-zero confidence intervals are significant
pairwise.t.test(data$ROST, data$CVET)

# non parametric anova
kruskal.test(data$ROST ~ data$CVET)
kruskal.test(data$ROST ~ data$VES)
pairwise.wilcox.test(data$ROST, data$CVET)

