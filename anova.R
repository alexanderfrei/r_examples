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

