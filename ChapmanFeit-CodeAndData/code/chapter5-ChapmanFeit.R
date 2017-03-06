seg.df <- read.csv("http://goo.gl/qw303p")

# descriptives

mean(seg.df$income[seg.df$Segment == "Moving up"])
mean(seg.df$income[seg.df$Segment == "Moving up" & 
                   seg.df$subscribe=="subNo"])

by(seg.df$income, seg.df$Segment, mean)
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)

aggregate(seg.df$income, list(seg.df$Segment), mean)

seg.income.mean <- aggregate(seg.df$income, list(seg.df$Segment), mean)
seg.df$segIncome <- seg.income.mean[seg.df$Segment, 2]
library(car)
some(seg.df)

seg.df$Segment
seg.income.mean[seg.df$Segment, ]
seg.income.mean[seg.df$Segment, 2]

# clear that variable
seg.df$segIncome <- NULL


#### formula version

aggregate(income ~ Segment, data=seg.df, mean)

##########
# two-way data aggregation
aggregate(income ~ Segment + ownHome, data=seg.df, mean)
aggregate(income ~ Segment + ownHome + subscribe, data=seg.df, mean)

agg.data <- aggregate(income ~ Segment + ownHome, data=seg.df, mean)
agg.data[2, ]
agg.data[2, 3]

# Count of factor level occurence by factor
with(seg.df, table(Segment, ownHome))
with(seg.df, table(kids, Segment))

# total of variables by factor
xtabs(kids ~ Segment, data=seg.df)

aggregate(kids ~ Segment, data=seg.df, sum)

seg.tab <- with(seg.df, table(kids, Segment))
apply(seg.tab*0:7, 2, sum)
colSums(seg.tab*0:7)



#### visualize counts by group


library(lattice)


# histogram by 1 factor
histogram(~subscribe | Segment, data=seg.df)

# counts instead of proportions, and some visual options
histogram(~subscribe | Segment, data=seg.df, type="count", 
          layout=c(4,1), col=c("burlywood", "darkolivegreen"))


# histogram by 2 factors
histogram(~subscribe | Segment + ownHome, data=seg.df)

# use prop.table to get just positive proportion
prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)

barchart(prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)[2, ], 
          xlab="Subscriber proportion by Segment", col="darkolivegreen")



#### visualize continuous data by group

## bar chart for continuous variable, the "spreadsheet" way to graph it
# aggregate our data
seg.mean <- aggregate(income ~ Segment, data=seg.df, mean)
# plot it
library(lattice)

barchart(income~Segment, data=seg.mean, col="grey")

seg.income.agg <- aggregate(income ~ Segment + ownHome, data=seg.df, mean)
# then plot it
barchart(income ~ Segment, data=seg.income.agg, 
         groups=ownHome, auto.key=TRUE,
         par.settings = simpleTheme(col=c("gray95", "gray50"))   # try rainbow, topo.colors, heat.colors, cm.colors
)


## better = boxplot for continuous variable

# base graphics way to do this

boxplot(income ~ Segment, data=seg.df, yaxt="n", ylab="Income ($k)")
ax.seq <- seq(from=0, to=120000, by=20000)
axis(side=2, at=ax.seq, labels=paste(ax.seq/1000, "k", sep=""), las=1)


# lattice gives more options, especially for multiway breakouts ("conditioning")
library(lattice)

bwplot(Segment ~ income, data=seg.df, horizontal=TRUE, xlab = "Income")

# add conditioning variable
bwplot(Segment ~ income | ownHome, data=seg.df, horizontal=TRUE, 
       xlab="Income")

