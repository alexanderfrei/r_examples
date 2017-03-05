
hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Count",
     breaks=30,             # more columns 
     col="lightblue")       # color the bars

# relabeling the x axis

hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Relative frequency",             # it's no londer the count!
     breaks=30, 
     col="lightblue", 
     freq=FALSE,                            # freq=FALSE means to plot density, not counts
     xaxt="n")                              # xaxt="n" means "x axis tick marks == no"

axis(side=1, at=seq(60, 300, by=20))        # add the x axis (side=1) tick marks we want

# adding curves to the histogram

hist(store.df$p1sales, 
     main="Product 1 Weekly Sales Frequencies, All Stores",
     xlab="Product 1 Sales (Units)",
     ylab="Relative frequency",
     breaks=30, 
     col="lightblue", 
     freq=FALSE,                            # freq=FALSE means to plot density, not counts
     xaxt="n")                              # xaxt="n" means "x axis tick marks == no"

axis(side=1, at=seq(60, 300, by=20))        # add the x axis (side=1) tick marks we want
lines(density(store.df$p1sales, bw=10),    # "bw= ..." adjusts the smoothing
      type="l", col="darkred", lwd=2)      # lwd = line width
