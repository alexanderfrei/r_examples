subs <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv",
                 sep=",",header=T)
library("ggplot2")

ggplot(subs, aes(1:31,Subscribers)) + 
  geom_point() + 
  ggtitle("Title") +
  scale_y_continuous(limits = c(0,30000),breaks = seq(0,30000,by = 5000), name = "Y") +
  scale_x_continuous(seq(1,31,by=5), name = "X")+
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(linetype = "dotted",color = "black"),
    axis.line.x = element_line(color="black", size = 0.3),
    plot.title = element_text(hjust = 0.0),
    axis.title.x = element_text(hjust = 0.05),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )

