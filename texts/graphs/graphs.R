setwd("C:\\R workspace\\r_examples\\texts\\CE\\data")

# ***

library(igraph)
library(visNetwork)
library(htmlwidgets)

# ***

word_graph <- function(data, word_treshold, conn_treshold, 
                       fontsize=1.2, bubblesize=0.01, 
                       png_name="example.png", 
                       to_remove=NULL,
                       plot_size=2000) {
  
  if (!is.null(to_remove)) {
    `%ni%` <- Negate(`%in%`)
    data = subset(data, select = names(data) %ni% to_remove)
  }
  
  bag_of_words = as.matrix(t(data))
  bag_of_words.m2 <- bag_of_words %*% t(bag_of_words)
  
  # filters
  bag_of_words.m2 <- bag_of_words.m2[rowSums(bag_of_words) >= word_treshold, rowSums(bag_of_words) >= word_treshold]
  power = diag(bag_of_words.m2)
  bag_of_words.m2 = ifelse(bag_of_words.m2 >= conn_treshold, bag_of_words.m2, 0)
  diag(bag_of_words.m2) = power 
  
  # remove loops
  bag_of_words.graph <- graph.adjacency(bag_of_words.m2, weighted=TRUE, mode="undirected")
  bag_of_words.graph <- simplify(bag_of_words.graph)
  
  # set labels and degrees of vertices
  V(bag_of_words.graph)$label <- V(bag_of_words.graph)$name
  V(bag_of_words.graph)$degree <- degree(bag_of_words.graph)
  
  # plot layout fruchterman.reingold
  layout1 <- layout.fruchterman.reingold(bag_of_words.graph)
  V(bag_of_words.graph)$label.cex <- fontsize
  plot(bag_of_words.graph, layout=layout1, vertex.size=power * bubblesize,
       vertex.label.color="black", vertex.color = "lightblue")
  title(paste("Минимальная частота слова: ", as.character(word_treshold), 
              "\nЧастота самого популярного слова:", as.character(max(power))))
  
  # save plot 
  V(bag_of_words.graph)$label.cex <- fontsize * 2.2
  png(png_name, plot_size, plot_size)
  plot(bag_of_words.graph, layout=layout1, vertex.size=power * bubblesize,
       vertex.label.color="black", vertex.color = "lightblue")
  title(paste("Минимальная частота слова: ", as.character(word_treshold), 
              "\nЧастота самого популярного слова:", as.character(max(power))),
        cex.main=5, line = -5)
  dev.off()
  
  bag_of_words.m2
  
}

# ***

df = read.csv("./c110.csv", sep = ",", header = T, stringsAsFactors = F)
word_graph(data = df, 
           fontsize = 1.3, 
           word_treshold = 200, 
           conn_treshold = 150, 
           bubblesize = 0.015, 
           png_name = "Most relevant.png", 
           plot_size = 2000,
           to_remove=c("позитива", "радостные", "отвлечься"))

# ***

df = read.csv("./c111.csv", sep = ",", header = T, stringsAsFactors = F)
word_graph(data = df, 
           fontsize = 1.2, 
           word_treshold = 75, 
           conn_treshold = 35, 
           bubblesize = 0.025, 
           png_name = "Least relevant.png",
           plot_size = 1500, 
           to_remove = c("особенным", "удел"))

# ***

df = read.csv("./c112.csv", sep = ",", header = T, stringsAsFactors = F)
word_graph(data = df, 
           fontsize = 1.2, 
           word_treshold = 75, 
           conn_treshold = 40, 
           bubblesize = 0.025, 
           png_name = "Unbelievable.png",
           to_remove = "особенным",
           plot_size = 1600)

# ***


df = read.csv("./c113.csv", sep = ",", header = T, stringsAsFactors = F)
word_graph(data = df, 
           fontsize = 1.2, 
           word_treshold = 40, 
           conn_treshold = 10, 
           bubblesize = 0.05, 
           png_name = "Unclear.png",
           plot_size = 1500, 
           to_remove = "цена")

# ***

df = read.csv("./c115.csv", sep = ",", header = T, stringsAsFactors = F)
word_graph(data = df, 
           fontsize = 1.2, 
           word_treshold = 75, 
           conn_treshold =25, 
           bubblesize = 0.025, 
           png_name = "New and different.png", 
           plot_size = 1500, 
           to_remove = "утро")
в