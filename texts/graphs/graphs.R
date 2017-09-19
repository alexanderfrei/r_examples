Sys.setlocale("LC_ALL", "Russian")

# ***

word_graph <- function(data, min_word_freq, min_connection, 
                       name ="example.png", to_remove=NULL,
                       edge_degree = 0,
                       random_seed = 1) {
  
  if (!is.null(to_remove)) {
    `%ni%` <- Negate(`%in%`)
    data = subset(data, select = names(data) %ni% to_remove)
  }
  
  data = as.matrix(t(data))
  conn_matrix = data %*% t(data)
  
  # filter 
  conn_matrix = conn_matrix[diag(conn_matrix) > min_word_freq, 
                            diag(conn_matrix) > min_word_freq]
  conn_matrix = ifelse(conn_matrix >= min_connection, conn_matrix, 0)
  
  # delete nodes with no links 
  
  connected_links = apply(conn_matrix, 1, function(x){sum(x>0)}) > 1
  conn_matrix = conn_matrix[connected_links, connected_links]
  
  # nodes
  
  id = seq(1, dim(conn_matrix)[1]) # id
  label = row.names(conn_matrix) # words
  weight = diag(conn_matrix) # freq of words
  
  nodes = as.data.frame(cbind(id, label, weight, deparse.level = 1),
                        row.names = F)
  
  nodes$weight = as.numeric(as.character(nodes$weight))
  
  # links 
  
  n = dim(conn_matrix)[1]
  from = c()
  to = c()
  weight = c()
  
  for (i in seq(1, n)) {
    for (j in seq(1, n)) {
      if (conn_matrix[i, j] > 0 && i != j) {
        from = c(from, i)
        to = c(to, j)
        weight = c(weight, conn_matrix[i, j])
      }
    }
  }
  
  type = rep('hyperlink', length(weight))
  arrows = rep('', length(weight))
  
  links = as.data.frame(cbind(from, to, type, arrows, weight, deparse.level = 1),
                        row.names = F)
  
  links$weight = as.numeric(as.character(links$weight))
  
  #########
  
  nodes$size = 20 * nodes$weight / max(nodes$weight)
  # links$width = 3 * links$weight / max(links$weight)
  links$width = 0.5
  links$smooth <- FALSE
  
  title = paste("Частота самого популярного слова:", max(diag(conn_matrix)), sep = " ")
  net = visNetwork(nodes, links, height = "1000px", width = "100%", 
                   main = title) %>% 
    visPhysics(enabled=F) %>% 
    visOptions(highlightNearest = list('enabled'= T, 'degree'= edge_degree), autoResize=T) %>%
    visLayout(randomSeed = random_seed)
  saveWidget(net, name)
  
  print(paste("Всего слов:", dim(nodes)[1], sep = " "))
  visNetwork(nodes, links) %>% visPhysics(enabled=F) %>% visLayout(randomSeed = 1) %>% 
    visLayout(randomSeed = random_seed)
  
}

# ?visOptions
# ***

setwd("./texts/graphs")
library(igraph)
library(visNetwork)
library(htmlwidgets)

# ***

df = read.csv("./bag of words/c110.csv", sep = ",", header = T, stringsAsFactors = F)
word_graph(data = df, 
           min_word_freq = 200, 
           min_connection = 150, 
           name = "Most relevant.html", 
           random_seed = 777)

# ***

df = read.csv("./bag of words/c111.csv", sep = ",", header = T, stringsAsFactors = F)
word_graph(data = df, 
           min_word_freq = 80, 
           min_connection = 40, 
           name = "Least relevant.html")

# ***

df = read.csv("./bag of words/c112.csv", sep = ",", header = T, stringsAsFactors = F)
word_graph(data = df, 
           min_word_freq = 100, 
           min_connection = 60, 
           name = "Unbelievable.html")

# ***


df = read.csv("./bag of words/c113.csv", sep = ",", header = T, stringsAsFactors = F)
word_graph(data = df, 
           min_word_freq = 35, 
           min_connection = 20, 
           name = "Unclear.html", random_seed = 78)

# ***

df = read.csv("./bag of words/c115.csv", sep = ",", header = T, stringsAsFactors = F)
word_graph(data = df, 
           min_word_freq = 75, 
           min_connection = 40, 
           name = "New and different.html", random_seed = 1)
