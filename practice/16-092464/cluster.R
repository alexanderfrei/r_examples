setwd('practice/16-092464')
library(plyr) # mapvalues
library(car) # recode
library(dplyr) 
library(cluster)
library(Rtsne)
library(ggplot2) 
library(psych) # describe
library(dbscan)

df <- read.csv("segm.csv", sep=";")

colnames(df)[1] <- "daytime"
df$transport <- NULL

# categorical -> factors
df$daytime <- mapvalues(as.factor(df$daytime), from = c("1", "2", "3", "4"), to = c("night","morning","day", "evening")) 

multi <- grep("(a5_|a51_|a7_).*", colnames(df))
df[multi] <- data.frame(apply(df[multi], 2, as.factor))

df$a6from_1 <- as.factor(car::recode(df$a6from_1, "1='From_home'; 2='From_workplace'; 3:hi='From_other'"))
df$a6to_1 <- as.factor(car::recode(df$a6to_1, "1='To_home'; 2='To_workplace'; 3:hi='To_other'"))
df$a8_1 <- as.factor(car::recode(df$a8_1, "1='Me'; 2='Partner'; 3:hi='Pay_other'"))

# continuos NA to median
df$a91_1.2[is.na(df$a91_1.2)] <- median(df$a91_1.2, na.rm = T)
df$a91_1.2 <- as.factor(car::recode(df$a91_1.2, "0:14='0-14m'; 
                               15:29='15-29m'; 
                               30:hi='30m+'"))

### dist

gower_dist <- daisy(df, metric = "gower")

### silhouette plot

silhouette_plot <- function(dist_matrix, clust_method){
  
  library(cluster)
  
  sil_width <- c(NA)
  for(i in 2:10){
    if (clust_method == "pam"){
      model <- pam(dist_matrix, diss = TRUE, k = i)  
    }
    if (clust_method == "hclust"){
      model <- cutree(hclust(dist_matrix, method="ward.D2"), i)
    }  
    sil_width[i] <- summary(silhouette(model, dist_matrix))$avg.width
  }
  
  # Plot sihouette width
  plot(1:10, sil_width,
       xlab = "Number of clusters",
       ylab = "Silhouette Width")
  lines(1:10, sil_width)
}

silhouette_plot(gower_dist, "hclust")
silhouette_plot(gower_dist, "pam")

### pam

pam.3 <- pam(gower_dist, diss = TRUE, k = 3)
pam.4 <- pam(gower_dist, diss = TRUE, k = 4)
pam.5 <- pam(gower_dist, diss = TRUE, k = 5)
pam.6 <- pam(gower_dist, diss = TRUE, k = 6)

### hclust

hclust.3 <- cutree(hclust(gower_dist, method="ward.D2"), 3)
hclust.4 <- cutree(hclust(gower_dist, method="ward.D2"), 4)
hclust.5 <- cutree(hclust(gower_dist, method="ward.D2"), 5)
hclust.6 <- cutree(hclust(gower_dist, method="ward.D2"), 6)
hclust.7 <- cutree(hclust(gower_dist, method="ward.D2"), 7)


### dbscan

dbscan.fit <- dbscan(gower_dist, eps=0.05, minPts = 10)
dbscan.fit$cluster <- car::recode(dbscan.fit$cluster, "2:hi=0")
table(dbscan.fit$cluster)

### k-means

# kmeans.3 <- kmeans(gower_dist, centers=3, nstart = 10)
# kmeans.4 <- kmeans(gower_dist, centers=4, nstart = 10)
# kmeans.5 <- kmeans(gower_dist, centers=5, nstart = 10)

### t-sne plot
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

plot_tsne <- function(tsne_obj, clusters) {
  tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(clusters))
}

tsne_data <- plot_tsne(tsne_obj, hclust.6)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

### write output 

pam <- cbind(pam.3$clustering, pam.4$clustering, pam.5$clustering)
colnames(pam) <- c("pam.3","pam.4","pam.5")

case_id <- 1:dim(df)[1]
df_clusters <- cbind(case_id, hclust.3, hclust.4, hclust.5, hclust.6, hclust.7, 
                     pam)
write.csv2(df_clusters, "g:/Job/Uber segm/clusters.csv", row.names = F)
