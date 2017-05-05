setwd('practice/16-092464')
path <- "W:/iMQ/Projects/_PROJECTS/Uber/Uber Moscow U&A/4. DATA (coding, de, dp)/Programming/"

########################################################################################################

library(plyr) # mapvalues
library(car) # recode
library(dplyr) 
library(cluster)
library(Rtsne)
library(ggplot2) 
library(psych) # describe
library(dbscan)

df <- read.csv("old_segm.csv", sep=";")
colnames(df)[1] <- "daytime"
df$transport <- NULL
InterviewID <- df$InterviewID
df$InterviewID <- NULL

########################################################################################################

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

# silhouette_plot <- function(dist_matrix, clust_method){
# 
#   library(cluster)
#   
#   sil_width <- c(NA)
#   for(i in 2:10){
#     if (clust_method == "pam"){
#       model <- pam(dist_matrix, diss = TRUE, k = i)  
#     }
#     if (clust_method == "hclust"){
#       model <- cutree(hclust(dist_matrix, method="ward.D2"), i)
#     }  
#     sil_width[i] <- summary(silhouette(model, dist_matrix))$avg.width
#   }
#   
#   # Plot sihouette width
#   plot(1:10, sil_width,
#        xlab = "Number of clusters",
#        ylab = "Silhouette Width")
#   lines(1:10, sil_width)
# }

# silhouette_plot(gower_dist, "hclust")
# silhouette_plot(gower_dist, "pam")

### pam

# pam.4 <- pam(gower_dist, diss = TRUE, k = 4)
# pam.5 <- pam(gower_dist, diss = TRUE, k = 5)
# pam.6 <- pam(gower_dist, diss = TRUE, k = 6)
# pam.7 <- pam(gower_dist, diss = TRUE, k = 7)

### hclust

hclust.3 <- cutree(hclust(gower_dist, method="ward.D2"), 3)
hclust.4 <- cutree(hclust(gower_dist, method="ward.D2"), 4)
hclust.5 <- cutree(hclust(gower_dist, method="ward.D2"), 5)
hclust.6 <- cutree(hclust(gower_dist, method="ward.D2"), 6)
hclust.7 <- cutree(hclust(gower_dist, method="ward.D2"), 7)


### dbscan

dbscan.fit <- dbscan(gower_dist, eps=0.05, minPts = 20 )
dbscan.fit$cluster <- car::recode(dbscan.fit$cluster, "2:hi=0")
table(dbscan.fit$cluster)

### k-means

# df_scaled <- scale(as.data.frame(sapply(df, as.numeric)))
# kmeans.4 <- kmeans(df_scaled, centers=4, nstart = 50)
# kmeans.5 <- kmeans(df_scaled, centers=5, nstart = 50)
# kmeans.6 <- kmeans(df_scaled, centers=6, nstart = 50)
# kmeans.7 <- kmeans(df_scaled, centers=7, nstart = 50)
# ><

### agg by clusters

df <- cbind(df, InterviewID, hclust.3, hclust.4, hclust.5, hclust.6, hclust.7)

get.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

resp_clusters <- 
  df %>%
  group_by(InterviewID) %>%
  summarise(hclust.3=get.mode(hclust.3),
            hclust.4=get.mode(hclust.4),
            hclust.5=get.mode(hclust.5),
            hclust.6=get.mode(hclust.6),
            hclust.7=get.mode(hclust.7))

write.csv2(resp_clusters, paste(path, "resp_clusters.csv", sep=""), row.names = F)

### write output 

# pam <- cbind(pam.3$clustering, pam.4$clustering, pam.5$clustering)
# colnames(pam) <- c("pam.3","pam.4","pam.5")

case_id <- 1:dim(df)[1]
df_clusters <- cbind(case_id, hclust.3, hclust.4, hclust.5, hclust.6, hclust.7)
write.csv2(df_clusters, paste(path, "clusters.csv", sep=""), row.names = F)

### t-sne plot

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

plot_tsne <- function(tsne_obj, clusters) {
  tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(clusters))
}

tsne_data <- plot_tsne(tsne_obj, hclust.5)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))




