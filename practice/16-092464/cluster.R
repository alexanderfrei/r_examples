# setwd('practice/16-092464')
library(plyr)
library(dplyr)
library(cluster)
library(Rtsne)
library(ggplot2) 
library(psych)
library(car)

df <- read.csv("segm.csv", sep=";")
colnames(df)[1] <- "daytime"
df$transport <- NULL

# categorical -> factors
df$daytime <- mapvalues(as.factor(df$daytime), from = c("1", "2", "3", "4"), to = c("night","morning","day", "evening")) 

multi <- grep("(a5_|a51_|a7_).*", colnames(df))
df[multi] <- data.frame(apply(df[multi], 2, as.factor))

# df$a6from_1 <- mapvalues(as.factor(df$a6from_1),
#                          from = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "98"),
#                          to = c("Home", "Workplace", "Education", "Classes / training", "Gym/fitness", "Extra classes for child", "Kindergarten / school for child", "Metro station", "Hospital", "Other government institutions", "Airport", "Railway station", "Bus terminal", "Grocery store", "Clothing store", "Shopping mall inside MKAD", "Sopping mall outside or on MKAD", "Friends' place", "Relatives' place", "Restaurant / cafe /bar", "Theatre / Circus / Museum", "Club", "Countryside", "Picnic", "Park", "Other city outside Moscow region", "Other city in Moscow region", "Client meeting", "Other"),
#                          )
# 
# df$a6to_1 <- mapvalues(as.factor(df$a6to_1),
#                        from = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "98"),
#                        to = c("Home", "Workplace", "Education", "Classes / training", "Gym/fitness", "Extra classes for child", "Kindergarten / school for child", "Metro station", "Hospital", "Other government institutions", "Airport", "Railway station", "Bus terminal", "Grocery store", "Clothing store", "Shopping mall inside MKAD", "Sopping mall outside or on MKAD", "Friends' place", "Relatives' place", "Restaurant / cafe /bar", "Theatre / Circus / Museum", "Club", "Countryside", "Picnic", "Park", "Other city outside Moscow region", "Other city in Moscow region", "Client meeting", "Other")
#                        )
# 
# df$a8_1 <- mapvalues(as.factor(df$a8_1), 
#                      from = c("1", "2", "3", "4", "5", "6", "7", "8"),
#                      to = c("Me", "Partner", "Child", "Parents", "Relatives", "Friends", "Colleague", "Employer"))

df$a6from_1 <- as.factor(recode(df$a6from_1, "1='From_home'; 2='From_workplace'; 3:hi='From_other'"))
df$a6to_1 <- as.factor(recode(df$a6to_1, "1='To_home'; 2='To_workplace'; 3:hi='To_other'"))
df$a8_1 <- as.factor(recode(df$a8_1, "1='Me'; 2='Partner'; 3:hi='Pay_other'"))

# continuos NA to median
df$a91_1.2[is.na(df$a91_1.2)] <- median(df$a91_1.2, na.rm = T)
df$a91_1.2 <- as.factor(recode(df$a91_1.2, "0:14='0-14m'; 
                               15:29='15-29m'; 
                               30:hi='30m+'"))

# TODO: factor from binary answers
# TODO: try DBSCAN 

# rename cols
# names(df)[names(df) == 'a91_1.2'] <- 'trip_duration'

### dist

gower_dist <- daisy(df, metric = "gower")
gower_dist <- daisy(df, metric = "gower")
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

### hclust
hclust.2 <- cutree(hclust(gower_dist, method="ward.D2"), 2)
hclust.3 <- cutree(hclust(gower_dist, method="ward.D2"), 3)
hclust.4 <- cutree(hclust(gower_dist, method="ward.D2"), 4)
hclust.5 <- cutree(hclust(gower_dist, method="ward.D2"), 5)

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

### output 

cutree(hclust.fit, 4)

