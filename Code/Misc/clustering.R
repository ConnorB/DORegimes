# Load necessary libraries
library(here)
library(tidyverse)
library(cluster)
library(FactoMineR) 
library(factoextra) 
library(widyr)
# Load the data
dometrics <- read.csv("~/Desktop/DOMetrics.csv")

# Select the relevant columns for clustering
do_data <- dometrics %>%
  filter(season %in% c("Spring", "Summer")) %>% 
  select(Site, Date, minDO.pctsat, maxDO.pctsat, avgDO.pctsat, DOpctsat.amp, probHypSat, flowState, WaterYear, season) %>% 
  mutate(
    flowState = as.factor(flowState),
    WaterYear = as.factor(WaterYear),
    season = as.factor(season)
  )

# Remove rows with NA values to avoid errors during clustering
do_data_clean <- do_data %>%
  drop_na()

# Scale the numeric data for clustering
scaled_data <- do_data_clean %>%
  select(minDO.pctsat, maxDO.pctsat,avgDO.pctsat, DOpctsat.amp, probHypSat) %>%
  scale()

# Determine the optimal number of clusters using the Elbow method
set.seed(123)  # Set seed for reproducibility
fviz_nbclust(scaled_data, kmeans, method = "wss") +
  theme_minimal() +
  labs(title = "Elbow Method for Optimal Number of Clusters")

# Determine the optimal number of clusters using the Silhouette method
fviz_nbclust(scaled_data, kmeans, method = "silhouette") +
  theme_minimal() +
  labs(title = "Silhouette Method for Optimal Number of Clusters")

# Determine the optimal number of clusters using the gap method
fviz_nbclust(scaled_data, kmeans, method = "gap_stat") +
  theme_minimal() +
  labs(title = "Gap Stat Method for Optimal Number of Clusters")


# Perform k-means clustering with the optimal number of clusters (e.g., 4)
optimal_clusters <- 3
kmeans_result <- kmeans(scaled_data, centers = optimal_clusters, nstart = 25)

# Add cluster information to the original dataset
do_data_clean <- do_data_clean %>%
  mutate(cluster = as.factor(kmeans_result$cluster))

# Visualize the clusters 
fviz_cluster(kmeans_result, data = scaled_data, geom = "point", ellipse.type = "convex") +
  theme_minimal() +
  labs(title = "Optimized Cluster Analysis of DO Metrics")

# Summarize cluster distribution by Year and flowState
clusterSummary <- do_data_clean %>%
  group_by(cluster, WaterYear, flowState, season, Site) %>%
  summarize(count = n()) %>%
  arrange(cluster, WaterYear, flowState, season, Site) %>%
  print()

# 
# # Use hcut() which compute hclust and cut the tree
# hc.cut <- hcut(scaled_data, k = 3, hc_method = "complete", hc_metric = "euclidean", stand = T)
# hc.cut$labels <- do_data_clean$Site
# # Visualize dendrogram
# t <- fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
# # Visualize cluster
# fviz_cluster(hc.cut, ellipse.type = "convex", geom = "point")
# fviz_contrib
# 
# # Load the data
# doDat <- read.csv(here("Data/allDat.csv"))
# doDat <- doDat %>% 
#   mutate(WaterYear = calcWaterYear(dateTime),
#          season = sapply(dateTime, calcSeason),
#          Year = year(dateTime))
# # Select the relevant columns for clustering
# do_data <- doDat %>%
#   mutate(
#     wetQf = as.factor(wetQf),
#     WaterYear = as.factor(WaterYear),
#     season = as.factor(season)
#   )
# 
# # Remove rows with NA values to avoid errors during clustering
# do_data_clean <- do_data %>%
#   drop_na()
# 
# # Scale the numeric data for clustering
# scaled_data <- do_data_clean %>%
#   select(DO.pctsat) %>%
#   scale()
# 
# # Determine the optimal number of clusters using the Elbow method
# set.seed(123)  # Set seed for reproducibility
# fviz_nbclust(scaled_data, kmeans, method = "wss") +
#   theme_minimal() +
#   labs(title = "Elbow Method for Optimal Number of Clusters")
# 
# # Determine the optimal number of clusters using the Silhouette method
# fviz_nbclust(scaled_data, kmeans, method = "silhouette") +
#   theme_minimal() +
#   labs(title = "Silhouette Method for Optimal Number of Clusters")
# 
# # Perform k-means clustering with the optimal number of clusters (e.g., 4)
# optimal_clusters <- 2
# kmeans_result <- kmeans(scaled_data, centers = optimal_clusters, nstart = 25)
# 
# # Add cluster information to the original dataset
# do_data_clean <- do_data_clean %>%
#   mutate(cluster = as.factor(kmeans_result$cluster))
# 
# # Visualize the clusters using principal component analysis (PCA)
# fviz_cluster(kmeans_result, data = scaled_data, geom = "point", ellipse.type = "convex") +
#   theme_minimal() +
#   labs(title = "Optimized Cluster Analysis of DO Metrics")
# 
# # Investigate how clusters relate to water year, season, and flow state
# # Summarize cluster distribution by Year and flowState
# clusterSummary <- do_data_clean %>%
#   group_by(cluster, WaterYear, flowState, season, Site) %>%
#   summarize(count = n()) %>%
#   arrange(cluster, WaterYear, flowState, season, Site) %>%
#   print()
# 
