
# NMDS --------------------------------------------------------------------

# # Step 1: Prepare data
ordData <- dailyDat |>
  select(Site, Date,
         # community (DO regime)
         avgDO.pctsat, DOSat.exc, DOpctsat.amp, probHypSat, cvDO.pctsat, #) |>
         # environmental variables
         active_upstream_length, percent_network_wet, a_cent , Q_1d, excProb,
         Depth_1d, PPT_1d, water.temp, DOY) |>
  drop_na()

# Step 2: Separate community and environment matrices
comm <- ordData |>
  select(avgDO.pctsat, DOSat.exc, DOpctsat.amp, probHypSat, cvDO.pctsat)

env <- ordData |>
  select(active_upstream_length, percent_network_wet, a_cent , Q_1d, excProb,
         Depth_1d, PPT_1d, water.temp, DOY)

# Step 3: Run NMDS (2 dimensions)
set.seed(42)
nmds <- metaMDS(comm, distance = "bray", k = 3, try = 50, maxtry = 200, autotransform = TRUE)
stressplot(nmds)
# Step 4: Fit environmental vectors
fit <- envfit(nmds, env, permutations = 999)

# Step 5: Extract NMDS scores and bind metadata
nmds_scores <- scores(nmds, display = "sites") |> as_tibble() |>
  bind_cols(ordData |> select(Site, Date))

# Step 6: Plot NMDS with environmental vectors
nmds_plot <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Site, shape = Site)) +
  geom_point(size = 2, alpha = 0.8) +
  #stat_ellipse(aes(group = Site), linetype = "dashed") +
  scale_color_manual(values = okabeIto) +
  labs(title = "NMDS - DO Regime Structure",
       x = "NMDS1", y = "NMDS2") +
  theme_few() #+
#theme(legend.position = "bottom")

# Add envfit arrows
arrow_df <- as.data.frame(scores(fit, display = "vectors"))
arrow_df$var <- rownames(arrow_df)
# Add envfit arrows
nmds_plot <- nmds_plot +
  geom_segment(
    data = arrow_df,
    aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
    inherit.aes = FALSE,  # CRITICAL: prevents looking for 'Site' in arrow_df
    arrow = arrow(length = unit(0.25, "cm")),
    color = "black"
  ) +
  geom_text_repel(
    data = arrow_df,
    aes(x = NMDS1 * 1.1, y = NMDS2 * 1.1, label = var),
    inherit.aes = FALSE,
    size = 3,
    color = "black"
  )
nmds_plot
nmds_scores <- scores(nmds, display = "sites") |>
  as.data.frame() |>
  bind_cols(ordData |> select(Site, Date))  # Add metadata

#
nmds_nbClust <- NbClust::NbClust(data = nmds_scores |> select(-Site, -Date),
                                 distance='euclidean',
                                 min.nc = 2, max.nc = 9,
                                 method="kmeans",
                                 index = "all")

set.seed(123)
k_clust <- kmeans(comm, centers = 2)  # try 2–6 clusters

nmds_scores$Cluster <- factor(k_clust$cluster)
cluster_colors <- c(
  "1" = "#0072B2",
  "2" = "#009E73",
  "3" = "#E69F00",
  "4" = "#D55E00"
)


ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, shape = Site, color = Cluster)) +
  # Points with custom aesthetics
  geom_point(alpha = 0.5) +
  # Cluster ellipses (using cluster grouping)
  #stat_ellipse(aes(group = Cluster, color = Cluster),linetype = "dashed",linewidth = 0.8, show.legend = FALSE) +
  # Custom color scale (clusters)
  scale_color_manual(values = cluster_colors) +
  # Labels and theme
  labs(
    title = "DO Regime Clusters in NMDS Space", x = "NMDS1", y = "NMDS2") +
  theme_few() +
  theme(
    legend.position = "right",
    legend.box = "vertical"
  ) +
  # Separate legends for color (cluster) and shape (site)
  guides(
    color = guide_legend(order = 1, title = "Cluster"),
    shape = guide_legend(order = 2, title = "Site")
  ) +
  geom_segment(
    data = arrow_df,
    aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.25, "cm")),
    color = "black"
  ) +
  geom_text_repel(
    data = arrow_df,
    aes(x = NMDS1 * 1.1, y = NMDS2 * 1.1, label = var),
    inherit.aes = FALSE,
    size = 3,
    color = "black"
  )

table(nmds_scores$Cluster, nmds_scores$Site)
fviz_nbclust(nmds_scores[, 1:2], kmeans, method = "silhouette")  # optimal k
sil <- silhouette(k_clust$cluster, dist(nmds_scores[, 1:2]))
mean(sil[, "sil_width"])  # closer to 1 is better


# 1. Add cluster assignments to your original data
ordData_with_clusters <- ordData |>
  mutate(Cluster = factor(k_clust$cluster))

# 2. Create the plot
ggplot(ordData_with_clusters, aes(x = Date, y = avgDO.pctsat, color = Cluster)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ Site, scales = "free_x") +
  scale_color_manual(values = cluster_colors) +
  labs(title = "Dissolved Oxygen (% Saturation) by Cluster",
       x = "Date",
       y = "DO (% saturation)",
       color = "Cluster") +
  theme_few() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Reshape data to long format for faceting
metrics <- c("avgDO.pctsat", "DOpctsat.amp", "DOSat.exc", "probHypSat")

plot_data <- ordData_with_clusters |>
  filter(!is.na(Cluster)) |> 
  select(Cluster, all_of(metrics)) |>
  pivot_longer(cols = all_of(metrics), 
               names_to = "Metric", 
               values_to = "Value")

# Create faceted plot
ggplot(plot_data, aes(x = Cluster, y = Value, fill = Cluster)) +
  geom_boxplot() +
  scale_fill_manual(values = cluster_colors) +
  geom_jitter(width = 0.2, alpha = 0.1, size = 0.5) +
  facet_wrap(~ Metric, ncol = 3, scales = "free") +
  labs(title = "DO Metrics by Cluster",
       y = "Value",
       x = "Cluster") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, face = "bold"))

