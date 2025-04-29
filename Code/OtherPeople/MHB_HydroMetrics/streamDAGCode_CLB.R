ptSTICs <- sePT |> 
  rename(SuperSensor = wetdry_SW) |> 
  mutate(siteId = case_when(Site == "PRF" ~ "PRM01",
                            Site == "TAL" ~ "TLM01",
                            Site == "WHR" ~ "WHM01"),
         SuperSensor = case_when(
           SuperSensor %in% c("dry", "0") ~ 0,
           SuperSensor %in% c("wet", "1") ~ 1,
           TRUE ~ NA_real_)) |> 
  select(dateTime, siteId, SuperSensor)




stics <- list.files(here("Data/SE/STICs"), full.names = T) |> 
  lapply(\(file) {
    read_csv(file, col_types = cols(.default = col_character()))
  }) |> 
  bind_rows() |> 
  select(-project, -rType, -SN, -rep) |> 
  rename(dateTime = datetime) |> 
  filter(siteId %in% seSites$siteId & qual_rating != "poor") |> 
  mutate(dateTime = ymd_hms(dateTime),
         wetdry = case_when(
           wetdry %in% c("dry", "0") ~ 0,
           wetdry %in% c("wet", "1") ~ 1,
           TRUE ~ NA_real_))


stics <- stics |> 
  full_join(ptSTICs, by = c("siteId", "dateTime")) |> 
  mutate(wetdry = coalesce(wetdry, SuperSensor)) |> 
  select(-SuperSensor) |> 
  group_by(siteId) |> 
  complete(dateTime = seq(floor_date(min(dateTime), "15 min"), floor_date(max(dateTime), "15 min"), by = "15 min")) |> 
  arrange(dateTime) |> 
  mutate(Site = case_when(
    startsWith(siteId, "W") ~ "WHR",
    startsWith(siteId, "P") ~ "PRF",
    startsWith(siteId, "T") ~ "TAL",
    TRUE ~ NA_character_  
  )) |> 
  ungroup()

dailyPercWet <- stics |>
  mutate(Date = as.Date(dateTime)) |> 
  group_by(Site, Date) |>                                
  summarize(
    PercWet_1d = sum(wetdry == 1, na.rm = TRUE) / sum(!is.na(wetdry)),
    n_stics = n_distinct(siteId[!is.na(wetdry)]),
    tot_stics = n_distinct(siteId),  # ← counted from all sites present on that day in this watershed
    pct_coverage = n_stics / tot_stics,
    .groups = "drop"
  ) |> 
  ungroup() |> 
  group_by(Site) |> 
  mutate(dPercWet_1d = PercWet_1d - lag(PercWet_1d, default = NA)) |> 
  ungroup()


dailyPercWet |>
  ggplot(aes(Date, PercWet_1d)) +
  geom_point() +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~Site, scales = "free", ncol = 1) 

dailyPercWet |>
  ggplot(aes(Date, dPercWet_1d)) +
  geom_point() +
  theme_few() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~Site, scales = "free", ncol = 1)


########## StreamDAG ##########
# Daily PresAbs
source(here("Code/AIMS_DAGs.R"))

daily_presAbs <- stics |>
  mutate(
    date = as_date(dateTime)
  ) |>
  filter(!is.na(Site)) |>
  group_by(Site, siteId, date) |>
  summarise(wetdry = get_mode(wetdry), .groups = "drop") |>
  split(~Site, drop = TRUE) |> 
  lapply(\(df) {
    df |>
      select(date, siteId, wetdry) |>
      pivot_wider(
        names_from = siteId,
        values_from = wetdry
      )
  })

daily_presAbs_clean <- map(daily_presAbs, stics_not_na)
doParallel::registerDoParallel(cores = nCores)
daily_presAbs_filled <- map(daily_presAbs_clean, STIC.RFimpute_parallel)

# Trim and rebuild aimsNets based on daily_presAbs_filled
aimsNets <- imap(aimsNets, \(net, wshed) {
  keep_nodes <- names(daily_presAbs_clean[[wshed]])[-1]
  reconnect_and_remove_nodes(net, keep_nodes)
})

##### Global Weighted ####
# Function to compute arc lengths for a single watershed
get_watershed_lengths <- function(watershed_name, sites, streams) {
  # Subset data
  ws_sites <- sites |> filter(watershed == watershed_name)
  ws_streams <- streams |> filter(watershed == watershed_name)
  
  # Skip if fewer than 2 sites or no streams
  if (nrow(ws_sites) < 2 || nrow(ws_streams) < 1) return(NULL)
  
  # Build network
  net <- as_sfnetwork(ws_streams, directed = TRUE) |>
    st_network_blend(ws_sites, tolerance = 4) |>
    activate("edges") |> 
    mutate(weight = edge_length())
  
  # Get cost matrix
  cost_mat <- net |>
    st_network_cost(from = ws_sites, to = ws_sites, direction = "out", weights = "weight") |> 
    units::drop_units()
  
  rownames(cost_mat) <- ws_sites$siteId
  colnames(cost_mat) <- ws_sites$siteId
  
  # Format into long tibble
  as.data.frame(as.table(cost_mat)) |>
    rename(from = Var1, to = Var2, Length = Freq) |>
    filter(is.finite(Length), Length > 0) |>
    mutate(
      Arc = paste(from, to, sep = " -> "),
      Length = as.numeric(Length)
    ) |>
    select(Arc, Length) |>
    as_tibble()
}

# Get list of unique watersheds
watersheds <- intersect(unique(seSites$watershed), unique(seStreams$watershed))

# Apply function across watersheds
all_arc_lengths <- watersheds |> 
  set_names() |>  # Name list by watershed
  map(\(w) get_watershed_lengths(w, seSites, seStreams)) |> 
  compact()  # Drop NULLs

# Apply the function to the aimsNets list
edge_lists <- get_edge_lists(aimsNets)

# PRF
plot_meta <- seSites |>
  filter(siteId %in% V(aimsNets$PRF)$name)
tmpEdge <- edge_lists$PRF
tmpArc <- all_arc_lengths$PRF |> 
  filter(Arc %in% tmpEdge)
tmpArc <- tmpArc |> 
  mutate(color = viridis::viridis(nrow(tmpArc))[rank(Length)])
plot_sf_stics(aimsNets$PRF, plot_meta$lon, plot_meta$lat, plot_meta$siteId, shapefile = plot_meta, arrow.lwd = 0.7, arrow.col = tmpArc$color)

# WHR
tmpEdge <- edge_lists$WHR
tmpArc <- all_arc_lengths$WHR |> 
  filter(Arc %in% tmpEdge)
tmpArc <- tmpArc |> 
  mutate(color = viridis::viridis(nrow(tmpArc))[rank(Length)])
plot_meta <- seSites |>
  filter(siteId %in% V(aimsNets$WHR)$name)
plot_sf_stics(aimsNets$WHR, plot_meta$lon, plot_meta$lat, plot_meta$siteId, shapefile = plot_meta, arrow.lwd = 0.7, arrow.col = tmpArc$color)

# TAL
tmpEdge <- edge_lists$TAL
tmpArc <- all_arc_lengths$TAL |> 
  filter(Arc %in% tmpEdge)
tmpArc <- tmpArc |> 
  mutate(color = viridis::viridis(nrow(tmpArc))[rank(Length)])
plot_meta <- seSites |>
  filter(siteId %in% V(aimsNets$TAL)$name)
plot_sf_stics(aimsNets$TAL, plot_meta$lon, plot_meta$lat, plot_meta$siteId, shapefile = plot_meta, arrow.lwd = 0.7, arrow.col = tmpArc$color)



##### Calc Global Weighted Metrics ####
# Define the metrics names
metric_names <- c(
  icsl = "ICSL",
  intact_to_sink = "IntactStreamLength",
  a_cent = "AlphaCentrality",
  harary = "Harary"
)

# Set up parallel processing
plan(multisession)

# Compute metrics per watershed
gWeighted <- imap(daily_presAbs_clean, \(df, wshed) {
  
  net <- aimsNets[[wshed]]
  sink <- sink_nodes[[wshed]]
  arc_lengths <- all_arc_lengths[[wshed]]
  
  # Prepare lookup for edge weights
  arc_lengths <- arc_lengths |>
    mutate(Arc_key = gsub(" -> ", "|", Arc))
  
  # Compute metrics for each day
  metric_df <- future_map(1:nrow(df), \(i) {
    temp_graph <- delete.nodes.pa(net, df[i, -1], na.response = "treat.as.1")
    
    edge_keys <- attributes(E(temp_graph))$vname
    length_sub <- arc_lengths$Length[match(edge_keys, arc_lengths$Arc_key)]
    
    E(temp_graph)$weights <- length_sub
    
    tibble(
      dateTime = df[[1]][i],
      icsl = ICSL(temp_graph, lengths = length_sub),
      intact_to_sink = size.intact.to.sink(temp_graph, sink = sink),
      a_cent = mean(alpha_centrality(temp_graph), na.rm = TRUE),
      harary = harary(temp_graph)
    )
  }, .options = furrr_options(seed = TRUE), .progress = TRUE) |> bind_rows()
  
  # Pivot to long and recode
  metric_df |>
    pivot_longer(cols = -dateTime, names_to = "metric", values_to = "value") |>
    mutate(metric = recode(metric, !!!metric_names)) |> 
    pivot_wider(names_from = metric, values_from = value) |> 
    mutate(Site = wshed)
})

plan(sequential)
