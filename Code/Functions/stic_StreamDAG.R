st_snap_points <- function(x, y, namevar, max_dist = 1000) {
  
  # this evaluates the length of the data
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)
  
  # this part: 
  # 1. loops through every piece of data (every point)
  # 2. snaps a point to the nearest line geometries
  # 3. calculates the distance from point to line geometries
  # 4. retains only the shortest distances and generates a point at that intersection
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  # this part converts the data to a dataframe and adds a named column of your choice
  out_xy <- st_coordinates(out) %>% as.data.frame()
  out_xy <- out_xy %>% 
    mutate({{namevar}} := x[[namevar]]) %>% 
    st_as_sf(coords=c("X","Y"), crs=st_crs(x), remove=FALSE)
  
  return(out_xy)
}



load_and_preprocess_stics <- function(data_dir, site_id_col = "siteID", datetime_col = "datetime", wetdry_col = "wetdry") {
  stics_data <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE) |>
    future_map(fread) |>
    bind_rows() |>
    select(siteId = !!sym(site_id_col), datetime = !!sym(datetime_col), wetdry = !!sym(wetdry_col)) |>
    unique() |>
    rename(dateTime = datetime) |>
    group_by(siteId) |>
    complete(dateTime = seq(min(dateTime), max(dateTime), by = "15 min")) |>
    arrange(siteId, dateTime) |>
    mutate(wetdry = zoo::na.locf(wetdry, na.rm = FALSE, maxgap = 8)) |>
    ungroup() |>
    filter(minute(dateTime) %% 15 == 0)
  
  return(stics_data)
}


stics_not_na <- function(presAbs) {
  presAbs <- as.data.frame(presAbs)
  # Identify site columns (everything except 'date')
  site_cols <- setdiff(colnames(presAbs), "date")
  # Keep only site columns with at least 50% non-NA
  valid_sites <- site_cols[colSums(!is.na(presAbs[site_cols])) >= 0.5 * nrow(presAbs)]
  # Figure out what was dropped
  lostSTICs <- setdiff(site_cols, valid_sites)
  # Subset the dataframe
  presAbs <- presAbs[, c("date", valid_sites)]
  # Message for dropped sites
  if (length(lostSTICs) > 0) {
    message(
      "STICs ",
      paste(lostSTICs, collapse = ", "),
      " measured less than 50% of the time and were removed."
    )
  }
  return(presAbs)
}



# Function to do the gap filling, using the missForest package
# From Rob
STIC.RFimpute_parallel <- function(p.a, ...) {
  # Ensure input is a dataframe
  df <- as.data.frame(p.a, check.names = FALSE)
  # Identify datetime columns to preserve
  datetime_cols <- sapply(df, inherits, what = "Date") | sapply(df, inherits, what = "POSIXt")
  df_datetime <- df[, datetime_cols, drop = FALSE]
  df_to_impute <- df[, !datetime_cols, drop = FALSE]
  # Convert presence-absence columns to factors with levels 0 and 1
  df_to_impute[] <- lapply(df_to_impute, factor, levels = c(0, 1))
  # Run imputation
  progressr::with_progress({
    p <- progressr::progressor(steps = 1)
    mf <- missForest::missForest(xmis = df_to_impute, parallelize = "forests", ...)
    p()
  })
  # Reattach datetime columns
  result <- cbind(df_datetime, mf$ximp)
  # Ensure the column order is consistent with the original data
  result <- result[, colnames(df)]
  return(result)
}

calc_arc_prob <- function(network, daily_pres_abs) {
  arc_daily_pres_abs <- arc.pa.from.nodes(network, daily_pres_abs[, -1])
  marg_prob <- colMeans(arc_daily_pres_abs, na.rm = TRUE)
  arc_prob <- cor(arc_daily_pres_abs, use = "pairwise.complete.obs")
  arc_cor_prob <- R.bounds(marg_prob, arc_prob)
  prob <- apply(arc_cor_prob, 2, mean)
  
  return(list(arc_daily_pres_abs = arc_daily_pres_abs, prob = prob))
}

calc_stream_dist <- function(stream_network, coords, tolerance = 1) {
  stream_network |>
    st_combine() |>
    st_distance(coords, along = _) |>
    `rownames<-`(coords$siteId) |>
    `colnames<-`(coords$siteId) |>
    as_tibble(rownames = "From") |>
    pivot_longer(-From, names_to = "To", values_to = "dist_m") |>
    mutate(Arcs = paste(From, "->", To),
           dist_m = as.numeric(dist_m))
}

calc_arc_prob <- function(network, presAbs) {
  arc_pres_abs <- arc.pa.from.nodes(network, presAbs)
  marg_prob <- colMeans(arc_pres_abs, na.rm = TRUE)
  arc_prob <- cor(arc_pres_abs, use = "pairwise.complete.obs")
  arc_cor_prob <- R.bounds(marg_prob, arc_prob)
  prob <- apply(arc_cor_prob, 2, mean)
  
  return(list(arc_pres_abs = arc_pres_abs, marg_prob = marg_prob))
}


# Function to calculate daily metrics:
# upstream_length: sum of edge lengths upstream of the target site.
# wet_nodes: number of nodes in the upstream subcomponent.
# present_nodes: count of nodes (from the network) that are not NA.
# total_nodes: total count of network nodes (even if NA).
calculate_network_metrics <- function(network, lengths_data, pres_abs_data, site_id) {
  # Extract node names from the network
  node_names <- as_tbl_graph(network) |>
    activate(nodes) |>
    as_tibble() |>
    pull(name)
  
  # Build the tidygraph with length attributes
  graph <- as_tbl_graph(network) |>
    activate(edges) |>
    mutate(arc = paste0(node_names[from], " -> ", node_names[to])) |>
    left_join(lengths_data, by = c("arc" = "Arcs"))
  
  # Reshape daily presence data to long format
  daily_long <- pres_abs_data |>
    pivot_longer(-date, names_to = "site", values_to = "present")
  
  # Function to calculate daily metrics
  calculate_day_metrics <- function(day_data, graph_tbl, target_site) {
    # Get network node names from the graph
    nodes_in_graph <- graph_tbl |>
      activate(nodes) |>
      as_tibble() |>
      pull(name)
    
    # Restrict day_data to nodes that are in the network
    day_data_net <- day_data |>
      filter(site %in% nodes_in_graph)
    
    total_nodes <- nrow(day_data_net)
    present_nodes <- sum(!is.na(day_data_net$present))
    
    # Define active nodes as those with present == 1
    active_sites <- day_data_net |>
      filter(present == 1) |>
      pull(site)
    
    # If target site is not active, return NA for upstream metrics
    if (!(target_site %in% active_sites)) {
      return(list(
        upstream_length = -Inf,
        wet_nodes = 0,
        present_nodes = present_nodes,
        total_nodes = total_nodes
      ))
    }
    
    # Create subgraph of active sites
    subgraph <- graph_tbl |>
      activate(nodes) |>
      filter(name %in% active_sites)
    
    subgraph_igraph <- as.igraph(subgraph)
    
    # Identify all nodes upstream of target (including target itself)
    upstream_nodes <- subcomponent(subgraph_igraph, target_site, mode = "in")
    induced_subgraph <- induced_subgraph(subgraph_igraph, upstream_nodes)
    
    wet_nodes <- vcount(induced_subgraph)
    upstream_length <- if (ecount(induced_subgraph) == 0) 0 else sum(E(induced_subgraph)$Lengths, na.rm = TRUE)
    # Convert kilometers to meters
    upstream_length <- upstream_length * 1000
    
    list(
      upstream_length = upstream_length,
      wet_nodes = wet_nodes,
      present_nodes = present_nodes,
      total_nodes = total_nodes
    )
  }
  
  # Calculate metrics for each day
  result <- daily_long |>
    group_by(date) |>
    nest() |>
    mutate(metrics = map(data, ~ calculate_day_metrics(.x, graph, site_id))) |>
    unnest_wider(metrics) |> 
    mutate(percentConnected = (wet_nodes/present_nodes)*100)
  
  return(result)
}


plot_sf_stics <- function(net, x, y, names, shapefile = NULL,  
                          cex = 1, arrow.col = "lightblue", arrow.lwd = 1, 
                          pch = 21, pt.bg = "orange") {
  if (!requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Please install 'ggplot2' and 'ggrepel'")
  }
  
  coords <- data.frame(Object.ID = names, x = x, y = y)
  v_pos <- coords |> tibble::column_to_rownames("Object.ID")
  
  edge_df <- igraph::as_data_frame(net, what = "edges") |>
    mutate(
      x_start = v_pos[from, "x"],
      y_start = v_pos[from, "y"],
      x_end = v_pos[to, "x"],
      y_end = v_pos[to, "y"]
    )
  
  g <- ggplot2::ggplot(shapefile) +
    ggplot2::geom_sf() +
    ggplot2::geom_segment(
      data = edge_df,
      ggplot2::aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
      arrow = grid::arrow(angle = 25, length = unit(0.2, "cm"), type = "closed"),
      color = arrow.col,
      linewidth = arrow.lwd
    ) +
    ggplot2::geom_point(
      data = coords,
      ggplot2::aes(x = x, y = y),
      shape = pch,
      fill = pt.bg,
      size = cex * 1.8
    ) +
    ggrepel::geom_text_repel(
      data = coords,
      ggplot2::aes(x = x, y = y, label = Object.ID),
      colour = "black",
      size = 5,
      box.padding = unit(0.3, "lines"),
      point.padding = unit(0.25, "lines")
    ) +
    ggplot2::theme_void() +
    ggplot2::xlab("") + ggplot2::ylab("")
  
  return(g)
}

snap_points_to_lines <- function(points, lines) {
  # Find the nearest line index for each point
  nearest_line_idx <- st_nearest_feature(points, lines)
  
  # Snap each point to its nearest line
  snapped_points <- vector("list", length = nrow(points))
  
  for (i in seq_len(nrow(points))) {
    pt <- points[i, ]
    nearest_line <- lines[nearest_line_idx[i], ]
    
    # Line from point to its projection on the line
    nearest_line_seg <- st_nearest_points(pt, nearest_line)
    
    # Extract the projected point on the line
    snapped_points[[i]] <- st_cast(nearest_line_seg, "POINT")[2]
  }
  snapped_points <- do.call(rbind, snapped_points)
  # Combine into a single geometry column
  snapped_sfc <- st_sfc(snapped_points, crs = st_crs(points))
  
  # Replace geometry in the input points object
  snapped_sf <- st_set_geometry(points, snapped_sfc)
  
  return(snapped_sf)
}


# Function to extract edges from each network in a list 
get_edge_lists <- function(graph_list) {
  # Use lapply to iterate through each element of the list
  edge_lists <- lapply(graph_list, function(graph) {
    # Extract edges as a data frame
    edges_df <- as_data_frame(graph, what = "edges")
    
    # Format each edge as "From -> To"
    formatted_edges <- paste(edges_df$from, "->", edges_df$to)
    
    return(formatted_edges)
  })
  
  return(edge_lists)
}

