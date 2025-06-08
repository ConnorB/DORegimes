# Function to remove STICs that record for less than some % of the total time
stics_not_na <- function(presAbs, percThresh = 0.5, watershed = NULL) {
  presAbs <- as.data.frame(presAbs)
  # Identify site columns (everything except 'date' and 'watershed')
  site_cols <- setdiff(colnames(presAbs), c("date", "watershed"))
  total_stics <- length(site_cols)
  # Calculate percentage recorded for each STIC
  perc_recorded <- colSums(!is.na(presAbs[site_cols])) / nrow(presAbs)
  # Keep only site columns with at least percThresh% non-NA
  valid_sites <- site_cols[perc_recorded >= percThresh]
  lostSTICs <- setdiff(site_cols, valid_sites)
  # Always keep date and watershed columns if they exist
  keep_cols <- c("date", valid_sites)
  if ("watershed" %in% colnames(presAbs)) {
    keep_cols <- c(keep_cols, "watershed")
  }
  # Subset the dataframe
  presAbs <- presAbs[, keep_cols, drop = FALSE]
  # Generate summary messages using cli
  if (!is.null(watershed)) {
    cli::cli_h3("Watershed: {.val {watershed}}")
  }
  cli::cli_alert_info("Summary: {.val {total_stics}} total STICs, {.val {length(valid_sites)}} retained, {.val {length(lostSTICs)}} removed")
  if (length(lostSTICs) > 0) {
    cli::cli_alert_warning("Removed STICs (with % recorded):")
    for (stic in lostSTICs) {
      cli::cli_li("{.field {stic}} ({.val {round(perc_recorded[stic] * 100, 1)}}%)")
    }
  }
  # Add blank line for better readability
  cli::cli_text("")
  # Return the cleaned data
  return(presAbs)
}

# Function to remove rows before stics were deployed and after stics were pulled
trim_na_rows <- function(df) {
  # Check which rows have all NAs (excluding 'date')
  all_na_rows <- apply(select(df, -date), 1, function(x) all(is.na(x)))
  # If all rows are NA, return an empty tibble (or keep 1 row if needed)
  if (all(all_na_rows)) {
    return(slice(df, 0))  # Returns 0 rows (or `slice(df, 1)` to keep 1 row)
  }
  # Find first and last non-NA row
  first_non_na <- which.min(all_na_rows)  # First FALSE (non-NA)
  last_non_na <- which.max(cumsum(!all_na_rows))  # Last non-NA
  # Slice the tibble to remove leading/trailing NA rows
  df |> 
    slice(first_non_na:last_non_na)
}


# Function to do the gap filling, using the missForest package
# From Rob
# STIC.RFimpute_parallel <- function(p.a, ...) {
#   # Ensure input is a dataframe
#   df <- as.data.frame(p.a, check.names = FALSE)
#   # Identify datetime columns to preserve
#   datetime_cols <- sapply(df, inherits, what = "Date") | sapply(df, inherits, what = "POSIXt")
#   df_datetime <- df[, datetime_cols, drop = FALSE]
#   df_to_impute <- df[, !datetime_cols, drop = FALSE]
#   # Convert presence-absence columns to factors with levels 0 and 1
#   df_to_impute[] <- lapply(df_to_impute, factor, levels = c(0, 1))
#   # Run imputation
#   progressr::with_progress({
#     p <- progressr::progressor(steps = 1)
#     mf <- missForest::missForest(xmis = df_to_impute, parallelize = "forests", ...)
#     p()
#   })
#   # Reattach datetime columns
#   result <- cbind(df_datetime, mf$ximp)
#   # Ensure the column order is consistent with the original data
#   result <- result[, colnames(df)]
#   return(result)
# }

STIC.RFimpute_parallel <- function(p.a, nCores = parallel::detectCores() - 1, ...) {
  # Register parallel backend (works cross-platform)
  cl <- parallel::makeCluster(nCores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))  # Ensure cluster stops
  
  # Ensure input is a dataframe
  df <- as.data.frame(p.a, check.names = FALSE)
  
  # Identify and preserve datetime columns
  datetime_cols <- sapply(df, inherits, what = c("Date", "POSIXt"))
  df_datetime <- df[, datetime_cols, drop = FALSE]
  df_to_impute <- df[, !datetime_cols, drop = FALSE]
  
  # Convert to factors
  df_to_impute[] <- lapply(df_to_impute, factor, levels = c(0, 1))
  
  # Run imputation with progress
  progressr::with_progress({
    p <- progressr::progressor(steps = 1)
    mf <- missForest::missForest(
      xmis = df_to_impute,
      parallelize = "forests",
      ...
    )
    p()
  })
  
  # Recombine results
  result <- cbind(df_datetime, mf$ximp)
  result[, colnames(df)]  # Maintain original column order
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

