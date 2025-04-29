aims_read <- function(input_dir = NULL, sublocation = NULL, rType = NULL, siteId = NULL, numCores = parallel::detectCores() - 1, ...) {
  # If rType is "EXOS", handle EXOS-specific data
  if (!is.null(rType) && rType == "EXOS") {
    if (is.null(siteId)) {
      stop("For EXOS data, 'siteId' must be specified.")
    }
    
    # Generate all file paths for all sites
    all_files <- unlist(lapply(siteId, function(site) {
      list.files(file.path(input_dir, site), 
                pattern = paste0("^", site, "_\\d{4}-\\d{2}_QAQCed\\.csv$"),
                full.names = TRUE)
    }))
    
    if (length(all_files) == 0) {
      warning("No matching EXOS files found for the specified sites.")
      return(NULL)
    }
    
    # Extract site names from file paths (they're in the filename)
    site_names <- sapply(strsplit(basename(all_files), "_"), function(x) x[1])
    
    # Use parallel processing to read all files
    cl <- parallel::makeCluster(numCores)
    parallel::clusterEvalQ(cl, {
      library(data.table)
    })
    parallel::clusterExport(cl, varlist = c("all_files", "site_names"), envir = environment())
    
    dataList <- parallel::parLapply(cl, seq_along(all_files), function(i) {
      tryCatch({
        data <- data.table::fread(all_files[i], fill = T)
        data[, Site := site_names[i]] # Add site name column
        return(data)
      }, error = function(e) {
        warning(paste("Error reading file:", all_files[i], "-", e$message))
        return(NULL)
      })
    })
    
    parallel::stopCluster(cl)
    
    # Filter out NULL values from the list
    dataList <- Filter(Negate(is.null), dataList)
    if (length(dataList) == 0) {
      warning("No valid data read for any site.")
      return(NULL)
    }
    
    # Combine all data.tables into one
    combined_data <- data.table::rbindlist(dataList, use.names = TRUE, fill = TRUE)
    return(combined_data)
  }
  
  # Handle general AIMS data (original code with improved pattern matching)
  # Get list of CSV files that match the expected pattern
  all_files <- list.files(path = input_dir, pattern = ".*_GP_.*\\.csv$", full.names = TRUE)
  
  # Apply sublocation filter if specified - modified to match _SW_ pattern
  if (!is.null(sublocation)) {
    all_files <- all_files[grepl(paste0("_", sublocation, "_"), basename(all_files))]
  }
  
  # Apply rType filter if specified
  if (!is.null(rType)) {
    all_files <- all_files[grepl(paste0("^", rType, "_"), basename(all_files))]
  }
  
  # Apply siteId filter if specified
  if (!is.null(siteId)) {
    all_files <- all_files[grepl(paste0("_", siteId, "_"), basename(all_files))]
  }
  
  # Check if there are any matching files
  if (length(all_files) == 0) {
    stop("No matching files found for the specified site, sublocation, and rType.")
  }
  
  # Read all matching files and combine into a single dataframe
  data <- data.table::rbindlist(
    lapply(all_files, \(f) data.table::fread(f, fill = TRUE)),  # <- fill is now correctly applied
    use.names = TRUE,  # <- ensures columns align by name
    fill = TRUE        # <- handles missing columns across files
  )
  return(data)
}
aims_write <- function(df, output_dir = ".", upload = TRUE) {
  # Ensure the 'datetime' column exists in the dataframe only if rType is DISC or PRES
  if (!("rType" %in% names(df))) {
    stop("Data frame must contain an 'rType' column")
  }
  
  if (any(df$rType %in% c("DISC", "PRES")) && !("datetime" %in% names(df))) {
    stop("Data frame must contain a 'datetime' column for DISC and PRES rTypes")
  }
  
  # Extract year from 'datetime' column if needed
  if ("datetime" %in% colnames(df)) {
    df <- df |> 
      dplyr::mutate(
        year = dplyr::if_else(
          rType %in% c("DISC", "PRES") & !is.na(datetime), 
          as.character(lubridate::year(datetime)), 
          "ALL"
        )
      )
  } else {
    df <- df |> dplyr::mutate(year = "ALL")
  }
  
  
  
  # Determine grouping based on rType
  df <- df |> 
    dplyr::mutate(
      year_group = ifelse(rType %in% c("DISC", "PRES"), as.character(year), "ALL")
    )
  
  df_list <- df |> dplyr::group_split(siteId, sublocation, watershed, year_group)
  
  
  # Iterate over the split data
  for (subset_df in df_list) {
    # Extract first row to get naming elements
    site_id <- unique(subset_df$siteId)
    sublocation <- unique(subset_df$sublocation)
    rType <- unique(subset_df$rType)
    watershed <- unique(subset_df$watershed)
    year <- unique(subset_df$year)
    
    # Drop the 'watershed' column from the data before saving
    subset_df$watershed <- NULL
    subset_df$year <- NULL
    subset_df$year_group <- NULL
    
    # Construct filename, excluding year if not applicable
    if (!is.na(year) && year != "ALL") {
      filename <- sprintf("%s_GP_%s_%s_%s_%s.csv", rType, watershed, site_id, sublocation, year)
    } else {
      filename <- sprintf("%s_GP_%s_%s_%s.csv", rType, watershed, site_id, sublocation)
    }
    
    # Write to CSV
    data.table::fwrite(subset_df, file = file.path(output_dir, filename))
    
    # Should file be uploaded to AIMS drive?
    if (upload == TRUE) {
      # Folder IDs based on rType and siteId
      folder_id <- dplyr::case_when(
        rType == "PRES" & site_id == "SFM01" ~ "1_2JsWvKMWGd2z90oSoVtbx9-lGs3WvV_",
        rType == "PRES" & site_id == "ENM01" ~ "1hQyQ23KnT3lktCS2vBPn9IyHbyC_6Eki",
        rType == "PRES" & site_id == "SHM01" ~ "1oTeBPif77e1A_zoTO3T6HCEuHSruFrci",
        rType %in% c("DISC", "RTCV") & site_id == "SFM01" ~ "1_p422H9-2YM8JlqcQ-Q78KcLnnFDpyi6",
        rType %in% c("DISC", "RTCV") & site_id == "ENM01" ~ "1dmtmM_fi4dnI-kFlDL6X84CCL71dc6zA",
        rType %in% c("DISC", "RTCV") & site_id == "SHM01" ~ "1jn5tYQQ09QBPsy2nPOA6a7I_b9JRVnpw",
        TRUE ~ NA_character_
      )
      
      if (!is.na(folder_id)) {
        googledrive::drive_upload(media = file.path(output_dir, filename), 
                                  overwrite = TRUE, 
                                  path = googledrive::as_id(folder_id))
      }
    }
  }
}


aims_format <- function(data, rType) {
  # Ensure that datetime_UTC exists before using it
  if (!"datetime_UTC" %in% colnames(data)) {
    stop("Error: The column 'datetime_UTC' is required but missing.")
  }
  
  # Compute min/max dates only if datetime_UTC exists
  data <- data |> 
    mutate(
      project = "AIMS",
      rType = rType,
      min_date = format(min(datetime_UTC, na.rm = TRUE), "%Y%m%d"),
      max_date = format(max(datetime_UTC, na.rm = TRUE), "%Y%m%d"),
      rep = paste(min_date, max_date, sep = "-")
    )
  
  # Select only columns that exist in the original dataset
  selected_cols <- c("project", "watershed", "siteId", "datetime_UTC", "rType", "rep", "tempC", "sensorPressure_kPa", 
                     "waterHeight_m", "waterLevel_m", "waterElevation_m", "waterDepth_m", 
                     "Q_Ls_upper", "Q_Ls", "Q_Ls_lower", "wetdry", "QAQC")
  
  # Keep only columns that exist in `data`
  existing_cols <- intersect(selected_cols, colnames(data))
  
  # Rename datetime_UTC to datetime if it exists
  if ("datetime_UTC" %in% existing_cols) {
    data <- data |> rename(datetime = datetime_UTC)
    existing_cols[existing_cols == "datetime_UTC"] <- "datetime"
  }
  
  # Remove waterDepth_m if rType is DISC
  if (rType == "DISC" && "waterDepth_m" %in% existing_cols) {
    existing_cols <- setdiff(existing_cols, "waterDepth_m")
  }
  
  return(data |> select(all_of(existing_cols)))
}


aims_qaqc <- function(data, window = 5) {
  # modified from 3_QAQC_allsites_v1.R script by Delaney
  #  - changed variable names to the ones I am using
  #  - altered rollmean and rollstd to exclude central point in averaging
  #  - made it so that flags do not overwrite each other
  
  # Helper functions to compute rolling mean and standard deviation
  sam_mean <- function(x) {
    if (length(x) %% 2 == 0L) { return(mean(x, na.rm = TRUE)) }
    if (length(x) %% 2 == 1L) { return(mean(x[-ceiling(0.5 * length(x))], na.rm = TRUE)) }
  }
  
  sam_sd <- function(x) {
    if (length(x) %% 2 == 0L) { return(sd(x, na.rm = TRUE)) }
    if (length(x) %% 2 == 1L) { return(sd(x[-ceiling(0.5 * length(x))], na.rm = TRUE)) }
  }
  
  # Initialize pasteFlags column
  data <- data |> mutate(pasteFlags = "")
  
  # Process `waterHeight_m` if it exists
  if ("waterHeight_m" %in% colnames(data)) {
    data <- data |> 
      mutate(
        avg = rollapply(waterHeight_m, width = window, FUN = sam_mean, fill = NA, align = "center"),
        sd = rollapply(waterHeight_m, width = window, FUN = sam_sd, fill = NA, align = "center"),
        rollz = (waterHeight_m - avg) / sd,
        flagZ = ifelse(is.finite(rollz) & (abs(rollz) > 3), "Z", ""),
        flagD = ifelse(waterHeight_m < 0.02, "D", ""),
        flagE = ifelse(waterHeight_m < -5, "E", ""),
        pasteFlags = paste0(flagZ, flagD, flagE)
      )
  }
  
  # Process `tempC` if it exists and `waterHeight_m` exists
  if (all(c("tempC", "waterHeight_m") %in% colnames(data))) {
    data <- data |> 
      mutate(
        flagT = ifelse(tempC < 0 & waterHeight_m > 0.25, "T", ""),
        pasteFlags = paste0(pasteFlags, flagT)
      )
  }
  
  # Process `Q_Ls` (discharge) if it exists
  if ("Q_Ls" %in% colnames(data)) {
    data <- data |> 
      mutate(
        Q_Ls = ifelse(Q_Ls < 0, 0, Q_Ls),  # Set negative discharge to 0
        avgQ = rollapply(Q_Ls, width = window, FUN = sam_mean, fill = NA, align = "center"),
        sdQ = rollapply(Q_Ls, width = window, FUN = sam_sd, fill = NA, align = "center"),
        rollzQ = (Q_Ls - avgQ) / sdQ,
        flagZ = ifelse(is.finite(rollzQ) & (abs(rollzQ) > 3), "Z", ""),
        flagQ = ifelse(is.finite(Q_Ls) & Q_Ls > maxQ * 1.5, "Q", ""),
        pasteFlags = paste0(pasteFlags, flagZ, flagQ)
      )
  }
  
  # Generate final QAQC flag only if flags exist
  if ("pasteFlags" %in% colnames(data)) {
    data <- data |> mutate(QAQC = ifelse(pasteFlags == "", NA, pasteFlags))
  }
  
  # Select only columns that exist in the original dataset
  selected_cols <- intersect(c("project", "siteId", "watershed", "datetime_UTC", "tempC", "sensorPressure_kPa", "waterHeight_m", 
                               "waterLevel_m", "waterElevation_m", "waterDepth_m", "wetdry", 
                               "Q_Ls_upper", "Q_Ls", "Q_Ls_lower", "QAQC"), colnames(data))
  
  return(data |> select(all_of(selected_cols)))
}


