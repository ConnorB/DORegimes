rolling_Z <- function(x, window_size = 30) {
  # Input validation
  if (!is.numeric(x)) stop("Input must be a numeric vector")
  if (window_size < 2) stop("Window size must be at least 2")
  
  # Pre-allocate vectors for speed
  n <- length(x)
  roll_mean <- roll_sd <- rep(NA_real_, n)
  half_window <- floor(window_size/2)
  
  # Calculate once outside the loop
  window_size_minus_1 <- window_size - 1
  
  # Main calculation loop
  for(i in (half_window + 1):(n - half_window)) {
    # Get window indices excluding center point
    window_idx <- (i - half_window):(i + half_window)
    window_idx <- window_idx[window_idx != i]
    
    # Calculate stats excluding middle value
    window_vals <- x[window_idx]
    roll_mean[i] <- mean(window_vals, na.rm = TRUE)
    roll_sd[i] <- sd(window_vals, na.rm = TRUE)
  }
  
  # Vectorized z-score calculation
  zscore <- (x - roll_mean) / roll_sd
  
  # Vectorized flag creation
  as.integer(abs(zscore) > 3)
}

rolling_Z_multi <- function(data, window_size = 30, columns = NULL) {
  # Early exit for empty data
  if (length(data) == 0) return(data)
  
  # Convert list to dataframe if needed
  result_df <- if(is.data.frame(data)) data else as.data.frame(data)
  
  # Fast column type check
  if (is.null(columns)) {
    numeric_cols <- which(vapply(result_df, is.numeric, logical(1)))
    
    # Fast pattern matching
    col_names <- names(result_df)[numeric_cols]
    exclude_pattern <- "^(ppt|precip|lat|long|elev)"
    exclude_cols <- grepl(exclude_pattern, col_names, ignore.case = TRUE)
    
    columns <- col_names[!exclude_cols]
    
    if (length(columns) == 0) {
      stop("No valid numeric columns found")
    }
  } else {
    # Validate columns exist
    missing_cols <- setdiff(columns, names(result_df))
    if (length(missing_cols) > 0) {
      stop("Columns not found: ", paste(missing_cols, collapse = ", "))
    }
    
    # Fast exclude check
    exclude_pattern <- "^(ppt|precip|lat|long|elev)"
    exclude_cols <- grepl(exclude_pattern, columns, ignore.case = TRUE)
    if (any(exclude_cols)) {
      warning("Excluding columns: ", paste(columns[exclude_cols], collapse = ", "))
      columns <- columns[!exclude_cols]
    }
    
    # Check numeric
    non_numeric <- !vapply(result_df[columns], is.numeric, logical(1))
    if (any(non_numeric)) {
      stop("Non-numeric columns: ", paste(columns[non_numeric], collapse = ", "))
    }
  }
  
  # Pre-allocate results for all columns at once
  n_rows <- nrow(result_df)
  for(col in columns) {
    result_df[[paste0(col, "_zFlag")]] <- numeric(n_rows)
  }
  
  # Process columns
  for(col in columns) {
    result_df[[paste0(col, "_zFlag")]] <- rolling_Z(result_df[[col]], window_size)
  }
  
  result_df
}

consolidate_flags <- function(df) {
  # Make a copy of the input dataframe
  result_df <- data.frame(df)
  
  # Input validation
  if (!is.data.frame(result_df)) {
    stop("Input must be a data frame")
  }
  
  # Identify zFlag columns
  flag_cols <- grep("_zFlag$", names(result_df), value = TRUE)
  if (length(flag_cols) == 0) {
    stop("No zFlag columns found in the data frame")
  }
  
  # Initialize empty flag column as character
  result_df$Flag <- character(nrow(result_df))
  
  # Process each flag column
  for (col in flag_cols) {
    # Extract base variable name (remove _zFlag suffix)
    base_var <- sub("_zFlag$", "", col)
    
    # Get first letter of the base variable name
    flag_letter <- substr(base_var, 1, 4)
    
    # Add flag letter where zFlag is 1
    result_df$Flag <- ifelse(
      result_df[[col]] == 1,
      ifelse(
        result_df$Flag == "",
        flag_letter,
        paste(result_df$Flag, flag_letter)
      ),
      result_df$Flag
    )
  }
  
  # Convert empty strings to NA
  result_df$Flag[result_df$Flag == ""] <- NA
  
  # Remove original zFlag columns
  result_df <- result_df[, !grepl("_zFlag$", names(result_df))]
  
  return(result_df)
}

rm_flagged <- function(df) {
  # Input validation
  if (!is.data.frame(df) || !"Flag" %in% names(df)) {
    stop("Input must be a data frame with a 'Flag' column")
  }
  
  # Create a copy of the input dataframe
  result_df <- data.frame(df)
  
  # Find all numeric columns (excluding Flag column)
  numeric_cols <- names(result_df)[sapply(result_df, is.numeric)]
  
  # Create mapping of column prefixes to full column names
  col_mapping <- list()
  for (col in numeric_cols) {
    # Get first 4 characters of column name, ignoring case
    prefix <- tolower(substr(col, 1, 4))
    col_mapping[[prefix]] <- col
  }
  
  # Process each row
  for (i in 1:nrow(result_df)) {
    if (!is.na(result_df$Flag[i])) {
      # Split the flag string into individual flags
      flags <- strsplit(result_df$Flag[i], " ")[[1]]
      
      # For each flag, set corresponding value to NA
      for (flag in flags) {
        flag_prefix <- tolower(substr(flag, 1, 4))
        if (flag_prefix %in% names(col_mapping)) {
          result_df[i, col_mapping[[flag_prefix]]] <- NA
        }
      }
    }
  }
  
  return(result_df)
}
