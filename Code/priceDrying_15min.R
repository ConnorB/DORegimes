#### Code Modified from Adam Price #### 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author: Adam Price 
# Co-authors: John Hammond and Nate Jones
# Tilte: Event Scale Analysis
# Date: 8/1/2020
# Description: Parallel process to examine individual storm events in USGS IRES 
#              gage data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Resources~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# https://cran.r-project.org/web/packages/foreach/vignettes/foreach.pdf
# https://stackoverflow.com/questions/29828710/parallel-processing-in-r-for-a-data-frame

# Create peak 2 zero function
metrics_fun <- function(df) {
  library(lubridate)
  library(tidyverse)
  
  # Process each site separately
  metrics <- df %>%
    # Rename columns and convert Date to POSIXct
    mutate(datetime = ymd_hms(Date),
           num_datetime = as.numeric(datetime),
           q = round(Flow, 1)) %>% 
    na.omit() %>%
    # Split the data by site_no
    group_by(site_no) %>%
    # Perform the calculations on each group
    group_modify( ~ {
      .x <- .x %>%
        mutate(q_peak = if_else(q > quantile(q, 0.95),  q, 0)) %>%
        mutate(
          # Calculate slope based on seconds
          slp_b = (q_peak - lag(q_peak)) / (num_datetime - lag(num_datetime)),
          slp_f = (lead(q_peak) - q_peak) / (lead(num_datetime) - num_datetime)
        ) %>%
        mutate(
          peak_flag = if_else(slp_b > 0.0001 & slp_f < 0, 1, 0),
          peak_flag = if_else(is.na(peak_flag), 0, peak_flag)
        ) %>%
        mutate(event_id = cumsum(peak_flag) + 1) %>%
        mutate(nf_start = if_else(q == 0 & lag(q) != 0, 1, 0))
      
      # Apply recession_fun and dry_fun for each site
      metrics_list <- lapply(
        X = seq(1, max(.x$event_id, na.rm = T)),
        FUN = function(m)
          full_join(recession_fun(m, .x), dry_fun(m, .x), by = "event_id")
      )
      
      bind_rows(metrics_list) %>%
        drop_na(dry_dur)
    })
  
  return(metrics)
}

# Modified recession_fun to include df argument
recession_fun <- function(m, df) {
  # Isolate individual recession events
  t <- df %>% filter(event_id == m)
  
  # Convert NA to zero
  t <- t %>% replace_na(list(nf_start = 0))
  
  # Compute drying regime stats if the following conditions exist
  if (sum(t$nf_start, na.rm = TRUE) != 0 &
      # there is a dry period in this event
      t$q[1] != 0 &
      # the event doesn't start with q=0
      sum(t$q_peak) != 0) {
    # there is no peak value
    
    # Define recession event as the length of time between peak and longest dry event before the next peak.
    # Define all drying event
    t <- t %>%
      # Number drying events
      mutate(dry_event_id = cumsum(nf_start)) %>%
      # Remove id number when > 0
      mutate(dry_event_id = if_else(q > 0, 0, dry_event_id))
    
    # Define dry date as the start of the longest drying event
    dry_date <- t %>%
      # Count length of individual drying events
      filter(dry_event_id > 0) %>%
      group_by(dry_event_id) %>%
      summarise(n = n(),
                datetime = min(datetime)) %>%
      # filter to max
      arrange(-n, datetime) %>%
      filter(row_number() == 1) %>%
      # isolate just the date
      select(datetime)
    
    # Dry Date
    t <- t %>% filter(datetime <= dry_date$datetime)
    
    # Define event_id
    event_id <- t$event_id[1]
    
    # Define Peak Data
    peak_datetime <- t$datetime[1]
    peak_value <- t$q[1]
    peak_quantile <- ecdf(df$q)(peak_value)
    
    # Define Peak to zero metric
    peak2zero <- nrow(t)
    
    # Create linear model of dQ vs q
    t <- t %>% mutate(dQ = lag(q) - q) %>% filter(dQ >= 0)
    model <- lm(log10(dQ + 0.1) ~ log10(q + 0.1), data = t)
    
    # Estimate drying rate [note the error catch for low slopes]
    drying_rate <-
      tryCatch(
        model$coefficients[2],
        error = function(e)
          NA
      )
    p_value <-
      tryCatch(
        summary(model)$coefficients[2, 4],
        error = function(e)
          NA
      )
    
    # Create output tibble
    output <-
      tibble(
        event_id,
        peak_datetime,
        peak_value,
        peak_quantile,
        peak2zero,
        drying_rate,
        p_value
      )
    
  } else {
    output <- tibble(
      event_id = t$event_id[1],
      peak_datetime = NA,
      peak_value = NA,
      peak_quantile = NA,
      peak2zero = NA,
      drying_rate = NA,
      p_value = NA
    )
  }
  
  # Export
  output
}



# Modified dry_fun to include df argument
dry_fun <- function(m, df) {
  # Isolate individual recession events
  t <- df %>% filter(event_id == m)
  
  # Convert NA to zero
  t <- t %>% replace_na(list(nf_start = 0))
  
  # If drying event occurs
  if (sum(t$nf_start, na.rm = TRUE) != 0) {
    # Define recession event as the length of time between peak and longest dry event before the next peak.
    # Define all drying events
    t <- t %>%
      # Number drying events
      mutate(dry_event_id = cumsum(nf_start)) %>%
      # Remove id number when > 0
      mutate(dry_event_id = if_else(q > 0, 0, dry_event_id))
    
    # Define longest dry event
    dry_event <- t %>%
      # Count length of individual drying events
      filter(dry_event_id > 0) %>%
      group_by(dry_event_id) %>%
      summarise(n = n(),
                datetime = min(datetime)) %>%
      # Filter to max
      arrange(-n, datetime) %>%
      filter(row_number() == 1) %>%
      # Isolate just the datetime
      select(dry_event_id) %>% pull()
    
    # Filter data frame to dry event
    t <- t %>% filter(dry_event_id == dry_event)
    
    # Create output
    output <- tibble(
      # Event_id
      event_id = t$event_id[1],
      # Define Year
      calendar_year = year(t$datetime[1]),
      # Define season
      season = if_else(
        month(t$datetime[1]) <= 3,
        "Winter",
        if_else(
          month(t$datetime[1]) > 3 & month(t$datetime[1]) <= 6,
          "Spring",
          if_else(month(t$datetime[1]) > 6 & month(t$datetime[1]) <= 9, "Summer", "Fall")
        )
      ),
      # Define meteorological year
      meteorologic_year = if_else(season == 'Winter', calendar_year - 1, calendar_year),
      # Define dry date start
      dry_date_start = t$datetime[1],
      # Define mean dry date
      dry_date_mean = mean(t$datetime, na.rm = TRUE),
      # Estimate dry duration in appropriate time units (e.g., hours, minutes)
      dry_dur = nrow(t)
    )
  } else {
    output <- tibble(
      event_id = t$event_id[1],
      calendar_year = NA,
      season = NA,
      meteorologic_year = NA,
      dry_date_start = NA,
      dry_date_mean = NA,
      dry_dur = NA
    )
  }
  
  # Export
  output
}


###### function to calc same event id on time series data to merge hydro metrics with
#Create new collumn with flow data 25% quantile for peak id
eventID <- function(df) {
  df <- df %>%
    # Convert Date to POSIXct and calculate numeric representation
    mutate(datetime = ymd_hms(Date),
           num_datetime = as.numeric(datetime),
           q = round(Flow, 1),
           q_peak = if_else(q > quantile(q, 0.95), q, 0))
  
  # Define peaks using slope break method
  df <- df %>% 
    # Define forward and backward slope at each point
    mutate(
      slp_b = (q_peak - lag(q_peak)) / (num_datetime - lag(num_datetime)),
      slp_f = (lead(q_peak) - q_peak) / (lead(num_datetime) - num_datetime)
    ) %>% 
    # Now flag those derivative changes
    mutate(
      peak_flag = if_else(slp_b > 0.0001 & slp_f < 0, 1, 0),
      peak_flag = if_else(is.na(peak_flag), 0, peak_flag)
    )
  
  # Define initiation of no flow
  df <- df %>%   
    # Define individual storm events
    mutate(
      event_id = cumsum(peak_flag) + 1,
      nf_start = if_else(q == 0 & lag(q) != 0, 1, 0)
    )
  
  return(df)
}