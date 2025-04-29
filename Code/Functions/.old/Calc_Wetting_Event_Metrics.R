library(lubridate)
library(tidyverse)
library(dataRetrieval)

rewet_metrics <- function(gageNumber, startDate, endDate, filePath){
  
  # Ensure the siteNumber is properly formatted with leading zeros
  siteNumber <- str_pad(gageNumber, 8, pad = "0")
  parameterCd <- "00060"
  
  # Fetch data from USGS using the provided parameters
  dat <- readNWISuv(siteNumber, parameterCd, startDate, endDate)
  
  dat$q_peak <- round(dat$X_00060_00000,1)
  
  
  dat <- dat %>% 
    mutate(
      slp_b = (q_peak-lag(q_peak))/(as.numeric(dateTime-lag(dateTime))), 
      slp_f = (lead(q_peak)-q_peak)/(as.numeric(lead(dateTime)-dateTime))
    ) %>% 
    mutate(peak_flag = if_else(slp_b>=0 & slp_f<0, 1,0),
           peak_flag = if_else(is.na(peak_flag), 0, peak_flag))
  
  tt <- dat %>%
    filter(peak_flag==1) %>%
    select(site_no, dateTime, X_00060_00000)
  
  if(nrow(tt) > 0){
    initEvent <- dat[dat$dateTime <= tt$dateTime[1] & dat$q_peak > 0,]
    
    if(nrow(initEvent) > 0){
      t <- initEvent %>% mutate(dQ = abs(lag(X_00060_00000) - X_00060_00000)) %>% filter(dQ >= 0)
      model <- lm(log10(dQ+0.1) ~ log10(X_00060_00000+0.1), data = t)
      
      inital_rewetting_rate <- tryCatch(as.numeric(model$coefficients[2]), error = function(e) NA)
      p_value_i <- tryCatch(summary(model)$coefficients[2,4], error = function(e) NA)
      
      entireEvent <- dat[dat$dateTime <= tt$dateTime[nrow(tt)] & dat$dateTime >= initEvent$dateTime[1],]
      
      t <- entireEvent %>% mutate(dQ = abs(lag(X_00060_00000) - X_00060_00000)) %>% filter(dQ >= 0)
      model <- lm(log10(dQ+0.1) ~ log10(X_00060_00000+0.1), data = t)
      
      rewetting_rate <- tryCatch(as.numeric(model$coefficients[2]), error = function(e) NA)
      p_value <- tryCatch(summary(model)$coefficients[2,4], error = function(e) NA)
    } else {
      inital_rewetting_rate <- NA
      p_value_i <- NA
      rewetting_rate <- NA
      p_value <- NA
    }
  } else {
    inital_rewetting_rate <- NA
    p_value_i <- NA
    rewetting_rate <- NA
    p_value <- NA
  }
  
  # Plotting and saving the figure
  plot <- ggplot(dat, aes(x = dateTime, y = X_00060_00000)) +
    geom_line() +
    labs(title = paste("Gage:", siteNumber),
         x = "Date",
         y = "Discharge (cfs)") +
    theme_few()
  
  # Create the file path for saving the plot
  fileName <- paste0(siteNumber, "_", gsub("-", "", startDate), ".png")
  ggsave(fileName, plot = plot, width = 10, height = 6, path = filePath)
  
  
  # Construct the output data frame with only the specified information
  output <- tibble(
    gage = siteNumber,
    start_date = startDate,
    end_date = endDate,
    inital_rewetting_rate = inital_rewetting_rate,
    p_value_i = p_value_i,
    rewetting_rate = rewetting_rate,
    p_value = p_value
  )
  
  return(output)
}
