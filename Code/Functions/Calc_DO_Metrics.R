library(dataRetrieval)
library(dplyr)
library(lubridate)
library(tidyr)
library(nasapower)
library(streamMetabolizer)

do_metrics <- function(site_no, start_date, end_date, latitude, longitude, elevation) {
  parameterCd <- c("00300", "00010")
  
  # Download USGS data
  usgsDO <- readNWISuv(site_no, parameterCd, start_date, end_date)
  usgsDO <- renameNWISColumns(usgsDO)
  usgsDO <- usgsDO %>% 
    rename(DO.obs = DO_Inst, temp.water = Wtemp_Inst)
  
  cat(paste("Downloading data for", site_no, "\n"))
  
  # Download NASA POWER data
  tempDat <- get_power(
    community = "sb",
    pars = c("PSC"),
    temporal_api = "hourly",
    lonlat = c(longitude, latitude),
    dates = c(start_date, end_date),
    site_elevation = elevation,
    time_standard = "UTC")
  
  cat(paste("Finished downloading data for", site_no, "\n"))
  
  # Process NASA POWER data
  tempDat <- tempDat %>%
    mutate(Time = paste0(str_pad(HR, 2, side = "left", pad = "0"), ":00")) %>% 
    unite("dateTime", YYYYMMDD, Time, sep = " ") %>% 
    mutate(dateTime = ymd_hm(dateTime, tz = "UTC"),
           BP = PSC * 10) %>% 
    select(dateTime, BP)
  
  # Prepare a data frame of date and time
  min.fill <- data.frame(dateTime = seq(
    from = floor_date(min(tempDat$dateTime), "15 mins"), 
    to = ceiling_date(max(tempDat$dateTime), "15 mins"),
    by = "15 mins"))
  
  # Merge NASA data with a continuous time series
  nasaDat <- merge(min.fill, tempDat, by = "dateTime", all = TRUE) %>%
    mutate(BP = na.approx(BP, na.rm = FALSE, maxgap = 5))
  
  # Join USGS and NASA data and calculate DO metrics
  usgsDO <- usgsDO %>% 
    left_join(nasaDat, by = "dateTime") %>%
    mutate(site_no = site_no,
           DO.sat = calc_DO_sat(temp.water = temp.water, pressure.air = BP, model = "garcia-benson"),
           DO.pctsat = (DO.obs/DO.sat)*100,
           solar.time = convert_UTC_to_solartime(date.time = dateTime, longitude = longitude, time.type = "mean solar"),
           isDaytime = calc_is_daytime(solar.time, lat = latitude)) %>% 
    select(dateTime, solar.time, site_no, DO.obs, DO.sat, DO.pctsat, temp.water, BP, isDaytime) %>% 
    drop_na() %>% 
    arrange(solar.time, site_no)
  
  # Calculate daily DO metrics
  dailyDO <- usgsDO %>%
    mutate(Date = floor_date(solar.time, "1 day")) %>%
    group_by(site_no, Date) %>%
    summarize(minDO.obs = min(DO.obs, na.rm = TRUE),
              maxDO.obs = max(DO.obs, na.rm = TRUE),
              minDO.sat = min(DO.sat, na.rm = TRUE),
              maxDO.sat = max(DO.sat, na.rm = TRUE),
              minDO.pctsat = min(DO.pctsat, na.rm = TRUE),
              maxDO.pctsat = max(DO.pctsat, na.rm = TRUE),
              avgDO.obs = mean(DO.obs, na.rm = TRUE),
              avgDO.sat = mean(DO.sat, na.rm = TRUE),
              avgDO.pctsat = mean(DO.pctsat, na.rm = TRUE),
              totalDO = n(),
              DO.hyp = sum(DO.obs <= 2, na.rm = TRUE)) %>%
    mutate(DO.amp = maxDO.obs - minDO.obs,
           DOsat.amp = maxDO.sat - minDO.sat,
           DOpctsat.amp = maxDO.pctsat - minDO.pctsat,
           probHyp = DO.hyp / totalDO) %>%
    arrange(site_no, Date)
  
  #nighttime hypoxia ratio
  nightHypRat <- usgsDO %>% 
    mutate(Date = floor_date(solar.time, "1 day")) %>%
    group_by(Date, site_no, isDaytime) %>% 
    summarise(DO.hyp = sum(DO.obs <= 2, na.rm = T)) %>% 
    pivot_wider(names_from = isDaytime, values_from = DO.hyp, names_prefix = "DO_hyp_") %>%
    rename(DayHyp = DO_hyp_TRUE, NightHyp = DO_hyp_FALSE) %>% 
    mutate(NHR = NightHyp / DayHyp)
  
  doMet <- dailyDO %>% 
    full_join(nightHypRat, by = c("Date", "site_no")) %>% 
    arrange(site_no, Date)
  
  # Return only the required DO metrics DataFrame
  return(doMet)
}
