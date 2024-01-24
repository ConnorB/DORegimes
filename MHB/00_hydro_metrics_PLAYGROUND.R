#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Flow metric calculation
#Date: 9/13/2022
#Coder: Nate Jones
#Purpose: Calculate Flow Metrics
# MHB edits
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(readxl)
library(lubridate)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# set working directory 
setwd("C:/Users/mhope/Desktop/Allen_Lab/RA/drying_regimes_2_github/")

#Clear memory
remove(list=ls())


#Define events
hydro_events <- read.csv("processed_data/FINAL/data_check.csv", header = TRUE) 
start_stop <- read.csv("processed_data/FINAL/hydro_event_start_stop.csv", 
                       header = TRUE) %>% 
  rename(SampleDate = ï..SampleDate) %>% 
  select(c(SampleDate, StartEvent, EndEvent, site))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Download logger data ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create function
download_fun<-function(n){
  #Download data
  x <- read_excel(
    path = "data/Loflen_Loggers_All_Data.xlsx", 
    sheet = sheets[n]
  )
  
  #Tidy
  x <- x %>% 
    select(
      datetime = starts_with("date"), 
      waterLevel = starts_with("water")) %>% 
    mutate(site_id = sheets[n])
  
  #get rid of extra water level collumns
  if("waterLevel1" %in% colnames(x)){
    x <- x %>% 
      rename(waterLevel = waterLevel1) %>% 
      select(-waterLevel2)
  }
  
  #Export
  x
  
}

#list excel sheets
sheets <- excel_sheets(path = "data/loflen_sites_final/Loflen_Loggers_All_Data.xlsx")

#Apply fun
waterLevel<-lapply(X=seq(1,length(sheets)), FUN = download_fun)
waterLevel<-bind_rows(waterLevel)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Estimate flow metrics ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tidy event df

## for full events
events<-hydro_events %>%
  #Filter to station code
  rename(site = stationcode) %>%
  # filter for hydro metrics
  filter(hydro_metrics ==  'full') # 30 total

# # for partial events
# events<-hydro_events %>%
#   #Filter to station code
#   rename(site = stationcode) %>%
#   # filter for hydro metrics
#   filter(hydro_metrics ==  'wetting')
# 
# events_slope <- hydro_events %>%
#   rename(site = stationcode) %>%
#   filter(hydro_metrics == "slope2sample_missing")
# 
# events <- rbind(events, events_slope) # 14 total

events <- left_join(events, start_stop, by = c("site", "SampleDate")) %>% 
  distinct() %>% 
  #Select cols of interest
  select('SampleDate', 'StartEvent', 'EndEvent',  'site')

# events$StartEvent <- as.Date(events$StartEvent, format = '%m/%d/%Y')



#3.2 Create functions ----------------------------------------------------------

fun<-function(n){
  #Prep Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Identify event of interest
  event<-events[n,]
  
  #Filter logger data to event
  wL <- waterLevel %>% 
    filter(site_id == event$site) %>% 
    filter(datetime > as.POSIXct(event$StartEvent, format = '%m/%d/%Y')) %>% 
    filter(datetime < as.POSIXct(event$EndEvent, format = '%m/%d/%Y'))
  # this only selects for the water level between the start and end date for 
    # the site
  
  #Estimate daily waterLevel
  wL <- wL %>% 
    mutate(day = date(datetime)) %>% 
    group_by(day) %>% 
    summarise(waterLevel = mean(waterLevel, na.rm=T))
  
  #For testing
  test_plot<-wL %>% ggplot() + geom_line(aes(x=day, y = waterLevel))
  test_plot # plots everything so nice
  
  #Find longest dry period and delete everything before it 
  # essentially defining where there is flow and no flow
    # seeing if following day is also zero or not
    # if not, creates a new column for each event and groups them
  wL <- wL %>% 
    #Define zeros
    mutate(zero = if_else(waterLevel == 0, 1, 0)) %>% 
    #Count zero events
    mutate(dry_start = if_else(zero == 1 & lag(zero) == 0, 1, 0)) %>% 
    mutate(dry_start = if_else(is.na(dry_start), 1, dry_start)) %>% 
    #Create groups
    mutate(dry_event = cumsum(dry_start))
    
  
  # identifies the dry event with the longest dry period
  period <- wL %>%   
    #Filter to zeros
    filter(waterLevel==0) %>% 
    #Estimate length of groups
    group_by(dry_event) %>% 
    summarise(count = n()) %>% 
    filter(count == max(count))

  
  # ### SEE NOTES FROM TODAY - THINK ABOUT DEFINING THE DRY DATE AS THE FIRST 
  # # NO FLOW DAY AFTER FAKE PEAK? 
  # ### OR DEFINE BASED ON THE DRY EVENT THAT LASTS FOR AT LEAST 10 CONSEC DAYS?
  #   ## TRY THAT ONE (LAG TIME)! BUT TAKE YOUR TIME AND THINK ABOUT IT....
  # # try to make a new one to pull for other periods
  # post_dry <- wL %>% 
  #   filter(dry_event >= period$dry_event) %>% 
  #   select(day, waterLevel)
  
  #For testing
  # now this only keeps the start of the dry day through the wet event
    # so how are the dry metrics cacualted? 
  test_plot<-wL %>% ggplot() + geom_line(aes(x=day, y = waterLevel))
  test_plot 

  # have that initial start day be at the beginning of the event
  start_date <- mdy(event$StartEvent)
  test_plot<-test_plot + geom_vline(aes(xintercept = start_date), col='gray')
  test_plot
  
  #Calculate metrics ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Define sampling date
  sampling_date <- mdy(event$SampleDate)
  test_plot<-test_plot + geom_vline(aes(xintercept = sampling_date), col='black')
  test_plot
  
  # #Determine first day of drying
  # dry_date <- post_dry %>% 
  #   filter(day > start_date,
  #          waterLevel == 0) %>% 
  #   summarise(day = min(day, na.rm=T)) %>% 
  #   pull() %>% 
  #   ymd(.)
  # test_plot<-test_plot + geom_vline(aes(xintercept = (dry_date)), col='red')
  # test_plot
  
  ### Determine dry_date, dame as rewet ###
  dry_date <- wL %>% 
    filter(day > start_date) %>% 
    mutate(
      lead_1 =lead(waterLevel, n=1),
      lead_2 =lead(waterLevel, n=2),
      lead_3 =lead(waterLevel, n=3),
      lead_4 =lead(waterLevel, n=4),
      lead_5 =lead(waterLevel, n=5),
      lead_6 =lead(waterLevel, n=6),
      lead_7 =lead(waterLevel, n=7),
      lead_8 =lead(waterLevel, n=8),
      lead_9 =lead(waterLevel, n=9),
      lead_10 =lead(waterLevel, n=10)) %>% 
    filter(waterLevel == 0) %>%
    filter(lead_1 == 0) %>% 
    filter(lead_2 == 0) %>% 
    filter(lead_3 == 0) %>% 
    filter(lead_4 == 0) %>% 
    filter(lead_5 == 0) %>%
    filter(lead_6 == 0) %>% 
    filter(lead_7 == 0) %>% 
    filter(lead_8 == 0) %>% 
    filter(lead_9 == 0) %>% 
    filter(lead_10 == 0) %>% 
    summarise(day = min(day,na.rm=T)) %>% 
    pull() %>% 
    ymd()
  # could define as 25% water level or could be 5 consecutive days of wet?
  test_plot<-test_plot + geom_vline(aes(xintercept = (dry_date)), col='red')
  test_plot
  

  #Determine first day of water
  first_wet_date <- wL %>%
    filter(day>dry_date) %>%
    filter(waterLevel>0) %>%
    summarise(day = min(day,na.rm=T)) %>%
    pull() %>%
    ymd()
  test_plot<-test_plot + geom_vline(aes(xintercept = (first_wet_date)), col='cyan')
  test_plot
  
  #Determine peak date
  peak_date <- wL %>% 
    filter(day>dry_date) %>%
    filter(day<sampling_date) %>% 
    filter(waterLevel == max(waterLevel, na.rm=T)) %>% 
    summarise(day = min(day,na.rm=T)) %>% 
    pull() %>% 
    ymd()
  test_plot<-test_plot + geom_vline(aes(xintercept = (peak_date)), col='purple')
  test_plot
  
  ### Determine rewet day ###
  rewet_date <- wL %>% 
    filter(day >= first_wet_date,
           day <= sampling_date) %>% 
    mutate(
      lead_1 =lead(waterLevel, n=1),
      lead_2 =lead(waterLevel, n=2),
      lead_3 =lead(waterLevel, n=3),
      lead_4 =lead(waterLevel, n=4),
      lead_5 =lead(waterLevel, n=5),
      lead_6 =lead(waterLevel, n=6),
      lead_7 =lead(waterLevel, n=7),
      lead_8 =lead(waterLevel, n=8),
      lead_9 =lead(waterLevel, n=9),
      lead_10 =lead(waterLevel, n=10)) %>% 
    filter(waterLevel > 0) %>%
    filter(lead_1 > 0) %>% 
    filter(lead_2 > 0) %>% 
    filter(lead_3 > 0) %>% 
    filter(lead_4 > 0) %>% 
    filter(lead_5 > 0) %>%
    filter(lead_6 > 0) %>% 
    filter(lead_7 > 0) %>% 
    filter(lead_8 > 0) %>% 
    filter(lead_9 > 0) %>% 
    filter(lead_10 > 0) %>% 
    summarise(day = min(day,na.rm=T)) %>% 
    pull() %>% 
    ymd()
  # could define as 25% water level or could be 5 consecutive days of wet?
  test_plot<-test_plot + geom_vline(aes(xintercept = (rewet_date)), col='blue')
  test_plot
  
  # ## manual for row 6 ("10/25/2015") / row 8 ("12/21/2015") / row 14 ("5/10/2015")
  # rewet_date <- wL %>%
  #   filter(day > mdy("5/10/2015")) %>%
  #   filter(waterLevel>0) %>%
  #   summarise(day = min(day,na.rm=T)) %>%
  #   pull() %>%
  #   ymd()
  # test_plot<-test_plot + geom_vline(aes(xintercept = (rewet_date)), col='blue')
  # test_plot

  #Determine FAKE peak date for recession coefficient
  FAKE_peak_date <- wL %>% 
    filter(day<dry_date) %>%
    filter(day>start_date) %>% 
    filter(waterLevel == max(waterLevel, na.rm=T)) %>% 
    summarise(day = min(day,na.rm=T)) %>% 
    pull() %>% 
    ymd()
  test_plot<-test_plot + geom_vline(aes(xintercept = (FAKE_peak_date)), col='green')
  test_plot
  
  # # count false starts! (bianary)
  # false_start_time <- wL %>%
  #   filter(day > dry_date,
  #          day < rewet_date) %>%
  #   #Count greater than 0 events
  #   mutate(wet_point = if_else(zero == 0, 1, 0)) 
  # false_start <- ifelse(sum(false_start_time$wet_point, na.rm=T)>0, 1, 0)
  # false_start
  
  #count false starts (Nate Dog)
  false_starts <- wL %>%
    filter(day > dry_date,
           day < rewet_date) %>% 
    mutate(count = if_else(lag(waterLevel==0) & waterLevel>0, 1, 0)) %>% 
    summarise(sum(count)) %>% # number of times went from 0 to something
    pull() 
  false_starts
  
  #Antedent time period 
  antecedent_time_period_days <- sampling_date - dry_date
  antecedent_time_period_days <- antecedent_time_period_days %>% paste %>% as.numeric
  antecedent_time_period_days
  
  #Dry duration
  dry_duration <- rewet_date - dry_date 
  dry_duration <- dry_duration %>% paste %>% as.numeric
  dry_duration 
  
  #Proportion of wet days
  prop_wet_days <- wL %>% 
    filter(day>dry_date) %>% 
    filter(day<sampling_date) %>% 
    filter(waterLevel>0) %>% 
    summarise(wet_days = n()) %>% 
    mutate(prop_wet_days = wet_days/antecedent_time_period_days) %>% 
    select(prop_wet_days) %>% 
    pull()
  prop_wet_days
  
  #Proportion of dry days
  prop_dry_days <- 1-prop_wet_days
  prop_dry_days
  
  #Rewet duration
  rewet_duration <- peak_date - rewet_date
  rewet_duration <- rewet_duration %>% paste %>% as.numeric
  rewet_duration
  
  #Rewet Slope
  rewet_slope <- wL %>% 
    filter(day<=peak_date) %>% 
    filter(day>=rewet_date) %>% 
    mutate(slope = lead(waterLevel) - waterLevel) %>% 
    filter(slope>0) %>% 
    summarise(slope = median(slope, na.rm=T)) %>% 
    pull()
  rewet_slope
  
  #peak2sample duration
  peak2sample_duration <- sampling_date - peak_date
  peak2sample_duration <- peak2sample_duration %>% paste %>% as.numeric
  peak2sample_duration
  
  #Peak2Sample Slope
  peak2sample_slope <- wL %>% 
    filter(day>=peak_date) %>% 
    filter(day<=sampling_date) %>% 
    mutate(slope = lead(waterLevel) - waterLevel) %>% 
    summarise(slope = median(slope, na.rm=T)) %>% 
    pull()
  peak2sample_slope
  
  #Peak depth
  peak_depth<- wL %>% 
    filter(day >= rewet_date) %>% 
    summarise(max(waterLevel, na.rm = T)) %>% 
    pull()
  peak_depth
  
  #Recession Rate
  r <- wL %>% 
    filter(day<=dry_date,
           day>=FAKE_peak_date) %>% 
    mutate(slope = lead(waterLevel) - waterLevel) 
  recession_coef <- -1*summary(lm(r$slope~r$waterLevel))$coef[2,1]
  recession_rsq <- summary(lm(r$slope~r$waterLevel))$r.squared
  recession_coef
  recession_rsq
  # relationship between the water level and how fast it dries
  # this has been important for biogeochem processes at least
  
  #Hello..this is GOD. You are going to graduate. Don't stress.
  
  # try this way too...
  recession_slope <- wL %>% 
    filter(day<=dry_date,
           day>=FAKE_peak_date) %>% 
    mutate(slope = lead(waterLevel) - waterLevel) %>% 
    summarise(slope = median(slope, na.rm=T)) %>% 
    pull()
  recession_slope
 ## these are really different... 
  # use median!! - less sensitive to outliers (SWITCH FOR EVERYTHING)
  ## could also just use the last - first (not averaging!)
  
  # also wanted to get the total_rewet times
  total_rewet <- (rewet_duration + peak2sample_duration)
  
  #Export plot ### N WILL BE THE ROW NUMBER!!!!
  
  ggsave(paste0("processed_data/hydrographs/", n, "_plot.png"), test_plot, 
         width=5, height =3, units = "in")
  
  #Create Export
  output<-tibble(
    site = event$site,
    SampleDate = event$SampleDate,
    antecedent_time_period_days, 
    dry_date,
    dry_duration,
    peak_date,
    peak_depth,
    peak2sample_duration,
    peak2sample_slope,
    prop_dry_days,
    prop_wet_days,
    recession_coef,
    recession_rsq,
    recession_slope,
    rewet_date,
    false_starts,
    first_wet_date,
    rewet_duration,
    rewet_slope,
    total_rewet,
    sampling_date
  )
  
  output
}

#3.3 Run event -----------------------------------------------------------------
#Create wrapper function 
error_fun<-function(n){
  tryCatch(
    expr = fun(n), 
    error = function(e)
      output<-tibble(
        site = events$site[n],
        SampleDate = events$SampleDate[n],
        antecedent_time_period_days = NA, 
        dry_date = NA,
        dry_duration = NA,
        peak_date = NA,
        peak_depth = NA,
        peak2sample_duration = NA,
        peak2sample_slope = NA,
        prop_dry_days = NA,
        prop_wet_days = NA,
        recession_coef = NA,
        recession_rsq = NA,
        rewet_date = NA,
        false_starts = NA,
        first_wet_date = NA, 
        rewet_duration = NA,
        rewet_slope = NA,
        sampling_date = NA
      )
  )
}  

#Run wrapper fun
metrics <- lapply(
  X = seq(1,nrow(events)), 
  FUN = error_fun) %>% 
  bind_rows()

# just to see what is missing - esp for partial events
hydro <- hydro_events %>% 
  select(c(stationcode, SampleDate, hydro_metrics, notes)) %>% 
  rename(site = stationcode) %>% 
  unique()

metrics <- left_join(metrics, hydro)

# write_csv(metrics, "processed_data/FINAL/final_metrics_full_events.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Patterns and Distributions ------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(psych)

metrics <- read.csv("processed_data/FINAL/final_metrics_full_events.csv", 
                    header = TRUE) %>% 
  select(-c(hydro_metrics, notes)) %>% 
  separate(SampleDate, into = c("month", "day", "year"), sep = "/", 
           remove = FALSE)

summary(metrics)

# distribution of Rsq
plot(metrics$recession_coef, metrics$recession_rsq)

# comparison of coefficient vs slope
plot(metrics$recession_coef, metrics$peak2sample_slope)

# distribution over years
plot(metrics$year, metrics$peak_depth)

# summary table
table <- metrics[ , c(6:25)]
table <- table %>%
  select(-ends_with("_date"))

table <- describe(table, fast = TRUE)
# write.csv(table, "processed_data/FINAL/extra_info/metric_summary.csv")
