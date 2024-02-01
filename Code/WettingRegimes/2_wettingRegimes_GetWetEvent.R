#####################################################################
##
## Script name: 2_wettingRegimes_GetWetEvent.R
##
## Author: Adam N. Price
##
## Date Created: 2022-02-25
##
## Copyright (c) Adam N. Price, 2022
## Email: adnprice@ucsc.edu
##
############################# Description ##########################
##
## Code to extract rewetting events at different flow thresholds
##   22-02-25: Current threholds are Q5,10,15,25,50, and peaks after
##             those thresholds are met
##
############################# Packages #############################
library(parallel)
library(lubridate)
library(tidyverse)
library(here)
##   
##
############################# Code ################################



# Get list of files
files <- list.files(here("Data/USGS_Q"),pattern = "*.csv",full.names = TRUE)

gage <- as.character(tools::file_path_sans_ext(basename(files)))

# Load wetting date data
wet = read_csv(here("Data/wettingRegimes_wetDate.csv"))
wet$wet_date = lubridate::as_date(wet$wet_date)


## Define the function to extract revwetting events

calcRewetEvent <- function(n){
  library(lubridate)
  library(tidyverse)
  
  # for (n in 1:nrow(wet)){
  
  
  output = tibble(
    event = NA,
    gage = NA,
    wet_date =NA,
    wetLengthDate = NA,
    wetLength = NA,
    wetLengthQ = NA,
    wetPeak = NA,
    wetPeakLength = NA,
    wetPeakQ = NA,
    Q_threshold = NA,
    Q_threshold_q = NA,)
  
  
  whichfile <- grep(
    x = files,
    pattern = wet$gage[n],
    value = TRUE
  )
  
  
  dat <- read_csv(file = whichfile) %>% 
    mutate(date=as_date(Date), 
           num_date = as.numeric(Date), 
           q = Flow) %>% 
    select(date, num_date, q) %>% 
    na.omit() 
  
  dat$date = lubridate::as_date(dat$date)
  wet_date = wet$wet_date[n]
  gage = wet$gage[n]

  p_list = c(0.05,0.1,0.15,0.25,0.50)
  for (i in p_list){
    df<-dat %>%
      #Round to nearest tenth
      mutate(q = round(q, 1)) %>%
      #1% quantile thresholds
      mutate(q_peak = if_else(q>quantile(q,i),  q, 0))
    
    df<-df %>% 
      #Define forward and backward slope at each point
      mutate(
        slp_b = (q_peak-lag(q_peak))/(num_date-lag(num_date)), 
        slp_f = (lead(q_peak)-q_peak)/(lead(num_date)-num_date), 
        slp_f = (lead(q_peak)-q_peak)/(lead(num_date)-num_date),  
      ) %>% 
      #now flag those derivative changes
      mutate(peak_flag = if_else(slp_b>0.000 & slp_f>0, 0,1),
             peak_flag = if_else(is.na(peak_flag), 0, peak_flag)) 
    
    # Filter out any data before wet date
    temp = df[df$date>=wet_date,]
    
    # Determine the wet length date
    wetLengthDate  = temp[temp$q_peak>0,]$date[1]
    
    # Determine the wetting length
    wetLength =  as.numeric(wetLengthDate - wet_date,units = "days")
    
    # Value at that length
    wetLengthQ = temp[temp$q_peak>0,]$q_peak[1]
    
    ## Get the date for the next wetPeak
    wetPeak = temp[temp$slp_f<0,]$date[1]
    
    ## Get the length to the next wetPeak
    wetPeakLength =  as.numeric(wetPeak - wet_date,units = "days")
    
    ## Get the value at that date
    wetPeakQ = temp[temp$slp_f<0,]$q_peak[1]
    
    ## Write the percentile used in filering and the discharge assocaited
    Q_threshold = i
    Q_threshold_q = quantile(dat$q,i)[[1]]
    
    event = n
    # output = output %>% add_row(event,gage,cluster,wet_date,wetLengthDate,wetLength,wetLengthQ,wetPeak,wetPeakLength,wetPeakQ,Q_threshold)
    output = output %>% add_row(event,gage,wet_date,wetLengthDate,wetLength,wetLengthQ,wetPeak,wetPeakLength,wetPeakQ,Q_threshold,Q_threshold_q)
  }
  ## Call output
  output
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Execute and write-----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Start timer
t0<-Sys.time()

#Create error handling function
execute<-function(a){
  tryCatch(calcRewetEvent(a), error=function(e){
    tibble(
      event = NA,
      gage = NA,
      cluster = NA,
      wet_date =NA,
      wetLengthDate = NA,
      wetLength = NA,
      wetLengthQ = NA,
      wetPeak = NA,
      wetPeakLength = NA,
      wetPeakQ = NA,
      Q_threshold = NA,
      Q_threshold_q = NA)}
  )
}

# get number of cores
n.cores <- detectCores() - 1

#start cluster
cl <-  makePSOCKcluster(n.cores)

#Export file list to cluster
clusterExport(cl, c('files', 'calcRewetEvent','wet'), env=.GlobalEnv)

# Use mpapply to exicute function
x<-parLapply(cl,seq(1, nrow(wet)),execute) #length(files)

# Stop the cluster
stopCluster(cl)

#gather output
output<-bind_rows(x)

#Capture finishing time
tf<-Sys.time()
tf-t0

# Write the data
write_csv(output,here("Data/wettingRegimes_wettingLengths.csv"))