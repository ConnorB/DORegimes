#####################################################################
##
## Script name: 3_wettingRegimes_GetSubEventsRate.R
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
library(dataRetrieval)
library(here)
##   
##
############################# Code ################################

# Load wetting date data
wetEvents = read_csv(here("Data/wettingRegimes_wettingLengths.csv")) %>%
  drop_na(event)

## Define the function to extract revwetting events


metrics_fun <- function(n){
  library(lubridate)
  library(tidyverse)
  
  
  siteNumber = str_pad(wetEvents$gage[n], 8, pad = "0")
  parameterCd = "00060"
  startDate = wetEvents$wet_date[n]-1
  endDate = wetEvents$wetLengthDate[n]
  
  dat = dataRetrieval::readNWISuv(
    siteNumber,
    parameterCd,
    startDate,
    endDate
  )
  
  dat$q_peak= round(dat$X_00060_00000,1)
  
  dat<-dat %>% 
    #Define forward and backward slope at each point
    mutate(
      slp_b = (q_peak-lag(q_peak))/(as.numeric(dat$dateTime-lag(dat$dateTime))), 
      slp_f = (lead(q_peak)-q_peak)/(as.numeric(lead(dat$dateTime)-dat$dateTime))
    ) %>% 
    #now flag those derivative changes
    # mutate(peak_flag = if_else(slp_b>0.0001 & slp_f<0, 1,0),
    mutate(peak_flag = if_else(slp_b>=0 & slp_f<0, 1,0),
           peak_flag = if_else(is.na(peak_flag), 0, peak_flag)) 
  
  tt <- dat %>%
    filter(peak_flag==1) %>%
    select(site_no,dateTime,X_00060_00000) %>%
    setNames(c("gage","wet_DateTime","q"))
  
  
  initEvent = dat[dat$dateTime<=tt$wet_DateTime[1] & dat$q_peak>0,]
  
  #Create linear model of dQ vs q
  t<- initEvent %>% mutate(dQ = abs(lag(X_00060_00000) - X_00060_00000)) %>% filter(dQ>=0)
  model<-lm(log10(dQ+0.1)~log10(X_00060_00000+0.1), data=t)
  
  inital_rewetting_rate <- tryCatch(as.numeric(model$coefficients[2]), error = function(e) NA)
  p_value_i <- tryCatch(summary(model)$coefficients[2,4], error = function(e) NA)
  
  entireEvent = dat[dat$dateTime<=tt$wet_DateTime[nrow(tt)] & dat$dateTime>=initEvent$dateTime[1],]
  
  t<- entireEvent %>% mutate(dQ = abs(lag(X_00060_00000) - X_00060_00000)) %>% filter(dQ>=0)
  model<-lm(log10(dQ+0.1)~log10(X_00060_00000+0.1), data=t)
  
  rewetting_rate <- tryCatch(as.numeric(model$coefficients[2]), error = function(e) NA)
  p_value <- tryCatch(summary(model)$coefficients[2,4], error = function(e) NA)
  
  output <- dat %>%
    tibble() %>%
    filter(peak_flag==1) %>%
    select(site_no,dateTime,X_00060_00000) %>%
    setNames(c("gage","wet_DateTime","q")) %>%
    mutate(rewetting_event = wetEvents$event[n],
           gage =  wetEvents$gage[n],
           threshold = wetEvents$Q_threshold[n],
           weting_subevent = row_number(),
           total_false_starts = count(.),
           inital_rewetting_rate = inital_rewetting_rate,
           p_value_i = p_value_i,
           rewetting_rate = rewetting_rate,
           p_value = p_value,
           sample_rate = as.numeric(difftime(dat$dateTime[2],dat$dateTime[1],units='hours')))
  
  tempInit = initEvent[1,] %>%
    tibble() %>%
    select(site_no,dateTime,X_00060_00000) %>%
    setNames(c("gage","wet_DateTime","q")) %>%
    mutate(rewetting_event = wetEvents$event[n],
           gage =  wetEvents$gage[n],
           threshold = wetEvents$Q_threshold[n],
           weting_subevent = 0,
           total_false_starts = count(output),
           inital_rewetting_rate = inital_rewetting_rate,
           p_value_i = p_value_i,
           rewetting_rate = rewetting_rate,
           p_value = p_value,
           sample_rate = as.numeric(difftime(dat$dateTime[2],dat$dateTime[1],units='hours')))
  
  output <- output %>% add_row(tempInit,.before=1)
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
  tryCatch(metrics_fun(a), error=function(e){
    tibble(
      gage = NA,
      threshold = NA,
      wet_DateTime = NA,
      q = NA,
      rewetting_event =NA,
      weting_subevent = NA,
      total_false_starts = NA,
      inital_rewetting_rate = NA,
      p_value_i = NA,
      rewetting_rate = NA,
      p_value = NA,
      sample_rate = NA)}
  )
}

# get number of cores
n.cores <- detectCores()

#start cluster
cl <-  makePSOCKcluster(n.cores)

#Export file list to cluster
clusterExport(cl, c('metrics_fun','wetEvents'), env=.GlobalEnv)

# Use mpapply to exicute function
x<-parLapply(cl,seq(1, nrow(wetEvents)),execute) #length(files)
# x<-parLapply(cl,seq(1381, 1385),execute) #length(files)

# Stop the cluster
stopCluster(cl)

#gather output
output<-bind_rows(x) %>% 
  drop_na(gage) %>% 
  mutate(total_false_starts = total_false_starts$n)

#Capture finishing time
tf<-Sys.time()
tf-t0

# Write the data
write_csv(output, here("Data/wettingRegimes_SubEvents.csv"))
