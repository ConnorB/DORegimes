metrics_fun <- function(df){
  library(lubridate)
  library(tidyverse)
  
  
  siteNumber = str_pad(df$gage[n], 8, pad = "0")
  parameterCd = "00060"
  startDate = df$wet_date[n]-1
  endDate = df$wetLengthDate[n]
  
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
    mutate(rewetting_event = df$event[n],
           gage =  df$gage[n],
           threshold = df$Q_threshold[n],
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
    mutate(rewetting_event = df$event[n],
           gage =  df$gage[n],
           threshold = df$Q_threshold[n],
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
