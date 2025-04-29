#Lyne-Holick filter function
BaseflowSeparation <- function(Q, beta=0.925, passes=3){
  # R implementation of BFLOW baseflow separation algorithm as
  # described in Arnold & Allen (1999). This is the same as the 
  # original digital filter proposed by Lyne & Holick (1979) and
  # tested in Nathan & McMahon (1990). 
  #
  # It is called BFLOW because of this website: 
  #   http://www.envsys.co.kr/~swatbflow/USGS_GOOGLE/display_GoogleMap_for_SWAT_BFlow.cgi?state_name=indiana
  #
  # This is effectively the same as the 'BaseflowSeparation' function 
  # in the EcoHydRology package but with slightly different handling of 
  # start/end dates.
  #
  # Inputs:
  #   Q = discharge timeseries (no missing data) (any units are OK)
  #   beta = filter parameter; recommended value 0.925 (Nathan & McMahon, 1990); 0.9-0.95 reasonable range
  #   passes = how many times to go through the data (3=default=forward/backward/forward)
  #       
  # Output:
  #   bf = baseflow timeseries, same length and units as Q
  
  ## package dependencies
  require(zoo)
  require(dplyr)
  require(magrittr)
  
  # Q for use in calculations
  bfP <- Q
  
  for (p in 1:passes){
    # figure out start and end
    if ((p %% 2)==0){
      # backward pass
      i.start <- length(Q)-1
      i.end   <- 1
      i.fill  <- length(Q)
      ts      <- -1
    } else {
      # forward pass
      i.start <- 2
      i.end   <- length(Q)
      i.fill  <- 1
      ts      <- 1
    }
    
    # make empty vector
    qf <- rep(NaN, length=length(Q))
    
    # fill in value for timestep that will be ignored by filter
    if (p==1){
      qf[i.fill] <- bfP[1]*0.5
    } else {
      qf[i.fill] <- max(c(0, (Q[i.fill]-bfP[i.fill])))
    }
    
    # go through rest of timeseries
    for (i in i.start:i.end){
      qf[i] <- 
        (beta*qf[i-ts] + ((1+beta)/2)*(bfP[i]-bfP[i-ts]))
      
      # check to make sure not too high/low
      if (qf[i] > bfP[i]) qf[i] <- bfP[i]
      if (qf[i] < 0) qf[i] <- 0
    }
    
    # calculate bf for this pass
    bfP <- bfP-qf
    
    # when p==passes, return bfP
    if (p==passes){
      bf <- bfP
    }
    
  } # end of passes loop
  
  return(bf)
}