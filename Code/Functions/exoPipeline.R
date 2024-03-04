#Function to read data from a given site directory from the AIMS EXO QAQC Pipeline and add site name
read_QAQCdata <- function(Sites) {
  sitePath <- file.path(qaqcPath, Sites)
  datFiles <- list.files(sitePath, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  
  if (length(datFiles) > 0) {
    data <- rbindlist(lapply(datFiles, fread, colClasses = c("timestamp" = "character")), idcol = "exportFile")
    data[, Site := Sites] # Add site name column
    return(data)
  } else {
    return(NULL)
  }
}