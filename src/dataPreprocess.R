#' Load in a dataset for training and pre-process for
#' acceptable data inputs
#' 
#' @param fileName A filename in string format.
#' @return tripData A data frame of the pre-processed csv file.
#' @examples
#' preprocessData('masterFile.csv')

preprocessData <- function(fileInput){
  tripData <- read.table(fileInput,header=TRUE, sep=',')
  tripData <- tripData[complete.cases(tripData),]
  tripData$uid <- as.factor(strtoi(tripData$uid,base=16))
  tripData$begintrip_at <- as.POSIXct( strptime(tripData$begintrip_at,"%Y-%m-%d_%H:%M:%S"))
  tripData$begintrip_lat <- as.numeric(as.character(tripData$begintrip_lat))
  tripData$begintrip_lng <- as.numeric(as.character(tripData$begintrip_lng))
  tripData$dropoff_lat <- as.numeric(as.character(tripData$dropoff_lat))
  tripData$dropoff_lng <- as.numeric(as.character(tripData$dropoff_lng))
  tripData <- tripData[complete.cases(tripData),]
  
  return(tripData)
  
}