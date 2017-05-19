#' createSplits
#' @description
#' Function to split an activity into custom splits
#' @param data (dataframe) a dataframe generated with gpxAnalyser::dataLoader()
#' @param splitValues (numeric vector) the split points
#' \itemize{
#' \item To split for distance enter the value in meters, i.e. 1000 to split at every Km
#' \item To split for distance enter the value in minutes, i.e. 5.5 to split at every 5min30sec
#' }
#' @param type (character). one of the following:
#' \itemize{
#' \item 'everyKm' to split at every Km, i.e. every 5Km
#' \item 'thisKm' to split at specific Km values, i.e. 1, 4, 6, 7 Km
#' \item 'everyMin' to split at every min, i.e. every 5min
#' \item 'thisMin' to split at specific min, i.e. 1, 4, 6, 7 min
#' }
#' @return
#' The function returns a list of data frames, one for each split
#' @details
#' The function accepts a data frame created with the dataLoader
#' function
#' @examples
#' gpx <- evenActivity
#' sp<-createSplits(gpx, 2000, type = "everyKm")
#' sp<-createSplits(gpx, c(1000, 3000, 4500), type = "thisKm")
#' sp<-createSplits(gpx, 2, type = "everyMin")
#' sp<-createSplits(gpx, c(5, 15, 35), type = "thisMin")
#' @export

createSplits <- function (data, splitValues,
                          type = c("everyKm","thisKm", "everyMin", "thisMin")[1]){
  
  if("DistanceMeters" %in% colnames(data) & any(diff(data$DistanceMeters)<0)){
    message("This workout is likely a swim. It will be split as such")
    n <- split(data, seq(nrow(data)))
  } else {
  #Km is in meters
  #time is in minutes
  if (type == "everyKm"){
    if(!"DistanceMeters" %in% colnames(data)){
      stop("Cannot split by distance as the column 'DistanceMeters' is not present")
    }
    nIntervals<-floor(max(data$DistanceMeters)/splitValues)+1
    splitValues <- seq(0,splitValues*nIntervals,splitValues)
    what <- "DistanceMeters"
  }
  if (type == "thisKm"){
    if(!"DistanceMeters" %in% colnames(data)){
      stop("Cannot split by distance as the column 'DistanceMeters' is not present")
    }
    if (splitValues[1] != 0){splitValues <-c(0, splitValues)}
    if (splitValues[length(splitValues)] < max(data$DistanceMeters)){
      splitValues <-c(splitValues, max(data$DistanceMeters))
    }
    what <- "DistanceMeters"
  }

  if (type == "everyMin"){
    if(!"Time" %in% colnames(data)){
      stop("Cannot split by time as the column 'Time' is not present")
    }
    nIntervals<-floor(max(data$Time)/splitValues)+1
    splitValues <- seq(0,splitValues*nIntervals,splitValues)
    what <- "Time"
  }
  if (type == "thisMin"){
    if(!"Time" %in% colnames(data)){
      stop("Cannot split by time as the column 'Time' is not present")
    }
    if (splitValues[1] != 0){splitValues <-c(0, splitValues)}
    if (splitValues[length(splitValues)] < max(data$Time)){
      splitValues <-c(splitValues, max(data$Time))
    }
    what <- "Time"
  }


  n<-doSplit(data, what, splitValues = unique(splitValues))
}
  
  names(n)<-paste("interval",seq(1,length(n),1), sep = "")
  return(n)
}
