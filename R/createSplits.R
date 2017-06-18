#' createSplits
#' @description
#' Function to split an activity into custom splits
#' @param data (dataframe) an activity dataframe
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
#' The function uses the provided split values and split type to divide the activity dataframe in a list of dataframes,
#' each one corresponding to a split starting and finishing at the specified times/distances.
#' @examples
#' gpx <- evenActivity
#' sp<-createSplits(gpx, 2000, type = "everyKm")
#' sp<-createSplits(gpx, c(1000, 3000, 4500), type = "thisKm")
#' sp<-createSplits(gpx, 2, type = "everyMin")
#' sp<-createSplits(gpx, c(5, 15, 35), type = "thisMin")
#' @export

createSplits <- function (data, splitValues,
                          type = c("everyKm","thisKm", "everyMin", "thisMin")[1]){


  if (type == "everyKm"){
    if(!"Distance" %in% colnames(data)){
      stop("Cannot split by distance as the column 'Distance' is not present")
    }
    nIntervals<-floor(max(data$Distance)/splitValues)+1
    splitValues <- seq(0,splitValues*nIntervals,splitValues)
    what <- "Distance"
  }
  if (type == "thisKm"){
    if(!"Distance" %in% colnames(data)){
      stop("Cannot split by distance as the column 'Distance' is not present")
    }
    if (splitValues[1] != 0){splitValues <-c(0, splitValues)}
    if (splitValues[length(splitValues)] < max(data$Distance)){
      splitValues <-c(splitValues, max(data$Distance))
    }
    what <- "Distance"
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
#}

  names(n)<-paste("interval",seq(1,length(n),1), sep = "")
  if(length(n)>1){
    for (i in 2:length(n)){
      if ("LatitudeDegrees" %in% colnames(n[[1]])){
        if(length(n[[i]]$LatitudeDegrees)>1){
          n[[i]]$LatitudeDegrees[1] <- n[[i-1]]$LatitudeDegrees[length(n[[i-1]]$LatitudeDegrees)]
          n[[i]]$LongitudeDegrees[1] <- n[[i-1]]$LongitudeDegrees[length(n[[i-1]]$LongitudeDegrees)]
        }
      }
    }
  }

  return(n)
}
