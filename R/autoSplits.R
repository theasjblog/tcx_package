#' autoSplits
#' @description
#' Function to automatically detect splits recorded in the activity.
#' @param data an activity dataframe
#' @return
#' A list of dataframes, one for each split
#' @details
#' Sometimes intervals are recorded directly in the activity, for instance when pressing the Lap button on a
#' Garmin device. This function will identify such splits, if any is present. The splits are identified as
#' repeated rows in the data frame.
#' @examples
#' gpx <- intervalActivity
#' autoSp <- autoSplits(gpx)
#' @export

autoSplits<-function(data){
  if (!is.data.frame(data)){
    stop("Data must be a data frame")
  }
  if (!"Time" %in% colnames(data)){
    "The column 'Time' is not in the data!"
  }


  idx<-data$Time[diff(data$Time)==0]
  if (length(idx)==0){
      stop("No watch provided splits")
  } else {
    splitValues <- idx
  }
  res<-createSplits(data,splitValues, "thisMin")

  return(res)
}
