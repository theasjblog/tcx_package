#' splitsFinder
#' @description
#' Function to automatically detect splits. If the splits were recorded by the Garmin device, the function
#' will find and return those
#' @param data a dataframe generated with gpxAnalyser::dataLoader()
#' @return
#' A list of dataframes, one for each split
#' @details
#' The function accepts a data frame created with dataLoader()
#' @examples
#' gpx <- intervalActivity
#' autoSp <- autoSplits(gpx) #to use Garmin splits
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
