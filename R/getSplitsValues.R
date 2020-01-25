#' @title getSplitsValues
#' @description
#' Function to detSplitsValues
#' @param gpx (data frame) a dataframe generated with gpxAnalyser::dataLoader()
#' @param sp (list) the list of dataframe representing the splits
#' @param xVariable (character) the variable to put on the x axis. One of 'DistanceMeters'
#' or 'Time'
#' @return
#' A numeric vector
#' @details
#' the function gives the splits intital time or distance, needed to plot the splits on the metrics plot
#' @examples
#' gpx <- intervalActivity
#' sp <- autoSplits(gpx)
#' splitsValues <- getSplitsValues(gpx, sp, "Time")
#' @export
getSplitsValues <- function(gpx, sp, xVariable){
  splitsIdx <- unlist(lapply(sp, function(x){
    x$OriginalIdx[1]
  }))
  splitsIdx <- splitsIdx[-1]
  if (xVariable == "Time"){
    splitsValues <- gpx$Time[splitsIdx]
  } else if(xVariable == "DistanceMeters"){
    splitsValues <- gpx$DistanceMeters[splitsIdx]
  }
  return(splitsValues)
}
