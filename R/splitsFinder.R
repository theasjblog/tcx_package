#' splitsFinder
#' @description
#' Function to automatically detect splits
#' @param data a dataframe generated with gpxAnalyser::dataLoader()
#' @param w (numeric) w should be << than half of the width of data. small values gets tiny bumps
#' @param span (numeric) span 0 to 1. larger makes bump go away
#' Use columns = 'all' to plot all the metrics
#' @return
#' A list of dataframes, one for each split
#' @details
#' The function accepts a data frame created with dataLoader()
#' @examples
#' # gpx <- dataLoader("path_to_activity.tcx")
#' # generateMap(gpx)
#' # autoSp<-autoSplits(gpx, 1, 1.1)
#' @export

autoSplits<-function(data, w, span){
  if (!is.data.frame(data)){
    stop("Data must be a data frame")
  }
  y<-data$Time
  x<-data$DistanceMeters
  a<-argmax(x, y, w = w, span = span)

  y.min<-min(y)
  plot(x,y, cex = 0.75, col = "Gray")
  lines(x,a$y.hat, lwd = 2)
  sapply(a$i, function(i) lines(c(x[i], x[i]), c(y.min, a$y.hat[i]),
                                col = "Red", lty = 2))
  points(x[a$i], a$y.hat[a$i], col = "Red", pch = 19, cex = 1.25)
  
  aa<<-x[a$i]

  res<-createSplits(data, x[a$i], "thisKm")

  return(res)
}
