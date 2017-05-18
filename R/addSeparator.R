#' addSeparator
#' @description
#' Function to add a split separator
#' @param data a dataframe generated with gpxAnalyser::dataLoader()
#' @param splitHere (numeric) vector for where to split
#' @return
#' A list of dataframes, one for each split
#' @details
#' The function accepts a data frame created with dataLoader(). The function does not return a list of splits. It is
#' intended to be used as a tool to identify the best positions for the splits, which can be generated later on using
#' createSplits(). The function uses a plot to help with the task. The plot is a representation of the activity
#' time vs distance. When the lines gets flatter, the speed has decreased. That is likely to be the beginning of a rest
#' interval. The proposed split times should intercept the plot line where it starts to get flat and where it increases
#' the slope again, marking the beginning end the end of each rest/active interval. If there is no rest interval, but simply
#' variations in speed, then the same logic holds: make the proposed split time to intercept the points where the activity
#' line changes slope.
#' @examples
#' gpx <- intervalActivity
#' proposedSplits<-c(7,15.5,17,25.5,27,35.5,37,45.5,47,56)
#' addSeparator(gpx, proposedSplits)
#' sp<-createSplits(gpx, proposedSplits, "thisMin")
#' @export

#split on time only?
addSeparator<-function(data, splitHere){
  if (!is.data.frame(data)){
    stop("data must be a data frame")
  }
  
  #needed to pass R CMD Check
  Time <- NULL
  DistanceMeters <- NULL
  
  p<-ggplot(data, aes(Time, DistanceMeters, color = "red"))+geom_line()+geom_vline(xintercept = splitHere, colour = "blue")
  print(p)
}