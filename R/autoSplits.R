#' splitsFinder
#' @description
#' Function to automatically detect splits. If the splits were recorded by the Garmin device, the function
#' will find and return those
#' @param data a dataframe generated with gpxAnalyser::dataLoader()
#' @param span (numeric) a numeric value. The smaller the values, the more splits you get (normally)
#' Use columns = 'all' to plot all the metrics
#' @return
#' A list of dataframes, one for each split
#' @details
#' The function accepts a data frame created with dataLoader()
#' @examples
#' gpx <- intervalActivity
#' autoSp <- autoSplits(gpx) #to use Garmin splits
#' # If the activity had no Garmin splits:
#' # autoSp <- autoSplits(gpx, span = 25) 
#' @export

autoSplits<-function(data, span=NULL){
  if (!is.data.frame(data)){
    stop("Data must be a data frame")
  }
  
  idx<-data$Time[diff(data$Time)==0]
  if (length(idx)==0){
  if (is.null(span)){
    stop("No watch provided splits. Must use the span argument")
  }
  xz<-data$DistanceMeters/data$Time
  xz<-as.zoo(xz[-1])
  isPeak<-rollapply(xz,span,function(x) which.min(x)==span)
  isPeak<-which(isPeak)
  
  n<-data.frame(b=isPeak,
                diff=c(0,diff(isPeak)),
                time=data$Time[isPeak],
                dist=data$DistanceMeters[isPeak]
                )
  n$diffTime <- c(0,diff(n$time))
  
  idx<-which(n$diffTime>1)
  idx<-sort(c(idx,idx-1))
  splitValues<-n$time[idx]
  xInt <- n$time[idx]
  }
  else {
    splitValues <- idx
    xInt <- idx
  }
  res<-createSplits(data,splitValues, "thisMin")
  
  ##testing plot hidden for now, might resume later for diagnostic
  #x<-data$Time
  #y<-data$DistanceMeters
  #p<-ggplot(data, aes(Time, DistanceMeters))+geom_line()+geom_vline(xintercept = xInt)
  #print(p)
  return(res)
}
