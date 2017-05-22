#' splitsFinder
#' @description
#' Function to automatically detect splits. If the splits were recorded by the Garmin device, the function
#' will find and return those
#' @param data a dataframe generated with gpxAnalyser::dataLoader()
#' @param span (numeric) a numeric value. The smaller the values, the more splits you get (normally)
#' @param yVariable (numeric) the variable (column name) to put on the y axis
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

autoSplits<-function(data, span=NULL, yVariable="DistanceMeters"){
  if (!is.data.frame(data)){
    stop("Data must be a data frame")
  }
  if (!"Time" %in% colnames(data)){
    "The column 'Time' is not in the data!"
  }
  
  
  idx<-data$Time[diff(data$Time)==0]
  if (length(idx)==0){
    if (is.null(span)){
      stop("No watch provided splits. Must use the span argument")
    }
    if (!is.null(yVariable) & (length(yVariable)>1 | !is.character(yVariable) | !yVariable %in% colnames(data))){
      "yvariable must be a character vector of length 1 and must be a column name available in data"
    }
  
    splitData<-data.frame(Time = data$Time,
                        y = data[, yVariable])
  
  
  xz<-splitData$y/splitData$Time
  xz<-as.zoo(xz[-1])
  isPeak<-rollapply(xz,span,function(x) which.min(x)==span)
  isPeak<-which(isPeak)
  if(length(isPeak) == 0){
    stop("Cannot find splits. Try to change span or yVariable")
  }
  n<-data.frame(b=isPeak,
                diff=c(0,diff(isPeak)),
                time=data$Time[isPeak],
                dist=splitData$y[isPeak]
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
