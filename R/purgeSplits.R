#' purgeSplits
#' @description
#' Function to a eliminate splits
#' @param data a list of dataframes for the activity splits
#' @param splitsToEliminate (numeric vector) the indexes of the splits
#' to eliminate from the map view. Default is NULL (no split is eliminated).
#' @return
#' A list of data frames
#' @details
#' This function allows to eliminate splits from the views and the summary tables. This can be useful when an activity includes
#' rest intervals, and we are interest in metrics that would be biased by the low intensity rest interval (i.e. total intervals
#' time for a 5X1Km session).
#' @examples
#' gpx <- intervalActivity
#' sp <- autoSplits(gpx)
#' purgedSplits <- purgeSplits(sp, splitsToEliminate = c(1,3,5,7,9,11,12))
#' @export

purgeSplits<-function(data, splitsToEliminate = NULL){
if(!is.list(data)){
  stop("data must be a list")
}
if(max(splitsToEliminate)> length(data)){
  stop("maxim number of splits exceeded")
}
namesList<-names(data)
namesList<-namesList[splitsToEliminate]
data <- data[ !names(data) %in% namesList]
return(data)
}
