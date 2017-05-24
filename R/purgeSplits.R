#' purgeSplits
#' @description
#' Function to a eliminate splits
#' @param data a list of dataframes generated with gpxAnalyser::createSplits()
#' @param splitsToEliminate (numeric vector) the indexes of the splits
#' to eliminate from the map view. Default is NULL (no split is eliminated).
#' @return
#' A list of data frames
#' @details
#' The function accepts a list of dataframes createdwith createSplits() or dataLoader()
#' @examples
#' gpx <- evenActivity
#' sp <- createSplits(gpx, 2000, type = "everyKm")
#' purgedSplits <- purgeSplits(sp, splitsToEliminate = c(1))
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
