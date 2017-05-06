#' plotCompareSplits
#' @description
#' Function to plot activity metrics comparison between splits
#' @param data (list) the list of dataframes generated with
#' gpxAnalyser::createSplits()
#' @param doFacet (logical) TRUE to plot variables separetly, FALSE to plot
#' them overlayed
#' @return
#' A ggplot object
#' @details
#' The function accepts a list of dataframes created
#' with createSplits()
#' @examples
#' # gpx <- dataLoader("path_to_activity.tcx")
#' # generateMap(gpx)
#' # sp<-createSplits(gpx, 2000, type = "everyKm")
#' # plotCompareSplits(sp)
#' @export


plotCompareSplit<-function(data, doFacet = TRUE){
  if(is.data.frame(data)){
    stop("Must be a list of splits")
  }
  summaryTable<-compareSplits(data)
  summaryTable <- summaryTable[seq(1,dim(summaryTable)[1]-1,1),]
  summaryTable$Interval<-as.numeric(summaryTable$Interval)
  summaryTable_norm<-lapply(summaryTable, normFn)
  summaryTable_norm<-as.data.frame(summaryTable_norm)
  summaryTable_norm$Interval<-summaryTable$Interval

  plottingData<-melt(summaryTable, "Interval")
  plottingData_norm<-melt(summaryTable_norm, "Interval")


  if(doFacet){
    p<-ggplot(data = plottingData,aes(x = Interval, y = as.numeric(value),
                                 group = variable, color = variable)) +
    geom_line() +
    facet_grid(variable ~ ., scales = "free")
  } else {
    p<-ggplot(data = plottingData_norm,aes(x = Interval, y = as.numeric(value),
                                      group = variable, color = variable)) +
      geom_line()

    }
  p <- p+ylab("Value")
  p

}
