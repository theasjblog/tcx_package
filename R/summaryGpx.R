#' summaryGpx
#' @description
#' Function to a map of the activity
#' @param data a dataframe generated with gpxAnalyser::dataLoader() or the
#' list of dataframes generated with gpxAnalyser::createSplits()
#' @return
#' A dataframe summarising activity metrics
#' @details
#' The function accepts a list of dataframes or a data frame created
#' with createSplits() or dataLoader()
#' @examples
#' # gpx <- dataLoader("path_to_activity.tcx")
#' # sp<-createSplits(gpx, 2000, type = "everyKm")
#' # overview<-summaryGpx(gpx)
#' # overview<-summaryGpx(sp)
#' @export

summaryGpx <- function(data){

  if (class(data) == "data.frame"){
    summaryTable <- getSummary(data)
  } else {
    summaryTable <- rbindlist(
      lapply(data, FUN = getSummary)
    )
    col<-colnames(summaryTable)
    col<-c("Interval", col)
    summaryTable<-cbind(seq(1,dim(summaryTable)[1],1), summaryTable)
    class(summaryTable)<-"data.frame"
    colnames(summaryTable)<-col
    adding<-apply(summaryTable, 2 , mean)
    if ("Distance" %in% colnames(summaryTable)){
      temp <- modifyColumns(summaryTable, adding, "Distance")
    }
    if ("Time" %in% colnames(summaryTable)){
      temp <- modifyColumns(summaryTable, adding, "Time")
    }
    summaryTable<- temp$summaryTable
    adding<- temp$adding
    adding$Interval <- "Overall"
    summaryTable <- rbind(summaryTable, adding)
  }

  return(summaryTable)
}
