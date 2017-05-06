#' compareSplits
#' @description
#' Function to compare activity splits
#' @param data a list of dataframes generated with gpxAnalyser::createSplits()
#' @return
#' A dataframe with a summary of comparisons between metrics for the splits
#' @details
#' The function accepts a list of dataframes created with createSplits()
#' @examples
#' # gpx <- dataLoader("path_to_activity.tcx")
#' # sp<-createSplits(gpx, 2000, type = "everyKm")
#' # compare<-createSplits(sp)
#' @export

compareSplits<-function(data){

  toDelta <- c("Time", "DistanceMeters")
  toSum<- c("AltitudeMetersDiff")
  toAvg<-c("HeartRateBpm", "Pace", "Speed")
  if (is.data.frame(data)){
    res1<-apply(data[,colnames(data) %in% toDelta], 2,deltaF)
    res2<-apply(data[,colnames(data) %in% toAvg], 2,avgF)
    res3<-c(Altitude=sumF(data[,toSum]))
    res<-c(res1, res2, res3)
    summaryTable<-as.data.frame(t(res))
    colnames(summaryTable)<-c("Time", "Distance","HR",
                              "Altitude", "Pace", "Speed")
  } else {

  res<-lapply(data, function(x){
    res1<-apply(x[,colnames(x) %in% toDelta], 2,deltaF)
    res2<-apply(x[,colnames(x) %in% toAvg], 2,avgF)
    res3<-c(Altitude=sumF(x[,toSum]))
    res<-c(res1, res2, res3)
  })

  summaryTable <- rbindlist(
    lapply(res, FUN = getSummaryVect)
  )
  adding<-apply(summaryTable, 2 , mean)
  adding["Time"]<-sum(summaryTable$Time)
  adding["Distance"]<-sum(summaryTable$Distance)
  summaryTable<-rbind(summaryTable, as.data.frame(t(adding)))
  Interval<-c(as.character(seq(1,dim(summaryTable)[1]-1,1)),"Overall")
  summaryTable<-cbind(Interval, summaryTable)
  }
  return(summaryTable)
}
