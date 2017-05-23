#' compareSplits
#' @description
#' Function to compare activity splits
#' @param data a list of dataframes generated with gpxAnalyser::createSplits()
#' @param showMe (character) The metrics to include in the plot. Must be the same name as the column in the dataframe.
#' @return
#' A dataframe with a summary of comparisons between metrics for the splits
#' @details
#' The function accepts a list of dataframes created with createSplits()
#' @examples
#' gpx <- intervalActivity
#' compareSplits(gpx, showMe = colnames(gpx))
#' sp<-autoSplits(gpx)
#' compareSplits(sp, showMe = colnames(sp[[1]]))
#' @export

compareSplits<-function(data, showMe){
  
  
  if (is.data.frame(data)){
    idx<-which(colnames(data) %in% showMe)
    data<-data[,idx]
  } else {
    idx<-which(colnames(data[[1]]) %in% showMe)
    for (i in 1:length(data)){
      data[[i]]<-data[[i]][,idx]
    }
  }

  toDelta <- c("Time", "DistanceMeters")
  toAvg<-c("HeartRateBpm", "Pace", "Speed", "Watts", "Cadence")
  toSum<- c("AltitudeMetersDiff")


  res1 <- NULL
  res2 <- NULL
  res3 <- NULL
  if (is.data.frame(data)){
    colRead <- colnames(data)

    idx<-which(!toDelta %in% colRead)
    if(length(idx)>0){toDelta<-toDelta[-c(idx)]}
    idx<-which(!toAvg %in% colRead)
    if(length(idx)>0){toAvg<-toAvg[-c(idx)]}
    idx<-which(!toSum %in% colRead)
    if(length(idx)>0){toSum<-toSum[-c(idx)]}

    summaryNames <- c(toDelta, toAvg, toSum)
    if(length(summaryNames)<1){
      stop("No summary can be displayed with this data. Check your column names")
    }

    if (length(toDelta)>0){
      for (i in 1:length(toDelta)){
        res1<-c(res1,deltaF(data[,toDelta[i]]))
      }
    }
    if(length(toAvg)>0){
      for (i in 1:length(toAvg)){
        res2<-c(res2,avgF(data[,toAvg[i]]))
      }
    }
    if(length(toSum)>0){
      for (i in 1:length(toSum)){
        res3<-c(res3,sumF(data[,toSum[i]]))
      }
    }

    res<-c(res1, res2, res3)
    summaryTable<-as.data.frame(t(res))
    colnames(summaryTable) <- summaryNames
  } else {

    colRead <- colnames(data[[1]])
    idx<-which(!toDelta %in% colRead)
    if(length(idx)>0){toDelta<-toDelta[-c(idx)]}
    idx<-which(!toAvg %in% colRead)
    if(length(idx)>0){toAvg<-toAvg[-c(idx)]}
    idx<-which(!toSum %in% colRead)
    if(length(idx)>0){toSum<-toSum[-c(idx)]}

    summaryNames <- c(toDelta, toAvg, toSum)
    if(length(summaryNames)<1){
      stop("No summary can be displayed with this data. Check your column names")
    }


      res<-lapply(data, function(x){
        res1<-NULL
        res2<-NULL
        res3<-NULL
        if (length(toDelta)>0){
          for (i in 1:length(toDelta)){
            res1<-c(res1,deltaF(x[,toDelta[i]]))
          }
        }
        if(length(toAvg)>0){
          for (i in 1:length(toAvg)){
            res2<-c(res2,avgF(x[,toAvg[i]]))
          }
        }
        if(length(toSum)>0){
          for (i in 1:length(toSum)){
            res3<-c(res3,sumF(x[,toSum[i]]))
          }
        }
        res<-c(res1, res2, res3)
      })



  summaryTable <- as.data.frame(matrix(unlist(res),nrow=length(res), byrow = TRUE))
  colnames(summaryTable) <- summaryNames

  adding<-apply(summaryTable, 2 , mean)
  if("Time" %in% colnames(summaryTable)){
    adding["Time"]<-sum(summaryTable$Time)
  }
  if("DistanceMeters" %in% colnames(summaryTable)){
    adding["DistanceMeters"]<-sum(summaryTable$DistanceMeters)
  }
  summaryTable<-rbind(summaryTable, as.data.frame(t(adding)))
  Interval<-c(paste("Interval",as.character(seq(1,dim(summaryTable)[1]-1,1), sep=" ")),"Overall")
  summaryTable<-cbind(Interval, summaryTable)
  }
  return(summaryTable)
}
