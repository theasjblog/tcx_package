#' compareSplits
#' @description
#' Function to compare activity splits
#' @param data a list of dataframes generated with gpxAnalyser::createSplits()
#' @return
#' A dataframe with a summary of comparisons between metrics for the splits
#' @details
#' The function accepts a list of dataframes created with createSplits()
#' @examples
#' gpx <- intervalActivity
#' sp<-autoSplits(gpx)
#' compareSplits(sp)
#' @export

compareSplits<-function(data){

  #if only one in the list then we need a special case of the calculate function
  #the app right now will crash as the case is handled only for toSum
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
      stop("Nosummary can be displayed with this data. Check your column names")
    }


    #lengths<-unlist(lapply(data, function(x){
    #  dim(x)[1]
    #}))
    #if(sum(lengths) == length(data)){
    #  res<-lapply(data, function(x){
    #    res1<-NULL
    #    res2<-NULL
    #    res3<-NULL
    #    if (length(toDelta)>0){
    #      res1<-x[,toDelta]
    #    }
    #    if(length(toAvg)>0){
    #      res2<-x[,toAvg]
    #    }
    #    if(length(toSum)>0){
    #      res3<-x[,toSum]
    #    }

    #  })
    #} else {
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
        #if (length(toDelta)>0){
        #  res1<-apply(x[,colnames(x) %in% toDelta], 2,deltaF)
        #}
        #if (length(toAvg)>0){
        #  res2<-apply(x[,colnames(x) %in% toAvg], 2,avgF)
        #}
        #if (length(toSum)>0){
        #  res3<-c(Altitude=sumF(x[,toSum]))
        #}
        res<-c(res1, res2, res3)
      })

    #}


  summaryTable <- as.data.frame(matrix(unlist(res),nrow=length(res), byrow = TRUE))
  colnames(summaryTable) <- summaryNames

  adding<-apply(summaryTable, 2 , mean)
  adding["Time"]<-sum(summaryTable$Time)
  adding["DistanceMeters"]<-sum(summaryTable$DistanceMeters)
  summaryTable<-rbind(summaryTable, as.data.frame(t(adding)))
  Interval<-c(paste("Interval",as.character(seq(1,dim(summaryTable)[1]-1,1), sep=" ")),"Overall")
  summaryTable<-cbind(Interval, summaryTable)
  }
  return(summaryTable)
}
