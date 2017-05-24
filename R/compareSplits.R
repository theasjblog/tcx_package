#' compareSplits
#' @description
#' Function to compare activity splits
#' @param data a list of dataframes generated with gpxAnalyser::createSplits()
#' @param showMe (character) The metrics to include in the plot. Must be the same name as the column in the dataframe.
#' @param ftp (numeric) Functional threshold power if bike workout, pace [min/km] if run, functional threshold if using HR
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

compareSplits<-function(data, showMe, ftp = NULL){


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


    s <- data$Time[length(data$Time)]*3600/60
    np <- NULL
    if(!is.null(ftp)){
    if("Watts" %in% colnames(data)){
      np <- mean(data$Watts)
    } else if ("DistanceMeters" %in% colnames(data)){
      if("AltitudeMeters" %in% colnames(data)){
        grade <- 100 * (data$AltitudeMeters[length(data$AltitudeMeters)] - data$AltitudeMeters[1]) / data$DistanceMeters[length(data$DistanceMeters)]
        perc <- ifelse(grade > 0, 0.035, 0.18)
        if (grade == 0){perc <- 0}
        gap <- mean(data$Pace) - mean(data$Pace)*(perc*grade)
      } else {
        gap <- mean(data$Pace)
      }
      ftp <- 60/ftp
      np <- 60/gap
    } else if ("HeartRateBpm" %in% colnames(data)){
      np <- mean(data$HeartRateBpm)
    }
      }
    if(!is.null(np)){
      summaryTable$IF <- np/ftp
      summaryTable$TSS <- 100*s*(summaryTable$IF)^2/3600
    }

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

        s <- x$Time[length(x$Time)]*3600/60
        np <- NULL
        if(!is.null(ftp)){
          if("Watts" %in% colnames(x)){
            np <- mean(x$Watts)
          } else if ("DistanceMeters" %in% colnames(x)){
            if("AltitudeMeters" %in% colnames(x)){
              grade <- 100 * (x$AltitudeMeters[length(x$AltitudeMeters)] - x$AltitudeMeters[1]) / x$DistanceMeters[length(x$DistanceMeters)]
              perc <- ifelse(grade > 0, 0.035, 0.18)
              if (grade == 0){perc <- 0}
              gap <- mean(x$Pace) - mean(x$Pace)*(perc*grade)
            } else {
              gap <- mean(x$Pace)
            }
            ftp <- 60/ftp
            np <- 60/gap
          } else if ("HeartRateBpm" %in% colnames(x)){
            np <- mean(x$HeartRateBpm)
          }
        }
        if(!is.null(np)){
          IF <- np/ftp
          TSS <- 100*s*(IF)^2/3600
          res <- c(res,IF, TSS)
        }
        return(res)

      })


  summaryTable <- as.data.frame(matrix(unlist(res),nrow=length(res), byrow = TRUE))
  if(dim(summaryTable)[2] != length(summaryNames)){
    summaryNames <- c(summaryNames, "IF", "TSS")
  }
  colnames(summaryTable) <- summaryNames

  adding<-apply(summaryTable, 2 , mean)
  if("Time" %in% colnames(summaryTable)){
    adding["Time"]<-sum(summaryTable$Time)
  }
  if("DistanceMeters" %in% colnames(summaryTable)){
    adding["DistanceMeters"]<-sum(summaryTable$DistanceMeters)
  }
  if("TSS" %in% colnames(summaryTable)){
    adding["TSS"]<-sum(summaryTable$TSS)
  }
  summaryTable<-rbind(summaryTable, as.data.frame(t(adding)))
  Interval<-c(paste("Interval",as.character(seq(1,dim(summaryTable)[1]-1,1), sep=" ")),"Overall")
  summaryTable<-cbind(Interval, summaryTable)
  }


  return(summaryTable)
}
