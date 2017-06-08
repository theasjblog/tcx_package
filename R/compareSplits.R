#' compareSplits
#' @description
#' Function to compare activity splits
#' @param data a list of dataframes generated with gpxAnalyser::createSplits()
#' @param showMe (character) The metrics to include in the plot. Must be the same name as the column in the dataframe.
#' @param ftp (numeric) Functional threshold power if bike workout, pace [min/km] if run, functional threshold if using HR
#' @param ftpType (character) One os 'power', 'pace' or 'HR', to describe what kind of FTP
#' was provided
#' @return
#' A dataframe with a summary of comparisons between metrics for the splits
#' @details
#' The function accepts a list of dataframes created with createSplits()
#' @examples
#' gpx <- intervalActivity
#' compareSplits(gpx)
#' sp<-autoSplits(gpx)
#' compareSplits(sp)
#' @export

compareSplits<-function(data, showMe = NULL, ftp = NULL,
                        ftpType = c("power", "pace", "HR")[1]){




  if (is.data.frame(data)){
    if (is.null(showMe)){
      showMe <- colnames(data)
    }
    idx<-which(colnames(data) %in% showMe)
    data<-data[,idx]
  } else {
    if (is.null(showMe)){
      showMe <- colnames(data[[1]])
    }
    idx<-which(colnames(data[[1]]) %in% showMe)
    for (i in 1:length(data)){
      data[[i]]<-data[[i]][,idx]
    }
  }

  toDelta <- c("Time", "Distance")
  toAvg<-c("Heart rate", "Pace", "Speed", "Power", "Cadence")
  toSum<- c("Elevation gain")



  if (is.data.frame(data)){

    filtering <- selectDeltaSumAvg(df = data, toDelta = toDelta, toAvg = toAvg, toSum = toSum)
    toDelta <- filtering$toDelta
    toSum <- filtering$toSum
    toAvg <- filtering$toAvg
    summaryNames <- filtering$summaryNames

    res <- computeDeltaAvgSum(df = data, toDelta = toDelta, toSum = toSum, toAvg = toAvg)
    summaryTable<-as.data.frame(t(res))
    colnames(summaryTable) <- summaryNames

    if(!is.null(ftp)){
      s <- data$Time[length(data$Time)]*3600/60
      ifs <- computeNp(df = data, ftp = ftp, ftpType = ftpType)


      if ("Distance" %in% colnames(data) & ftpType == "pace"){

        TSSs <- 100*30*(ifs)^2/3600
        summaryTable$IF <- mean(ifs)
        summaryTable$TSS <- sum(TSSs)
      } else {
        if(!is.null(ifs)){
          TSSs <- 100*30*(ifs)^2/3600
          summaryTable$IF <- mean(ifs)
          summaryTable$TSS <- sum(TSSs)

        }
      }
    }

  } else {

    filtering <- selectDeltaSumAvg(df = data[[1]], toDelta = toDelta, toAvg = toAvg, toSum = toSum)
    toDelta <- filtering$toDelta
    toSum <- filtering$toSum
    toAvg <- filtering$toAvg
    summaryNames <- filtering$summaryNames


    res<-lapply(data, function(x){

      res <- computeDeltaAvgSum(df = x, toDelta = toDelta, toSum = toSum, toAvg = toAvg)

        if(!is.null(ftp)){
          s <- x$Time[length(x$Time)]*3600/60
          ifs <- computeNp(df = x, ftp = ftp, ftpType = ftpType)

        if(!is.null(ifs)){
          if ("Distance" %in% colnames(x) & ftpType == "pace"){

          }
          IF <- mean(ifs)
          TSS <- 100*30*(ifs)^2/3600
          res <- c(res,IF, sum(TSS))
        }}
        return(res)

      })


  summaryTable <- as.data.frame(matrix(unlist(res),nrow=length(res), byrow = TRUE))
  if(dim(summaryTable)[2] != length(summaryNames)){
    summaryNames <- c(summaryNames, "IF", "TSS")
  }
  colnames(summaryTable) <- summaryNames

  temp <- rbindlist(data)
  adding <- apply(temp, 2 , mean)
  idx <- which(names(adding) %in% colnames(summaryTable))
  adding <- adding[idx]


  if("Time" %in% colnames(summaryTable)){
    adding["Time"]<-sum(summaryTable$Time)
  }
  if("Distance" %in% colnames(summaryTable)){
    adding["Distance"]<-sum(summaryTable$Distance)
  }
  if("TSS" %in% colnames(summaryTable)){
    adding["TSS"]<-sum(summaryTable$TSS)
  }
  if("IF" %in% colnames(summaryTable)){
    adding["IF"]<-mean(summaryTable$IF)
  }
  summaryTable<-rbind(summaryTable, as.data.frame(t(adding)))
  Interval<-c(paste("Interval",as.character(seq(1,dim(summaryTable)[1]-1,1), sep=" ")),"Overall")
  summaryTable<-cbind(Interval, summaryTable)
  }


  return(summaryTable)
}
