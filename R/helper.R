deltaF<-function(x){if (max(x) != min(x)){max(x)-min(x)}else{x}}
avgF<-function(x){mean(x[x!=0])}
sumF<-function(x){sum(x)}


melting<-function (data, xVariable){

  variable<-NULL

  columns<-colnames(data)

  data[xVariable]<-data[xVariable]-min(data[xVariable])
  xymelt <- melt(data, id.vars = xVariable)

  selection <- xymelt %>% filter(variable %in% columns)
  selection$variable<-as.character(selection$variable)
  a<-split(selection, selection$variable)
  a<-lapply(a, function(x){
    x$value[is.nan(x$value)]  <-  min(x$value[!is.nan(x$value) & !is.infinite(x$value)])
    x$value[is.infinite(x$value)]  <-  max(x$value[!is.nan(x$value) & !is.infinite(x$value)])
    x$value_norm<-rescale(as.numeric(x$value),c(0,100))
    x$value <- as.numeric(x$value)
    x$variable<-x$variable
    x[xVariable]<-x[xVariable]
    return(x)
  })

  selection<-setDF(rbindlist(a))
}


doSplit <- function(data, what, splitValues){
  data$OriginalIdx <- seq(1,dim(data)[1],1)
  n<-split(data, cut(data[,what], splitValues, include.lowest=TRUE))
  b <- unlist(lapply(n,function(x){length(x$Time)}))
  idx <- which(b == 0)
  if (length(idx) >0){
    n <- n[-idx]
  }

  if (length(n)>1){
    for (i in 2:length(n)){
      n[[i]] <- rbind(n[[i-1]][length(n[[i-1]]$Time),],n[[i]])
    }
  }


  n<-lapply(n,function(x){
    if ("Time" %in% colnames(x)){
      if(length(x$Time)>1){
        x$Time<-x$Time-x$Time[1]
      }
    }
    if ("Distance" %in% colnames(x)){
      if(length(x$Distance)>1){
        x$Distance <- x$Distance-x$Distance[1]
      }
    }
    if ("Pace" %in% colnames(x)){
      x$Pace<-1000*x$Time/x$Distance
      if(length(x$Time)>1){
        x$Pace[1]<-0
      }
      x$Pace[is.na(x$Pace)] <- 0
      x$Pace[is.infinite(x$Pace)] <- 0
    }
    if ("Speed" %in% colnames(x)){
      x$Speed<-0.06*x$Distance/x$Time
      if(length(x$Time)>1){
        x$Speed[1]<-0
      }
      x$Speed[is.na(x$Speed)] <- 0
      x$Speed[is.infinite(x$Speed)] <- 0
    }
    return(x)
    })

  return(n)

}


normFn<-function(x){
  rescale(x,c(0,100))
}

percFn<-function(x){
  100*((x-mean(x))/mean(x))
}


dealMissingPoints <- function(data, issues, nds){

  for (ii in 1:length(issues)){
    n<-lapply(nds,function(x){names(x)})

    v<-unlist(lapply(seq_along(n),function(i){
      if (issues[[ii]] %in% n[[i]]){
        i
      } else {
        NA
      }
    }))
    idx<-length(which(!is.na(v)))

    if (idx == 0){
      n<-lapply(nds,function(x){names(x$Extensions$TPX)})

      v<-unlist(lapply(seq_along(n),function(i){
        if (issues[[ii]] %in% n[[i]]){
          i
        } else {
          NA
        }
      }))
    }
    idx<-length(which(!is.na(v)))

    if (idx == 0){
      n<-lapply(nds,function(x){names(x$Position)})

      v<-unlist(lapply(seq_along(n),function(i){
        if (issues[[ii]] %in% n[[i]]){
          i
        } else {
          NA
        }
      }))
    }

    haveit <- which(!is.na(v))
    temp<-data[[issues[ii]]]
    data[[issues[ii]]]<-v
    data[[issues[ii]]][haveit]<-temp

  }
  return(data)
}


interpolateMissing <-function(data){

  missing<-which(is.na(data) | data==0)
  if (length(missing)!=length(data)){
  haveit<-which(!is.na(data) & data!=0)
  if (length(missing) > 0){
    if (missing[1]==1){
      data[1]<-data[haveit[1]]
    }
    if (missing[length(missing)]==length(data)){
      data[length(data)]<-data[haveit[length(haveit)]]
    }
    missing<-which(is.na(data) | data==0)
    if(length(missing)>0){
    haveit<-which(!is.na(data) & data!=0)
    m<-diff(missing)
    end<-which(m>1)
    end<-missing[end]
    end<-c(end,missing[length(missing)])
    start<-1+which(m>1)
    start <- missing[start]
    start<-c(missing[1],start)
    for (i in 1:length(start)){
      data[seq(start[i], end[i],1)] <- seq(from = data[start[i]-1],
                                           to = data[end[i]+1],
                                           length = (end[i]-start[i])+1)
    }
  }
  }
  }
  return(data)
}

getSplitsValues <- function(gpx, sp, xVariable){
  splitsIdx <- unlist(lapply(sp, function(x){
    c(x$OriginalIdx[1],x$OriginalIdx[length(x$OriginalIdx)])
  }))
  splitsIdx <- unique(splitsIdx)
  splitsIdx <- splitsIdx[-1]
  if (xVariable == "Time"){
    splitsValues <- gpx$Time[splitsIdx]
  } else if(xVariable == "Distance"){
    splitsValues <- gpx$Distance[splitsIdx]
  }
  return(splitsValues)
}


notToHide <- function(sp){
  res <- rbindlist(sp)
  res <- as.vector(res$OriginalIdx)
}

displayNames <- function(x){
  switch(x,
         HeartRateBpm = "Heart rate",
         AltitudeMeters = "Elevation",
         AltitudeMetersDiff = "Elevation gain",
         Watts = "Power",
         DistanceMeters = "Distance",
         x)
}


computeNp <- function(df, ftp, ftpType){
  ifs <- NULL
  if("Power" %in% colnames(df) & ftpType == "power"){
    temp <- createSplits(df, 0.5, "everyMin")
    means <- unlist(lapply(temp, function(b){mean(b$Power)}))
    means <- means[!is.na(means)]
    means <- means[!is.infinite(means)]
    ifs <- means/ftp

  } else if ("Distance" %in% colnames(df) & ftpType == "pace"){
    if("Elevation" %in% colnames(df)){

      temp <- createSplits(df, 0.5, "everyMin")
      means <- unlist(lapply(temp, function(b){
        if(length(b$Distance) >= 5){
          b$Distance <- seq(from = 0,
                            to = max(b$Distance),
                            len = length(b$Distance))
          b$Time <- seq(from = 0,
                            to = max(b$Time),
                            len = length(b$Time))

          b$Pace <- 1000*b$Time/b$Distance
          b$Speed <- 0.06*b$Distance/b$Time
          b$Pace[1] <- 0
          b$Speed[1] <- 0
          grade <- 100 * (b$Elevation[length(b$Elevation)] - b$Elevation[1]) / b$Distance[length(b$Distance)]
          perc <- ifelse(grade > 0, 0.035, 0.018)
          if (grade == 0){perc <- 0}
          gap <- b$Pace - b$Pace*(perc*grade)
        } else {
          gap <- NA
        }
        return(mean(gap))
        }))
      means <- means[!is.na(means)]
      means <- means[!is.infinite(means)]
      ifs <- ftp/means



    } else {

      temp <- createSplits(df, 0.5, "everyMin")
      means <- unlist(lapply(temp, function(b){mean(b$Pace[b$Pace!=0])}))
      means <- means[!is.na(means)]
      means <- means[!is.infinite(means)]
      ifs <- ftp/means

    }



  } else if ("Heart rate" %in% colnames(df) & ftpType == "HR"){
    temp <- createSplits(df, 0.5, "everyMin")
    means <- unlist(lapply(temp, function(b){mean(b[,"Heart rate"])}))
    means <- means[!is.na(means)]
    means <- means[!is.infinite(means)]
    ifs <- means/ftp

  }
  if(!is.null(ifs)){
    ifs <- ifs[!is.na(ifs)]
    ifs <- ifs[!is.infinite(ifs)]
  }

  return(ifs)
}

selectDeltaSumAvg <- function(df, toDelta, toAvg, toSum){
  colRead <- colnames(df)
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

  return(list(toDelta = toDelta,
              toAvg = toAvg,
              toSum = toSum,
              summaryNames = summaryNames))

}


computeDeltaAvgSum <- function(df, toDelta, toSum, toAvg){
  res1 <- NULL
  res2 <- NULL
  res3 <- NULL
  if (length(toDelta)>0){
    for (i in 1:length(toDelta)){
      res1<-c(res1,deltaF(df[,toDelta[i]]))
    }
  }
  if(length(toAvg)>0){
    for (i in 1:length(toAvg)){
      res2<-c(res2,avgF(df[,toAvg[i]]))
    }
  }
  if(length(toSum)>0){
    for (i in 1:length(toSum)){
      res3<-c(res3,sumF(df[,toSum[i]]))
    }
  }

  res<-c(res1, res2, res3)
  return(res)
}


fitLm <- function(x, y){
  y <- rescale(y, c(0,100))
  ff <- lm(y ~ x)
  ff <- ff$coefficients[2]
  return(ff)
}


computeDecoupling <- function(data, variable, from, to){
  divide <- mean(c(from, to))
  sp <- createSplits(data, c(from, divide, to), "thisMin")
  intervals <- length(sp)
  if(intervals >=4){
    sp <- purgeSplits(sp, c(1,length(sp)))
  } else if(intervals ==2) {
    sp <- sp
  } else if (intervals == 3){
    if (from == 0){
      sp <- purgeSplits(sp, length(sp))
    } else {
      sp <- purgeSplits(sp, 1)
    }
  }

  allHR <- c(sp[[1]][,"Heart rate"],sp[[2]][,"Heart rate"])
  allVar <- c(sp[[1]][,variable],sp[[2]][,variable])
  allTime <- c(sp[[1]][,"Time"],sp[[2]][,"Time"])
  HR1 <- sp[[1]][,"Heart rate"]
  HR2 <- sp[[2]][,"Heart rate"]
  var1 <- sp[[1]][,variable]
  var2 <- sp[[2]][,variable]
  time1 <- sp[[1]][,"Time"]
  time2 <- sp[[2]][,"Time"]

  compareHR  <-  data.frame(Overall = mean(allHR),
                            First = mean(HR1),
                            Second = mean(HR2),
                            slope_first = fitLm(time1, HR1),
                            slope_second = fitLm(time2, HR2))
  row.names(compareHR) <- ""

  compareVar  <-  data.frame(Overall = mean(allVar),
                             First = mean(var1),
                             Second = mean(var2),
                             slope_first = fitLm(time1, var1),
                             slope_second = fitLm(time2, var2))
  row.names(compareVar) <- ""

  compareSlopes  <-  data.frame(HR = fitLm(allTime, allHR),
                                Selected_variable = fitLm(allTime, allVar))
  row.names(compareSlopes) <- ""


  return(list(compareHR = compareHR,
              compareVar = compareVar,
              compareSlopes = compareSlopes))

}


plotDecoupling <- function(data, variable, from, to){
  divide <- mean(c(from, to))
  data <- data[,c("Time", "Heart rate", variable)]
  time <- data$Time
  data[data$Time<from | data$Time > to,] <- NA

  data$Time <- time
  data <- melt(data, id.vars = "Time")
  value <- NULL
  p <- ggplot(data, aes(x = data$Time, y = as.numeric(value),
                        group = variable, color = variable)) +
    geom_line() +
    geom_vline(xintercept = divide) +
    xlim(from,to) +
    xlab("Time") +
    ylab("Value") +
    facet_grid(variable ~ ., scales = "free") +
    theme_bw() +
    theme(legend.position='none')


  print(p)
  return(p)
}

