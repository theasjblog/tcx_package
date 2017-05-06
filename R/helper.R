split_coord <- function(mystring){
  pipi<-unique(unlist(str_locate_all(mystring,"\\+")))
  me<-unique(unlist(str_locate_all(mystring,"\\-")))
  ee<-unique(unlist(str_locate_all(mystring,"E")))
  pime<-sort(c(pipi,me,ee))
  di<-diff(pime)
  if (any(di==1)){
    b<-which(di==1)
    re<-c(b,b+1)
    pime<-pime[-c(re)]
  }
  if (length(pime) <1){
    po<-unique(unlist(str_locate_all(mystring,"\\.")))[2]
    lat<-as.numeric(str_sub(mystring,1,po-1))
    lon<-as.numeric(str_sub(mystring,po))
  } else {

    if (pime[1]==1){
      pime<-pime[-c(1)]
    }
    lat <-as.numeric(str_sub(mystring,1,pime[1]-1))
    lon<-as.numeric(str_sub(mystring,pime[1]))
  }
  return(list(lat = lat,
              lon = lon
  )
  )
}



removeColumns <-function(data, xVariable, columns){
  excludeColumns<-c("lat", "lon", "Extensions",
                    "Position", "AltitudeMetersDiff")
  if (xVariable == "DistanceMeters") {
    excludeColumns<-c(excludeColumns,"Time")
  } else {
    excludeColumns<-c(excludeColumns,"DistanceMeters")
  }
  if (length(columns)==1 && columns=="all"){
    if(is.data.frame(data)){
      columns<-colnames(data)
    } else {
      columns<-colnames(data[[1]])
    }
    columns <- columns[!columns %in% excludeColumns]
  }
  res<- data[, columns]

}


getSummary <- function(df){
  data.frame(Altitude = sum(df$AltitudeMeters),
             Distance = df$DistanceMeters[length(df$DistanceMeters)],
             HR = mean(df$HeartRateBpm),
             Pace = mean(df$InstPace[df$InstPace !=0]),
             Speed = mean(df$InstSpeed[df$InstSpeed !=0])
  )
}

getSummaryVect <- function(vect){
  data.frame(Time = vect["Time"],
             Distance = vect["DistanceMeters"],
             HR = vect["HeartRateBpm"],
             Pace = vect["Pace"],
             Speed = vect["Speed"],
             Altitude = vect["Altitude"]
  )
}


deltaF<-function(x){max(x)-min(x)}
avgF<-function(x){mean(x)}
sumF<-function(x){sum(x)}


melting<-function (data){

  variable<-NULL

  columns<-colnames(data)
  if ("Time" %in% columns){
    xVariable <- "Time"
  } else {
    xVariable <- "DistanceMeters"
  }
  data[xVariable]<-data[xVariable]-min(data[xVariable])
  xymelt <- melt(data, id.vars = xVariable)

  selection <- xymelt %>% filter(variable %in% columns)
  selection$variable<-as.character(selection$variable)
  a<-split(selection, selection$variable)
  a<-lapply(a, function(x){
    x$value_norm<-rescale(as.numeric(x$value),c(0,100))
    x$value <- as.numeric(x$value)
    x$variable<-x$variable
    x[xVariable]<-x[xVariable]
    return(x)
  })

  selection<-setDF(rbindlist(a))
}

modifyColumns<- function(summaryTable, adding, selectedColumn){
  summaryTable[,selectedColumn] <- c(summaryTable[1, selectedColumn], diff(summaryTable[, selectedColumn]))
  adding[selectedColumn] <- sum(summaryTable[, selectedColumn])
  return(list(summaryTable = summaryTable,
              adding = adding))
}


#require(zoo)
argmax <- function(x, y, w=1, ...) {
  #required to pass r cmd check
  loess<-NULL

  n <- length(y)
  y.smooth <- loess(y ~ x, ...)$fitted
  y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  res <- list(x=x[i.max], i=i.max, y.hat=y.smooth)
  return(res)
}


doSplit <- function(data, what, splitValues){
  n<-split(data, cut(data[,what], splitValues, include.lowest=TRUE))
  n<-lapply(n,function(x){
    x$Time<-x$Time-x$Time[1]
    x$DistanceMeters<-x$DistanceMeters-x$DistanceMeters[1]
    x$Pace<-1000*x$Time/x$DistanceMeters
    x$Speed<-0.06*x$DistanceMeters/x$Time
    x$Pace[1]<-0
    x$Speed[1]<-0
    return(x)
    })
  names(n)<-paste("interval",seq(1,length(n),1), sep = "")
  return(n)
}


columnExpected<-function(){
  list(timeColumn="Time",
       distanceColumn="DistanceMeters",
       altitudeColumn="AltutideMeters",
       positionColumn = "Position",
       HColumn = "HeartRateBpm"
  )
}


normFn<-function(x){
  plotrix::rescale(x,c(0,100))
}

