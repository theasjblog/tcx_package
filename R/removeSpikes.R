#NOT exported now because it is not ready/useful
# #' removeSpikes
# #' @description
# #' Function to remove spikes in metrics
# #' @param data a dataframe generated with gpxAnalyser::dataLoader() or the
# #' list of dataframes generated with gpxAnalyser::createSplits()
# #' @param tolLevel (numeric) the tolerance level
# #' @param span (numeric) the span value for the smooth function.
# #' Big span means big smooth
# #' @return
# #' The given object without the spikes
# #' @details
# #' The function accepts a list of dataframes or a data frame created
# #' with createSplits() or dataLoader()
# #' @examples
# #' gpx <- intervalActivity
# #' sp <- createSplits(gpx, 2000, type = "everyKm")
# #' sp <- removeSpikes(sp)
# #' @export

removeSpikes<-function(data, tolLevel = 0.1, span = 0.5){

  if(is.data.frame(data)){
    res<-norFn(data, tolLevel)
    } else {
      res<-lapply(data,function(x){
        res<-norFn(x, tolLevel, span)
        return(res)
  })
    }
  return(res)
}

norFn<-function(data, tolLevel, span){
  column<-c("Pace", "Speed")
  res<-apply(data[,colnames(data) %in% column], 2,myFn, tolLevel)
  data[,colnames(data) %in% column]<-res
  #smoothCol<-colnames(data)
  #noSmoothCol<-c("Time", "Position","Extensions",
  #               "lat", "lon", "AltitudeMetersDiff")
  #smoothCol<-smoothCol[!smoothCol %in% noSmoothCol]
  #res<-apply(data[,colnames(data) %in% column], 2, smmothFn, span)
  #data[,colnames(data) %in% smoothCol]<-res
  return(data)
}

myFn<-function(data, tolLevel){
  avg<-mean(data)
  tol<-avg*tolLevel
  idxMin<-which(data<avg-tol)
  idxMax<-which(data>avg+tol)
  idx<-c(idxMin,idxMax)
  data[idx]<-avg
  return(data)
}


smoothFn<-function(data, span){
  #required to pass r cmd check
  loess<-NULL
  predict<-NULL

  val<-data.frame(y=data,
                  x=seq(1,length(data),1))
  myMod <- loess(y ~ x,
                 data = val, span = span)
  data$pred <- predict(myMod, data = val)
  return(data$pred)
}
