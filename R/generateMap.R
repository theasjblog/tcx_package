#' generateMap
#' @description
#' Function to a map of the activity
#' @param data a dataframe generated with gpxAnalyser::dataLoader() or the
#' list of dataframes generated with gpxAnalyser::createSplits()
#' @return
#' A leaflet object to view as a map
#' @details
#' The function accepts a list of dataframes or a data frame created
#' with createSplits() or dataLoader()
#' @examples
#' gpx <- intervalActivity
#' generateMap(gpx)
#' sp<-createSplits(gpx, 2000, type = "everyKm")
#' generateMap(sp)
#' @export

generateMap <- function(data){

  if(is.data.frame(data)){
    if(!"LatitudeDegrees" %in% colnames(data) | !"LongitudeDegrees" %in% colnames(data)){
      stop("data must contains the columns 'LatitudeDegrees' and 'LongitudeDegrees'")
    }
  } else {
    if(!"LatitudeDegrees" %in% colnames(data[[1]]) | !"LongitudeDegrees" %in% colnames(data[[1]])){
      stop("data must contains the columns 'LatitudeDegrees' and 'LongitudeDegrees'")
    }
  }
  
  
  m<-leaflet() %>% addTiles()
  
  if (!is.data.frame(data)){
    colors <- c("red", "blue", "green", "black","magenta")
    colors<- rep(colors,length.out = length(data))
    
    for(i in 1: length(data)){
      #add markers with colours for start, intermediate and finish
      m<- m %>% addPolylines(data = data[[i]], ~LongitudeDegrees, ~LatitudeDegrees,
                             color = colors[i]) %>% addMarkers(lng=data[[i]]$LongitudeDegrees[length(data[[i]]$LongitudeDegrees)],
                                                               lat=data[[i]]$LatitudeDegrees[length(data[[i]]$LatitudeDegrees)],
                                                               label=as.character(i))
    }
  } else {
    m<- m %>% addPolylines(data = data, ~LongitudeDegrees, ~LatitudeDegrees,
                           color = "blue") %>% addMarkers(lng=c(data$LongitudeDegrees[1],data$LongitudeDegrees[dim(data)[1]]),
                                                          lat=c(data$LatitudeDegrees[1],data$LatitudeDegrees[dim(data)[1]]),
                                                             label=c("start", "finish"))
    }
m
return(m)
}
