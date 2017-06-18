#' generateMap
#' @description
#' Function to generate a map of the activity
#' @param data an activity dataframe or a list of activity dataframes (splits)
#' @return
#' A leaflet object to view as a map
#' @details
#' If the activity dataframe(s) contains latitude and longitude informations, then this
#' function will plot the activity trackpoints on a map
#' @examples
#' gpx <- evenActivity
#' generateMap(gpx)
#' sp <- autoSplits(gpx)
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
