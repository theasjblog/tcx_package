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
m<-leaflet() %>% addTiles()

if (!is.data.frame(data)){
  colors <- c("red", "blue", "green", "white", "black","magenta")
  colors<- rep(colors,1+floor(length(data)/length(colors)))
  for(i in 1: length(data)){
  #add markers with colours for start, intermediate and finish
    m<- m %>% addPolylines(data = data[[i]], ~lon, ~lat,
                           color = colors[i]) %>% addMarkers(lng=data[[i]]$lon[length(data[[i]]$lon)],
                                                             lat=data[[i]]$lat[length(data[[i]]$lat)],
                                                             label=as.character(i))
    }
  } else {
    m<- m %>% addPolylines(data = data, ~lon, ~lat,
                           color = "blue") %>% addMarkers(lng=c(data$lon[1],data$lon[dim(data)[1]]),
                                                          lat=c(data$lat[1],data$lat[dim(data)[1]]),
                                                             label=c("start", "finish"))
    }
m
return(m)
}
