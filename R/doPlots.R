#' doPlots
#' @description
#' Function to plot activity metrics
#' @param data a dataframe generated with gpxAnalyser::dataLoader() or the
#' list of dataframes generated with gpxAnalyser::createSplits()
#' @param xVariable (character) the variable to put on the x axis. One of 'DistanceMeters'
#' or 'Time'
#' @param columns (character) The matrics to include in the plot. Must be the same name as the column in the dataframe.
#' Use columns = 'all' to plot all the metrics
#' @param doFacet (logical) TRUE to plot variables separetly, FALSE to plot
#' them overlayed
#' @return
#' A ggplot object
#' @details
#' The function accepts a list of dataframes or a data frame created
#' with createSplits() or dataLoader()
#' @examples
#' # gpx <- dataLoader("path_to_activity.tcx")
#' # generateMap(gpx)
#' # sp<-createSplits(gpx, 2000, type = "everyKm")
#' # doPlots(gpx)
#' # doPlots(gpx, doFacet = TRUE)
#' # doPlots(sp)
#' # doPlots(sp, doFacet = TRUE)
#' @export

doPlots<-function(data, xVariable = c("DistanceMeters","Time")[1],
                  columns = "all", doFacet = FALSE){

  if (is.data.frame(data)){
    data<-removeColumns(data, xVariable, columns)
  } else {
    for (i in 1:length(data)){
      data[[i]]<-removeColumns(data[[i]], xVariable, columns)
    }
  }

    if (is.data.frame(data)){
    selection<-melting(data)
  } else {
    selection<-lapply(data, melting)
    selection <-setDF(rbindlist(selection, idcol = "Interval"))
  }

  value <-NULL
  variable<-NULL
  value_norm<-NULL

  if (is.data.frame(data)){
    if (doFacet){
      p<-ggplot(data = selection,aes(x = selection[xVariable], y = as.numeric(value),
                                     group = variable, color = variable)) +
        geom_line() +
        facet_grid(variable ~ ., scales = "free")

    } else {
      p<-ggplot(data = selection,aes(x = selection[xVariable], y = as.numeric(value_norm),
                                     group = variable, color = variable)) +
        geom_line()
    }
  } else {
    if (doFacet){
      p<-ggplot(data = selection,aes(x = selection[xVariable], y = as.numeric(value),
                                     group = variable, color = variable)) +
        geom_line()
      p <- p + facet_grid(variable ~ selection$Interval, scales = "free")
    } else {
      p<-ggplot(data = selection,aes(x = selection[xVariable], y = as.numeric(value_norm),
                                     group = variable, color = variable)) +
        geom_line()
      p <- p + facet_grid(selection$Interval ~ ., scales = "free")
    }
  }

  p <- p + xlab(xVariable) + ylab("Value")
  p
}
