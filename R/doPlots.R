#' doPlots
#' @description
#' Function to plot activity metrics
#' @param data a dataframe generated with gpxAnalyser::dataLoader() or the
#' list of dataframes generated with gpxAnalyser::createSplits()
#' @param xVariable (character) the variable to put on the x axis. One of 'DistanceMeters'
#' or 'Time'
#' @param showMe (character) The metrics to include in the plot. Must be the same name as the column in the dataframe.
#' @param doFacet (logical) TRUE to plot variables separetly, FALSE to plot
#' them overlayed
#' @return
#' A ggplot object
#' @details
#' The function accepts a list of dataframes or a data frame created
#' with createSplits() or dataLoader()
#' @examples
#' gpx <- evenActivity
#' doPlots(gpx, showMe = colnames(gpx))
#' doPlots(gpx, doFacet = TRUE, showMe = colnames(gpx))
#' sp<-createSplits(gpx, 2000, type = "everyKm")
#' doPlots(sp, showMe = colnames(sp[[1]]))
#' doPlots(sp, doFacet = TRUE, showMe = colnames(sp[[1]]))
#' @export

doPlots<-function(data, xVariable = c("DistanceMeters","Time")[2],
                  showMe, doFacet = TRUE){

  showMe<-unique(c(xVariable,showMe))

  if (is.data.frame(data)){
    idx<-which(colnames(data) %in% showMe)
    data<-data[,idx]
  } else {
    idx<-which(colnames(data[[1]]) %in% showMe)
    for (i in 1:length(data)){
      data[[i]]<-data[[i]][,idx]
    }
  }

    if (is.data.frame(data)){
    selection<-melting(data, xVariable)
  } else {
    selection<-lapply(data, function(x){melting(x, xVariable)})
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
