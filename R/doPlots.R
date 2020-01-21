#' doPlots
#' @description
#' Function to plot activity metrics
#' @param data an activity dataframe
#' @param xVariable (character) the variable to put on the x axis. One of 'DistanceMeters'
#' or 'Time'
#' @param showMe (character) The metrics to include in the plot. Must be the same name as the column in the dataframe.
#' @param doFacet (logical) TRUE to plot variables separetly, FALSE to plot
#' them overlayed
#' @param sp (list) the list of frame of splits, if plotting splits
#' @return
#' A ggplot object
#' @details
#' The function accepts a list of dataframes or a data frame created
#' with createSplits() or dataLoader()
#' @examples
#' gpx <- evenActivity
#' doPlots(gpx)
#' doPlots(gpx, doFacet = TRUE)
#' sp<-autoSplits(gpx)
#' doPlots(gpx, sp = sp)
#' @export

doPlots<-function(data, xVariable = c("Distance","Time")[2],
                  showMe = NULL, doFacet = TRUE, sp = NULL){


  if (is.data.frame(data)){
    if (is.null(showMe)){
      showMe <- colnames(data)
    }
    idx<-which(colnames(data) %in% showMe)

  } else {
    stop("Must be a data frame")
  }


  showMe <- showMe[!showMe %in% c("LatitudeDegrees", "LongitudeDegrees")]


  if ("Elevation gain" %in% showMe){
    showMe[showMe == "Elevation gain"] <- "Elevation"
  }

  if(xVariable == "Time"){
    if ("Distance" %in% showMe){
      idx <- which(showMe == "Distance")
      showMe <- showMe[-idx]
    }
  }
  if(xVariable == "Distance"){
    if ("Time" %in% showMe){
      idx <- which(showMe == "Time")
      showMe <- showMe[-idx]
    }
  }
  showMe<-unique(c(xVariable,showMe))
  idx <- which(colnames(data) %in% showMe)
 
  if (length(idx)>0){
    data <- data[,idx]
  }



    if(!is.null(sp)){

      splits <- getSplitsValues(data, sp, xVariable)
      toKeep <- notToHide(sp)
      varSave <- data[,xVariable]
      temp <- data
      data[,] <- NA
      data[toKeep,] <- temp[toKeep,]
      data[,xVariable] <- varSave
    }
  idx <- which(diff(data[,xVariable]) == 0)
  if (length(idx)>0){
    data <- data[-idx,]
  }
 
  selection<-melting(data, xVariable)

  value <-NULL
  variable<-NULL
  value_norm<-NULL

    if (doFacet){
      p <- ggplot(data = selection,aes(x = selection[[xVariable]], y = as.numeric(value),
                                     group = variable, color = variable)) +
        geom_line() +
        #geom_area(aes(fill = variable)) +
        facet_grid(variable ~ ., scales = "free")

    } else {
      p<-ggplot(data = selection,aes(x = selection[[xVariable]], y = as.numeric(value_norm),
                                     group = variable, color = variable)) +
        geom_line()
    }

  if (!is.null(sp)){
    p <- p + geom_vline(xintercept = splits, color = "black")
  }

  p <- p + xlab(xVariable) + ylab("Value") +
    theme_bw() +
    theme(legend.position='none')
    
  p
}
