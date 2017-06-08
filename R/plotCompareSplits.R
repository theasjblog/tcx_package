#' plotCompareSplits
#' @description
#' Function to plot activity metrics comparison between splits
#' @param data (list) the list of dataframes generated with
#' gpxAnalyser::createSplits()
#' @param doFacet (logical) TRUE to plot variables separetly, FALSE to plot
#' them overlayed
#' @param type (character) One of "raw", "norm" or "perc" to plot differently
#' normalised valued. Raw is the raw data. Norm is the data normalised in the rage 0 to 100,
#' perc is the variation in percentage from the mean value of all the intervals. If using 'raw'
#' or 'perc' it is suggested to set doFacet = TRUE
#' @param xVariable (character) the variable to put on the x axis. One of 'DistanceMeters'
#' or 'Time'
#' @param showMe (character) The metrics to include in the plot. Must be the same name as the column in the dataframe.
#' @param ftp (numeric) Functional threshold power if bike workout, pace [min/km] if run, functional threshold if using HR
#' @param ftpType (character) One os 'power', 'pace' or 'HR', to describe what kind of FTP
#' was provided
#' @return
#' A ggplot object
#' @details
#' The function accepts a list of dataframes created
#' with createSplits()
#' @examples
#' gpx <- intervalActivity
#' sp <- autoSplits(gpx)
#' plotCompareSplit(sp, doFacet=TRUE, type = "raw")
#' plotCompareSplit(sp, doFacet=TRUE, type = "norm")
#' plotCompareSplit(sp, doFacet=FALSE, type = "norm")
#' plotCompareSplit(sp, doFacet=TRUE, type = "perc")
#' @export


plotCompareSplit<-function(data, doFacet = TRUE, type = c("raw", "norm", "perc")[1],
                           xVariable = "Time", showMe = NULL, ftp = NULL,
                           ftpType = c("power", "pace", "HR")[1]){
  if(is.data.frame(data)){
    stop("Must be a list of splits")
  }

  if (is.null(showMe)){
    showMe <- colnames(data[[1]])
    showMe <- showMe[!showMe %in% c("LatitudeDegrees", "LongitudeDegrees")]
  }
  showMe<-unique(c(xVariable,showMe))
  if ("Elevation gain" %in% showMe){
    showMe[showMe == "Elevation gain"] <- "Elevation"
  }


  summaryTable<-compareSplits(data, showMe, ftp, ftpType)

  if("TSS" %in% colnames(summaryTable)){
    showMe <- c(showMe, "IF", "TSS")
  }


  idx<-which(colnames(summaryTable) %in% showMe)
  summaryTable <- summaryTable[,idx]


  summaryTable <- summaryTable[seq(1,dim(summaryTable)[1]-1,1),]
  summaryTable$Interval<-seq(1,dim(summaryTable)[1],1)

  summaryTable_norm<-lapply(summaryTable, normFn)
  summaryTable_norm<-as.data.frame(summaryTable_norm)
  summaryTable_norm$Interval<-summaryTable$Interval

  summaryTable_perc <- lapply(summaryTable, percFn)
  summaryTable_perc<-as.data.frame(summaryTable_perc)
  summaryTable_perc$Interval<-summaryTable$Interval


  plottingData<-melt(summaryTable, "Interval")
  plottingData_norm<-melt(summaryTable_norm, "Interval")
  plottingData_perc<-melt(summaryTable_perc, "Interval")

  #required to pass r cmd check
  Interval<-NULL
  value<-NULL
  variable<-NULL

  if(type == "raw"){
    p<-ggplot(data = plottingData,aes(x = Interval, y = as.numeric(value),
                                 group = variable, color = variable)) +
    geom_line()
  } else if (type == "norm"){
    p<-ggplot(data = plottingData_norm,aes(x = Interval, y = as.numeric(value),
                                      group = variable, color = variable)) +
      geom_line()
  } else if(type == "perc"){
    p<-ggplot(data = plottingData_perc,aes(x = Interval, y = as.numeric(value),
                                           group = variable, color = variable)) +
      geom_line()
    }
  p <- p+ylab("Value")

  if (doFacet){
    p <- p + facet_grid(variable ~ ., scales = "free")
  }
  p <- p + theme_bw() + theme(legend.position='none')
    
  p

}
