#' addSeparator
#' @description
#' Function to assist in identifying the split values to manually generate splits.
#' @param data an activity dataframe
#' @param splitHere (numeric) vector for where to split
#' @param xVariable (numeric) the variable (column name) to put on the x axis
#' @param yVariable (numeric) the variable (column name) to put on the y axis
#' @return
#' A ggplot plot
#' @details
#' The function accepts a data frame created with dataLoader(). The function does not return a list of splits. It is
#' intended to be used as a tool to identify the best positions for the splits, which can be generated later on using
#' createSplits(). The function uses a plot to help with the task. By visualising appropriate metrics combiantions
#' (i.e. 'Time' vs 'Pace') it is possible to exactly identify when a split starts/finishes. The values can then be
#' used to generate splits using the function createSplits()
#' @examples
#' gpx <- intervalActivity
#' proposedSplits <- c(9, 13, 15, 19, 21.2, 25.1, 27.3, 31.4, 33.4, 37.5, 39.5)
#' addSeparator(gpx, proposedSplits)
#' sp <- createSplits(gpx, proposedSplits, "thisMin")
#' @export

addSeparator<-function(data, splitHere, xVariable="Time", yVariable = "Distance"){
  if (!is.data.frame(data)){
    stop("data must be a data frame")
  }
  if(!is.numeric(splitHere) | any(is.na(splitHere))){
    stop("splitHere must be numeric and not NAs")
  }
  if (length(xVariable)>1 | !is.character(xVariable) | !xVariable %in% colnames(data)){
    "xvariable must be a character vector of length 1 and must be a column name available in data"
  }
  if (length(yVariable)>1 | !is.character(yVariable) | !yVariable %in% colnames(data)){
    "yvariable must be a character vector of length 1 and must be a column name available in data"
  }

  plotData<-data.frame(x=data[,xVariable],
                       y=data[,yVariable])

  #needed to pass R CMD Check
  x <- NULL
  y <- NULL

  p<-ggplot(plotData, aes(x, y, color = "red"))+geom_line()+geom_vline(xintercept = splitHere, colour = "blue") +
    xlab(xVariable) + ylab(yVariable) +
    theme_bw() +
    theme(legend.position='none')
  print(p)
}
