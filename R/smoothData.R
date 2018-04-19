#' gpxSmooth
#' @description
#' Function to smooth  variable. The function uses the function
#' runmed() to smooth the data. See its documentation for
#' arguents and tier meaning
#' @param data (dataframe) an activity dataframe
#' @param variable (character) the column name of the variable to smooth. If NULL, all
#' variable except 'Time', 'Distance', 'LatitudeDegrees' and 'LongitudeDegrees' will be
#' smoothed
#' @return
#' An activity dataframe with smoothed data
#' @details
#' The function uses the provided split values and split type to divide the activity dataframe in a list of dataframes,
#' each one corresponding to a split starting and finishing at the specified times/distances.
#' @examples
#' gpx <- evenActivity
#' smoothedData <- gpxSmooth(gpx, "Pace", k = 39)
#' @export

gpxSmooth <- function(data, variable = NULL, k,
                       endrule = c("median", "keep", "constant")[1],
                       algorithm = NULL, print.level = 0){

  if (is.null(variable)){
    variable <- colnames(data)[!colnames(data) %in% c("Time",
                                                      "Distance", "LatitudeDegrees",
                                                      "LongitudeDegrees")]
  }
  validateDataSmooth(data, variable)

  for (i in 1:length(variable)){
    data[, variable[i]] <- runmed(data[, variable[i]], k,
                               endrule = c("median", "keep", "constant")[1],
                               algorithm = NULL, print.level = 0)
  }

  return(data)
}

validateDataSmooth <- function(data, variable){
  if (!is.data.frame(data)){
    stop("'data' must be a data.frame")
  }
  if(dim(data)[1]<1){
    stop("Not enough data")
  }
  if(!is.character(variable)){
    stop("'variable' must be a character")
  }

  if(!all(variable %in% colnames(data))){
    stop("At least one of 'variable' is not a column in the data.frame")
  }
}
