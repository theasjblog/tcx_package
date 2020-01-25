#' @importFrom zoo rollmean
#' @title gpxSmooth
#' @description
#' Function to smooth  variable. The function uses the function
#' rollmean() from the package zoo to smooth the data. See its documentation for
#' arguents and tier meaning
#' @param data (dataframe) an activity dataframe
#' @param variable (character) the column name of the variable to smooth. If NULL, all
#' variable except 'Time', 'Distance', 'LatitudeDegrees' and 'LongitudeDegrees' will be
#' smoothed
#' @param k integer the size of the smooting kernel. Must be odd.
#' An inner function converts even Ks to the closest odd K.
#' @return
#' An activity dataframe with smoothed data
#' @details
#' The function uses the provided split values and split type to divide the activity dataframe in a list of dataframes,
#' each one corresponding to a split starting and finishing at the specified times/distances.
#' @examples
#' gpx <- evenActivity
#' smoothedData <- gpxSmooth(gpx, "Pace", k = 39)
#' @export

gpxSmooth <- function(data, variable = NULL, k = 1){
  
  k <- validateK(k)
  
  if (is.null(variable)){
    variable <- colnames(data)[!colnames(data) %in% c("Time",
                                                      "Distance", "LatitudeDegrees",
                                                      "LongitudeDegrees")]
  }
  validateDataSmooth(data, variable)
  
  for (i in 1:length(variable)){
    data[, variable[i]] <- zoo::rollmean(data[, variable[i]], k,
                                         fill = c('extend', 'extend', 'extend'))
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

validateK <- function(k){
  errorString <- list()
  if (length(k) != 1){
    errorString <- c(errorString,
                     '"k" must be of lenght 1')
  }
  if (is.na(k)){
    errorString <- c(errorString,
                     '"k" cannot be NA')
  }
  if(!any(c(is.numeric(k), is.integer(k)))){
    errorString <- c(errorString,
                     '"k" must be numeric or integer')
  }
  if (is.infinite(k)){
    errorString <- c(errorString,
                     '"k" cannot be infinite')
  }
  if (k <= 1){
    errorString <- c(errorString,
                     '"k" must be at least 1')
  }
  
  if (length(errorString) != 0){
    stop(
      paste0(unlist(errorString), collapse = '. ')
    )
  }
  
  k <- makeKOdd(k)
  
  return(k)
}


makeKOdd <- function(k){
  originalK <- k
  k <- as.integer(k)
  if (k %% 2 == 0){
    k <- k + 1
  }
  if (k != originalK){
    warning(paste0('Change "k" from ', originalK,
                   ' to ', k))
    
  }
  return(k)
}