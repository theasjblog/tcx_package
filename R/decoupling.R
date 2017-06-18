#' decoupling
#' @description
#' Function to analyse activity decoupling
#' @param data an activity dataframe
#' @param variable (character) The metrics to compare in the decaopling analysis against heart rate.
#' Must be the same name as the column in the dataframe.
#' @param from (numeric) When to start the decoupling analysis
#' @param to (numeric) When to stop the decoupling analysis
#' @return
#' A list of dataframes with a summary of comparisons between heart rate (1st and 2nd half, compareHR),
#' variable (1st and 2nd half, compareVar), and heart rate against variable (overall, compareSlopes).
#' It also returns the decoupling plot (p).
#' @details
#' The concept of decoupling is discussed in Joe Friel blog. In summary, this function allows to visualise and 
#' quantify if an activity (ideally a long steady effort in zone 2) shows decoupling or not, that is if the  heart rate stays
#' constant but the pace/power decrease or if the the pace/power stay constant, but the heart rate increases.
#' High decoupling is symptomatic of poor aerobic fitness. 
#' @examples
#' gpx <- evenActivity
#' decoplingRes <- decoupling(data = gpx, variable = "Pace", from = 10, to = 60)
#' decoplingRes$compareHR
#' decoplingRes$compareVar
#' decoplingRes$compareSlopes
#' print(decoplingRes$p)
#' @export

decoupling <- function(data, variable, from, to){
  
  divide <-  mean(c(from, to))
  
  meansReal <- computeDecoupling(data, variable, from, to)
  p <- plotDecoupling(data, variable, from, to)
  meansReal$p <- p
  return(meansReal)
  
}
