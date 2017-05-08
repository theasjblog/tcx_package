#' Example of activity containing intervals.
#'
#' A dataset resulting from loading a tcx file
#' of an activity with intervals
#'
#' @format A data frame with 3730 rows and 11 variables:
#' \describe{
#'   \item{Time}{The recorded time}
#'   \item{Position}{The lat-lon position}
#'   \item{AltitudeMeters}{The altitude above sea level}
#'   \item{DistanceMeters}{The distance done [meters]}
#'   \item{HeartRateBpm}{The hear rate}
#'   \item{Extensions}{??}
#'   \item{lat}{The calculated latitude}
#'   \item{lon}{The calculated longitude}
#'   \item{AltitudeMetersDiff}{The elevation gained/lost}
#'   \item{Pace}{The pace [min/km]}
#'   \item{Speed}{The speed [km/h]}
#'   }
"intervalActivity"

#' Example of activity not containing intervals.
#'
#' A dataset resulting from loading a tcx file
#' of an activity without intervals
#'
#' @format A data frame with 4324 rows and 11 variables:
#' \describe{
#'   \item{Time}{The recorded time}
#'   \item{Position}{The lat-lon position}
#'   \item{AltitudeMeters}{The altitude above sea level}
#'   \item{DistanceMeters}{The distance done [meters]}
#'   \item{HeartRateBpm}{The hear rate}
#'   \item{Extensions}{??}
#'   \item{lat}{The calculated latitude}
#'   \item{lon}{The calculated longitude}
#'   \item{AltitudeMetersDiff}{The elevation gained/lost}
#'   \item{Pace}{The pace [min/km]}
#'   \item{Speed}{The speed [km/h]}
#' }
"evenActivity"
