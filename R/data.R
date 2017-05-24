#' Example of activity containing intervals.
#'
#' A dataset resulting from loading a tcx file
#' of an activity with intervals
#'
#' @format A data frame with 4051 rows and 9 variables:
#' \describe{
#'   \item{Time}{The recorded time}
#'   \item{AltitudeMeters}{The altitude above sea level}
#'   \item{DistanceMeters}{The distance done [meters]}
#'   \item{HeartRateBpm}{The hear rate}
#'   \item{LatitudeDegrees}{The calculated latitude}
#'   \item{LongitudeDegrees}{The calculated longitude}
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
#' @format A data frame with 3207 rows and 11 variables:
#' \describe{
#'   \item{Time}{The recorded time}
#'   \item{AltitudeMeters}{The altitude above sea level}
#'   \item{DistanceMeters}{The distance done [meters]}
#'   \item{HeartRateBpm}{The hear rate}
#'   \item{LatitudeDegrees}{The calculated latitude}
#'   \item{LongitudeDegrees}{The calculated longitude}
#'   \item{AltitudeMetersDiff}{The elevation gained/lost}
#'   \item{Pace}{The pace [min/km]}
#'   \item{Speed}{The speed [km/h]}
#' }
"evenActivity"
