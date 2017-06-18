#' Example of activity containing intervals.
#'
#' A dataset resulting from loading a tcx file
#' of an activity with intervals.
#'
#' @format A data frame with 2701 rows and 10 variables:
#' \describe{
#'   \item{Time}{The recorded time}
#'   \item{Elevation}{The altitude above sea level}
#'   \item{Distance}{The distance done [meters]}
#'   \item{Heart rate}{The hear rate}
#'   \item{LatitudeDegrees}{The calculated latitude}
#'   \item{LongitudeDegrees}{The calculated longitude}
#'   \item{Elevation gain}{The elevation gained/lost}
#'   \item{Pace}{The pace [min/km]}
#'   \item{Speed}{The speed [km/h]}
#'   \item{GAP}{The gradient adjusted pace [min/Km]}
#'   }
"intervalActivity"

#' Example of activity not containing intervals.
#'
#' A dataset resulting from loading a tcx file
#' of an activity without intervals, i.e. a steady effort activity.
#'
#' @format A data frame with 5192 rows and 10 variables:
#' \describe{
#'   \item{Time}{The recorded time}
#'   \item{Elevation}{The altitude above sea level}
#'   \item{Distance}{The distance done [meters]}
#'   \item{Heart rate}{The hear rate}
#'   \item{LatitudeDegrees}{The calculated latitude}
#'   \item{LongitudeDegrees}{The calculated longitude}
#'   \item{Elevation gain}{The elevation gained/lost}
#'   \item{Pace}{The pace [min/km]}
#'   \item{Speed}{The speed [km/h]}
#'   \item{GAP}{The gradient adjusted pace [min/Km]}
#' }
"evenActivity"
