% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataLoader.R
\name{dataLoader}
\alias{dataLoader}
\title{dataLoader}
\usage{
dataLoader(datapath)
}
\arguments{
\item{datapath}{The path to the tcx file}
}
\value{
The function returns a data frame with the tcx data
}
\description{
Function to load TCX activity data
}
\details{
The function accepts a TCX file with gps data. Files
downloaded from Garmin have been tested for swim (OW and pool),
run, cycle (turbo trainer and outdoor) and cross-country skiing.
In theory any TCX should work, but there is guarantee that all will.
}
\examples{
# gpx <- dataLoader("path_to_activity.tcx")
}
