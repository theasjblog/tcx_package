% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createSplits.R
\name{createSplits}
\alias{createSplits}
\title{createSplits}
\usage{
createSplits(
  data,
  splitValues,
  type = c("everyKm", "thisKm", "everyMin", "thisMin")[1]
)
}
\arguments{
\item{data}{(dataframe) an activity dataframe}

\item{splitValues}{(numeric vector) the split points
\itemize{
\item To split for distance enter the value in meters, i.e. 1000 to split at every Km
\item To split for distance enter the value in minutes, i.e. 5.5 to split at every 5min30sec
}}

\item{type}{(character). one of the following:
\itemize{
\item 'everyKm' to split at every Km, i.e. every 5Km
\item 'thisKm' to split at specific Km values, i.e. 1, 4, 6, 7 Km
\item 'everyMin' to split at every min, i.e. every 5min
\item 'thisMin' to split at specific min, i.e. 1, 4, 6, 7 min
}}
}
\value{
The function returns a list of data frames, one for each split
}
\description{
Function to split an activity into custom splits
}
\details{
The function uses the provided split values and split type to divide the activity dataframe in a list of dataframes,
each one corresponding to a split starting and finishing at the specified times/distances.
}
\examples{
gpx <- evenActivity
sp<-createSplits(gpx, 2000, type = "everyKm")
sp<-createSplits(gpx, c(1000, 3000, 4500), type = "thisKm")
sp<-createSplits(gpx, 2, type = "everyMin")
sp<-createSplits(gpx, c(5, 15, 35), type = "thisMin")
}
