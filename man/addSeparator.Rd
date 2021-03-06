% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addSeparator.R
\name{addSeparator}
\alias{addSeparator}
\title{addSeparator}
\usage{
addSeparator(data, splitHere, xVariable = "Time", yVariable = "Distance")
}
\arguments{
\item{data}{an activity dataframe}

\item{splitHere}{(numeric) vector for where to split}

\item{xVariable}{(numeric) the variable (column name) to put on the x axis}

\item{yVariable}{(numeric) the variable (column name) to put on the y axis}
}
\value{
A ggplot plot
}
\description{
Function to assist in identifying the split values to manually generate splits.
}
\details{
The function accepts a data frame created with dataLoader(). The function does not return a list of splits. It is
intended to be used as a tool to identify the best positions for the splits, which can be generated later on using
createSplits(). The function uses a plot to help with the task. By visualising appropriate metrics combiantions
(i.e. 'Time' vs 'Pace') it is possible to exactly identify when a split starts/finishes. The values can then be
used to generate splits using the function createSplits()
}
\examples{
gpx <- intervalActivity
proposedSplits <- c(9, 13, 15, 19, 21.2, 25.1, 27.3, 31.4, 33.4, 37.5, 39.5)
addSeparator(gpx, proposedSplits)
sp <- createSplits(gpx, proposedSplits, "thisMin")
}
