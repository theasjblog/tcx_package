#' @import dplyr
#' @import data.table
#' @import XML
#' @import reshape2
#' @import plotrix
#' @import stringr
#' @import utils
#' @import leaflet
#' @import ggplot2
#' @import zoo

NULL

#' dataLoader
#' @description
#' Function to load TCX satelltite data
#' @param datapath The path to the tcx file
#' @return
#' The function returns a data frame with the tcx data
#' @details
#' The function accepts a TCX file with gps data. Files
#' downloaded from Garmin have been tested
#' @examples
#' # gpx <- dataLoader("path_to_activity.tcx")
#' @export

dataLoader <- function(datapath){
definedColumns<-columnExpected()

doc<- xmlParse(datapath)
a<-xmlToDataFrame(nodes <- getNodeSet(doc, "//ns:Trackpoint", "ns"))
#no nans
nans <- NULL
for (i in 1:dim(a)[2]){
  nans<-append(nans, which(is.na(a[,i])))
}
a<-a[-c(nans),]

#check dataframe
columnsRead<-colnames(a)
if (!definedColumns$timeColumn %in% columnsRead){
  stop("Time'c must be present")
}


#convert time
a$Time<-as.character(a$Time)
b<-strsplit(a$Time, "T")
n<-sapply(b, "[[", 2)
b<-strsplit(n, "\\.")
n<-sapply(b, "[[", 1)
times<-c(as.matrix(read.table(text = n, sep = ":")) %*% c(60, 1, 1/60))
a$Time<-times-times[1]
#make position, distance, altitude, HRM the right format
b<-a
a<-as.data.frame(lapply(a, function(x){as.numeric(x)}))
v<-sapply(a, function(a)all(is.na(a)))
if (any(v)){
  a[,v]<-b[,v]
}

if ("Position" %in% columnsRead){
  #compute lat and lon
  for (i in 1:dim(a)[1]){
    coord <- split_coord(a$Position[i])
    a$lat[i]<-coord$lat
    a$lon[i]<-coord$lon
  }
}
a<-a[!is.na(a$lat),]


#handle speed and pace. compute here, but also in splits using
#diff(time)/diff(distance). make a function in helper to compute
#them
if ("AltitudeMeters" %in% columnsRead){
  a$AltitudeMetersDiff<-c(0, diff(a$AltitudeMeters))
}

a$Pace<-1000*a$Time/a$DistanceMeters
a$Speed<-0.06*a$DistanceMeters/a$Time
a$Pace[1]<-0
a$Speed[1]<-0

return(a)
}

