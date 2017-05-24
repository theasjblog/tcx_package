#' @import dplyr
#' @import data.table
#' @import xml2
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

x <- read_xml(datapath)
ns <- xml_ns(x)
cols <- suppressWarnings(xml_name(xml_children(xml_find_all(x,
                                                            "//d1:Trackpoint", ns))))
cols<-unique(cols)
cols <- paste0("//d1:", cols)
if (any(grepl("Position", cols))) {
  cols <- cols[!grepl("Position", cols)]
  tmp <- suppressWarnings(xml_name(xml_children(xml_find_one(x,
                                                             "//d1:Position", ns))))
  cols <- c(cols, paste0("//d1:", tmp))
}
if (any(grepl("Extensions", cols))) {
  cols <- cols[!grepl("Extensions", cols)]
  tmp <- suppressWarnings(xml_name(xml_children(xml_find_all(x,
                                                             "//ns3:TPX", ns))))
  cols <- c(cols, paste0("//ns3:", tmp))
}
cols<-unique(cols)
trcols <- paste0("//d1:Trackpoint", cols)
message("Reading .tcx file...")
data <- lapply(trcols, function(c) {
  out <- xml_text(xml_find_all(x, c, ns))
  if (all(!is.na(suppressWarnings(as.numeric(out)))))
    out <- as.numeric(out)
  out
})

names(data) <- vapply(strsplit(cols, ":"), function(x) x[length(x)],
                      character(1))

data$Time<-as.character(data$Time)
b<-strsplit(data$Time, "T")
n<-sapply(b, "[[", 2)
b<-strsplit(n, "\\.")
n<-sapply(b, "[[", 1)
times<-c(as.matrix(read.table(text = n, sep = ":")) %*% c(60, 1, 1/60))


    len <- vapply(data, length, numeric(1))
    if (length(unique(len)) > 1) {
      message("Resolving missing data points...")
      issues <- names(len[len < max(len, na.rm = TRUE)])

      .issues <- cols[vapply(issues, function(x) which(grepl(x,
                                                             cols)), numeric(1))]
      nds <- as_list(xml_find_all(x, "//d1:Trackpoint", ns))

      data<-dealMissingPoints(data, issues, nds)

    }

    fields<-names(data)
    for (i in 1:length(fields)){
      if (fields[i] == "DistanceMeters"){
        if(any(data[[i]] %%1 != 0)){
        data[[i]]<-interpolateMissing(data[[i]])
        }
      }else{
        data[[i]]<-interpolateMissing(data[[i]])
      }
    }

    data$Time<-times-times[1]

    data <- as.data.frame(data)




if("Time" %in% colnames(data) && "DistanceMeters" %in% colnames(data)){
  data$Pace<-1000*data$Time/data$DistanceMeters
  data$Speed<-0.06*data$DistanceMeters/data$Time
  data$Pace[1]<-0
  data$Speed[1]<-0
  data$DistanceMeters[1] <- 0
}
if ("AltitudeMeters" %in% colnames(data)){
  data$AltitudeMetersDiff<-c(0, diff(data$AltitudeMeters))
}

idx<-which(is.na(apply(data,2,sum)))
if(length(idx)>0){
  data<-data[,-idx]
}

v<-factor(round(diff(data$Time),digits = 4))
a<-summary(v)
thres <- as.numeric(names(a)[a == max(a)])
newTime<-rep(0, length(data$Time))
for (i in 2:length(data$Time)){
  if(data$Time[i] != data$Time[i-1]){
    newTime[i] <- newTime[i-1]+thres
  } else if (data$Time[i] == data$Time[i-1]){
    newTime[i] <- newTime[i-1]
  }
}
data$Time <- newTime


return(data)

}

