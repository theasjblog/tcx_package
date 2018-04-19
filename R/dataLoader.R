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
#' @importFrom stats lm

NULL

#' dataLoader
#' @description
#' Function to load TCX activity data
#' @param datapath The path to the tcx file
#' @return
#' The function returns a data frame with the tcx data
#' @details
#' The function accepts a TCX file with gps data. Files
#' downloaded from Garmin have been tested for swim (OW and pool),
#' run, cycle (turbo trainer and outdoor) and cross-country skiing.
#' In theory any TCX should work, but there is guarantee that all will.
#' @examples
#' # gpx <- dataLoader("path_to_activity.tcx")
#' @export

dataLoader <- function(datapath){

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
  if(length(tmp != 0)){
    cols <- c(cols, paste0("//d1:", tmp))
    }
}
if (any(grepl("Extensions", cols))) {
  cols <- cols[!grepl("Extensions", cols)]
  tmp <- suppressWarnings(xml_name(xml_children(xml_find_all(x,
                                                             "//ns3:TPX", ns))))
  if(length(tmp != 0)){
    cols <- c(cols, paste0("//ns3:", tmp))
  }

}
cols<-unique(cols)
trcols <- paste0("//d1:Trackpoint", cols)
message("Reading .tcx file...")
data <- lapply(trcols, function(c) {
  out <- suppressWarnings(xml_text(xml_find_all(x, c, ns)))
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
b<-strsplit(n, "Z")
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
        if(any(data[[i]][!is.na(data[[i]])] %%1 != 0)){
        data[[i]]<-interpolateMissing(data[[i]])
        }
      }else{
        data[[i]]<-interpolateMissing(data[[i]])
      }
    }


    data$Time<-times-times[1]
    data <- as.data.frame(data)


    idx<-which(is.na(apply(data,2,sum)))
    rowsRemove <- NULL
    if(length(idx)>0){
      for (i in 1:length(idx)){
        rowsRemove <- c(rowsRemove, which(is.na(data[,idx[i]])))
      }
    }
    if (!is.null(rowsRemove)){
      data <- data[-rowsRemove,]
    }


    #fix pauses
    if (!all(diff(data$DistanceMeters[data$DistanceMeters!=0]) %% min(data$DistanceMeters[data$DistanceMeters!=0]) == 0)){#exclude swims in pool
      v <- round(diff(data$Time), digits = 4)
      v <- v[v != 0]
      v <- factor(v)
      a <- summary(v)
      thres <- as.numeric(names(a)[a == max(a)])[1]
      newTime <- rep(0, length(data$Time))
      for (i in 2:length(data$Time)){
        if (data$Time[i] != data$Time[i-1]){
          newTime[i] <- newTime[i-1]+thres
        } else if (data$Time[i] == data$Time[i-1]){
          newTime[i] <- newTime[i-1]
        }
      }
      data$Time <- newTime
    }


if("Time" %in% colnames(data) && "DistanceMeters" %in% colnames(data)){
  data$Pace<-c(0,1000*diff(data$Time)/diff(data$DistanceMeters))
  data$Speed<-c(0,0.06*diff(data$DistanceMeters)/diff(data$Time))
  #data$Pace[1]<-0
  #data$Speed[1]<-0
  #idx <- unique(
  #  c(which(is.infinite(data$Pace)),
  #    which(is.nan(data$Pace))))
  #data <- data[-idx,]
  data$DistanceMeters[1] <- 0
  idx <- which(is.nan(data$Pace))
  if (length(idx)>0){
    data[idx,] <- data[idx-1,]
  }
  idx <- which(is.nan(data$Pace))
  if (length(idx)>0){
    data[idx,] <- data[idx-1,]
  }
  idx <- which(is.infinite(data$Pace))
  if (length(idx)>0){
    data <- data[-idx,]
  }
}
#    Inf and NaN issue in compareSplits:
#    na.rm = TRUE
#    Multiply your matrix by the result of is.finite(m) and call rowSums on the product with na.rm=TRUE. This works because Inf*0 is NaN.
#
#    m <- matrix(c(1:3,Inf,4,Inf,5:6),4,2)
#    rowSums(m*is.finite(m),na.rm=TRUE)
#    na.rm = TRUE

if ("AltitudeMeters" %in% colnames(data)){
  data$AltitudeMetersDiff<-c(0, diff(data$AltitudeMeters))
}

if("Time" %in% colnames(data) && "DistanceMeters" %in% colnames(data) && "AltitudeMeters" %in% colnames(data)){
      grade <- 100 * c(0,diff(data$AltitudeMeters)) / c(1,diff(data$DistanceMeters))
      grade[1] <- 0
      perc <- ifelse(grade > 0, 0.035, 0.018)
      perc[grade == 0] <- 0
      data$GAP <- data$Pace - data$Pace*(perc*grade)
      idx <- which(is.nan(data$GAP))
      if (length(idx)>0){
        data[idx,] <- data[idx-1,]
      }
      idx <- which(is.infinite(data$GAP))
      if (length(idx)>0){
        data <- data[-idx,]
      }

  }

newNames <- unlist(lapply(seq_along(data),function(i){displayNames(colnames(data)[i])}))
colnames(data) <- newNames

return(data)

}

