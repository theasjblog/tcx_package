x <- read_xml(datapath5)
ns <- xml_ns(x)
cols <- suppressWarnings(xml_name(xml_children(xml_find_one(x, "//d1:Lap", ns))))
cols <- paste0("//d1:", cols)
trcols <- paste0("//d1:Lap", cols)
data <- lapply(trcols, function(c) {
     out <- xml_text(xml_find_all(x, c, ns))
     if (all(!is.na(suppressWarnings(as.numeric(out))))) 
            out <- as.numeric(out)
        out
    })
names(data)<-suppressWarnings(xml_name(xml_children(xml_find_one(x, "//d1:Lap", ns))))

#get interval lengths
idx <- which(data$DistanceMeters==0)
m<-data$DistanceMeters[idx-1]
b<-diff(m)
b[1]<-data$DistanceMeters[1]
b <- c(b,data$DistanceMeters[length(data$DistanceMeters)]-m[length(m)])
data$DistanceMeters <- b

data$DistanceMeters <- data$DistanceMeters[-c(which(data$MaximumSpeed==0))]
data$TotalTimeSeconds <- data$TotalTimeSeconds[-c(which(data$TotalTimeSeconds==0))]

data<-data.frame(Time=data$TotalTimeSeconds,
                 DistanceMeters=data$DistanceMeters,
                 Pace=(100/60)*data$TotalTimeSeconds/data$DistanceMeters,#min/100m
                 Speed=(3600/1000)*data$DistanceMeters/data$TotalTimeSeconds)#km/h