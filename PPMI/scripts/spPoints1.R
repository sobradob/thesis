# spacepoints 2
# http://rstudio-pubs-static.s3.amazonaws.com/10685_1f7266d60db7432486517a111c76ac8b.html
CreateSegment <- function(coords, from, to) {
  distance <- 0
  coordsOut <- c()
  biggerThanFrom <- F
  for (i in 1:(nrow(coords) - 1)) {
    d <- sqrt((coords[i, 1] - coords[i + 1, 1])^2 + (coords[i, 2] - coords[i + 
                                                                             1, 2])^2)
    distance <- distance + d
    if (!biggerThanFrom && (distance > from)) {
      w <- 1 - (distance - from)/d
      x <- coords[i, 1] + w * (coords[i + 1, 1] - coords[i, 1])
      y <- coords[i, 2] + w * (coords[i + 1, 2] - coords[i, 2])
      coordsOut <- rbind(coordsOut, c(x, y))
      biggerThanFrom <- T
    }
    if (biggerThanFrom) {
      if (distance > to) {
        w <- 1 - (distance - to)/d
        x <- coords[i, 1] + w * (coords[i + 1, 1] - coords[i, 1])
        y <- coords[i, 2] + w * (coords[i + 1, 2] - coords[i, 2])
        coordsOut <- rbind(coordsOut, c(x, y))
        break
      }
      coordsOut <- rbind(coordsOut, c(coords[i + 1, 1], coords[i + 1, 
                                                               2]))
    }
  }
  return(coordsOut)
}
CreateSegments <- function(coords, length = 0, n.parts = 0) {
  stopifnot((length > 0 || n.parts > 0))
  # calculate total length line
  total_length <- 0
  for (i in 1:(nrow(coords) - 1)) {
    d <- sqrt((coords[i, 1] - coords[i + 1, 1])^2 + (coords[i, 2] - coords[i + 
                                                                             1, 2])^2)
    total_length <- total_length + d
  }
  
  # calculate stationing of segments
  if (length > 0) {
    stationing <- c(seq(from = 0, to = total_length, by = length), total_length)
  } else {
    stationing <- c(seq(from = 0, to = total_length, length.out = n.parts), 
                    total_length)
  }
  
  # calculate segments and store the in list
  newlines <- list()
  for (i in 1:(length(stationing) - 1)) {
    newlines[[i]] <- CreateSegment(coords, stationing[i], stationing[i + 
                                                                       1])
  }
  return(newlines)
}

MergeLast <- function(lst) {
  l <- length(lst)
  lst[[l - 1]] <- rbind(lst[[l - 1]], lst[[l]])
  lst <- lst[1:(l - 1)]
  return(lst)
}

SegmentSpatialLines <- function(sl, length = 0, n.parts = 0, merge.last = FALSE) {
  stopifnot((length > 0 || n.parts > 0))
  id <- 0
  newlines <- list()
  sl <- as(sl, "SpatialLines")
  for (lines in sl@lines) {
    for (line in lines@Lines) {
      crds <- line@coords
      # create segments
      segments <- CreateSegments(coords = crds, length, n.parts)
      if (merge.last && length(segments) > 1) {
        # in case there is only one segment, merging would result into error
        segments <- MergeLast(segments)
      }
      # transform segments to lineslist for SpatialLines object
      for (segment in segments) {
        newlines <- c(newlines, Lines(list(Line(unlist(segment))), ID = as.character(id)))
        id <- id + 1
      }
    }
  }
  return(SpatialLines(newlines))
}

# https://gis.stackexchange.com/questions/153051/waypoints-with-coordinates-in-r

# new path to points function
# takes as input the output of the local curves function
# returns as output the points on the line

spacePointsSP <- function(pathPoints,meter){
  library(rgdal)
  library(rgeos)
  data <- SpatialLines(list(Lines(list(Line(coords = pathPoints)), ID = "1")),proj4string =  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  ## reprojection (utm-32n) and extraction of coordinates
  # MISSING: read into what UTM zone to use
  # I changes something here. do look into it at some point
  dataUTM <- spTransform(data, CRS("+init=epsg:32631"))
  
  dataUTMCoord <- coordinates(dataUTM)[[1]][[1]]
  
  totalLengthM <- gLength(sl2)
  int_len_seq <- seq(0, totalLengthM, meter)
  
  ls_crd_seg <- lapply(2:length(int_len_seq), function(i) {
    
    # extract coordinates of current line segment
    mat_segment <- CreateSegment(dataUTMCoord, 
                                 from = int_len_seq[i-1], to = int_len_seq[i]) #giving an error
    
    # end coordinate
    crd_out <- matrix(mat_segment[nrow(mat_segment), ], ncol = 2)
    
    # during the first iteration, also return the start coordinate
    if (i == 2) {
      crd_start <- matrix(mat_segment[1, ], ncol = 2)
      crd_out <- rbind(crd_start, crd_out)
    } 
    
    return(crd_out)
  })
  
  ## coordinates to SpatialPoints
  crd_seg <- do.call("rbind", ls_crd_seg)
  spt_seg <- SpatialPoints(crd_seg)
  proj4string(spt_seg) <- proj4string(dataUTM)
  data<- spTransform(spt_seg, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  points<- coordinates(data) %>% data.frame()
  colnames(points) <- c("lon","lat")
  return(points)
}
