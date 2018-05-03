NLlatlon2UTM <- function(data){
  data <- SpatialLines(list(Lines(list(Line(coords = data)), ID = "1")),proj4string =  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  library(rgdal)
  library(rgeos)
  ## reprojection (utm-32n) and extraction of coordinates
  # MISSING: read into what UTM zone to use
  # I changes something here. do look into it at some point
  dataUTM <- spTransform(data, CRS("+init=epsg:32631"))
  
  dataUTMCoord <- coordinates(dataUTM)[[1]][[1]]
  return(dataUTMCoord)
}

data <- NLlatlon2UTM(tripMat)
NLutm2LatLon <- function(data){
  library(sp)
  library(rgdal)
  library(rgeos)
  data <- SpatialPoints(data,proj4string = CRS("+init=epsg:32631"))
  data<- spTransform(data, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  points<- coordinates(data) %>% data.frame()
  colnames(points) <- c("lon","lat")
  return(points)
}

#round(NLutm2LatLon(x) - tripMat)

