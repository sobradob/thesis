# takes raw measurements as input and assigns each to the closest cluster, as well as specifying wether it is wthin accuracy

binMeasurements <- function(data,clustMap){
  library(dbplyr)
  library(tidyr)
  #calculate distances between clusters and the data, then establish which one is closest
  distance <-  raster::pointDistance(data[,c("lon","lat")], clustMap[,c("lon","lat")], longlat = T)
  clust<- apply(distance,MARGIN = 1, FUN = which.min)
  data$distClustMin<- apply(distance,MARGIN = 1, FUN = min)
  data$clust <- clust
  
  return(data)
}