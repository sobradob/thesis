# funcion to group clusters based on distance to each other

mergePoints <-  function(toClust, d = 400){
  
  library(sp)
  library(rgdal)
  library(geosphere)
  
  # convert data to a SpatialPointsDataFrame object
  xy <- SpatialPointsDataFrame(
    toClust, data.frame(ID=seq(1:nrow(toClust))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  cat("In Spatial Data frame Object \n")
  rm(toClust)
  # use the distm function to generate a geodesic distance matrix in meters
  cat("Dist Matrix Init \n")
  mdist <- distm(xy)
  cat("Dist Matrix Complete \n")
  # cluster all points using a hierarchical clustering approach
  hc <- hclust(as.dist(mdist), method="complete")
  cat("Clustered \n")
  # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
  xy$clust <- cutree(hc, h=d)
  cat("Cut  \n")
  xy <- cbind(xy@data,xy@coords)
  xy<- xy %>% as.data.frame() %>% 
    group_by(clust) %>% 
    summarise(lon = mean(lon),
              lat = mean(lat),
              visits   = n())
  return(xy)
}

# funcion to group clusters based on distance to each other

mergePoints2 <-  function(toClust, d = 400){
  
  library(sp)
  library(rgdal)
  library(geosphere)
  
  # convert data to a SpatialPointsDataFrame object
  xy <- SpatialPointsDataFrame(
    toClust[,c("lon","lat")], data.frame(ID=seq(1:nrow(toClust)), type = toClust$type),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  rm(toClust)
  # use the distm function to generate a geodesic distance matrix in meters
  mdist <- distm(xy)
  
  # cluster all points using a hierarchical clustering approach
  hc <- hclust(as.dist(mdist), method="complete")
  
  # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
  xy$clust <- cutree(hc, h=d)
  
  xy <- cbind(xy@data,xy@coords)
  xy<- xy %>% as.data.frame() %>% 
    group_by(clust) %>% 
    summarise(lon = mean(lon),
              lat = mean(lat),
              visits   = n(),
              type = max(type))
  return(xy)
}