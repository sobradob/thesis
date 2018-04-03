# alternative to LPC

#merge points in clusters
# filter by amount of measurements and accuracy
# do again
# etc. 

#To do: evaluate brussels and how this operaterates on new situations. 

mergePointsRoute <-  function(data, start = start, end = end,d = 400, maxAcc = 90, minVisit = 2){
  
  library(sp)
  library(rgdal)
  library(geosphere)
  
  # Cluster points between two pauses
  toClust<- data %>% 
    filter((nextPauseClust == start &  prevPausClust == end)|
             (nextPauseClust == end &  prevPausClust == start)) %>% 
    select(lon, lat, accuracy) %>% as.matrix()
  
  if(nrow(toClust)<2){return(NULL)}
  # convert data to a SpatialPointsDataFrame object
  xy <- SpatialPointsDataFrame(
    toClust[,1:2], data.frame(ID=seq(1:nrow(toClust))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  # use the distm function to generate a geodesic distance matrix in meters
  mdist <- distm(xy)
  
  # cluster all points using a hierarchical clustering approach
  hc <- hclust(as.dist(mdist), method="complete")
  
  # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
  xy$clust <- cutree(hc, h=d)
  
  xy <- cbind(xy@data,xy@coords,toClust[,"accuracy"])
  colnames(xy)[5] <- "accuracy"
  xy<- xy %>% as.data.frame() %>% 
    group_by(clust) %>% 
    summarise(lon = mean(lon),
              lat = mean(lat),
              visits   = n(),
              mAcc  = mean(accuracy)) %>% 
    filter(mAcc<maxAcc & visits>=minVisit)
  return(xy)
}

