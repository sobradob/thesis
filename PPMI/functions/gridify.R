# rasterize

gridFunc <- function(test3, minAccuracy = 150, cs = 0.5, d = 300){
  library(raster)
  toClust <- test3 %>% filter(is.na(isPause) & accuracy <=minAccuracy) %>%
    select(lon,lat) %>% 
    as.matrix()
  
  xy <- SpatialPointsDataFrame(
    toClust, data.frame(ID=seq(1:nrow(toClust))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  cat("In Spatial Data frame Object \n")
  
  bb <- bbox(xy)
  myPointsGrid <- GridTopology(cellcentre.offset =  bb[, 1] + (cs/2), cellsize = c(1, 1), cells.dim = ceiling(diff(t(bb))/cs))
  myPointsGrid <- SpatialGrid(myPointsGrid, proj4string = "+proj=longlat +ellps=WGS84 +datum=WGS84")
  res<- cbind(toClust,grid = as.vector(over(xy,myPointsGrid)))
  cat(paste0("Put into ",length(unique(res[,"grid"]))," grids"))
  
  allRoute <- list()
  gridBins<- unique(res[,3])
  i <- 1
  for(grid in gridBins){
    cat(paste0(Sys.time()," doing grid ", i, "\n total rows ", nrow(res[which(res[,3] == grid),1:2])),"\n")
    allRoute[[i]]<- mergePoints(res[which(res[,3] == grid),1:2], d = d)
    i <- i+1
  }
  tBins<- bind_rows(allRoute)
  return(tBins)  
}

