# impute missing data

# palmius to do: create functions to execute his downsampling and filtering methods
# create functions to execute his imputation algos. 

filteringPalmius <- function(data, n = 10, maxSpeed = 27.7 ){
  
  d<- data
  dPrimeSource<- 100
  dPrimeDest <- 1
  removed <- data.frame(lon = 1, lat = 1)
  
  while(dPrimeSource >= n & dPrimeDest < n){
    
    #calculate distance between points
    d$distancesMeter<- apply(d, 1, FUN = function(row) {
      raster::pointDistance(c(as.numeric(as.character(row["lon"])),
                              as.numeric(as.character(row["lat"]))),
                            c(as.numeric(as.character(row["nextLon"])), as.numeric(as.character(row["nextLat"]))),
                            lonlat = T) # Parameter 'lonlat' has to be TRUE!
    })
    
    #add index and calculate speed between points
    d <- mutate(d,
                speedMeterSecond= distancesMeter/as.numeric(nextTimeSec),
                index = 1:nrow(dataExample)
    )
    
    
    # extract source 
    filteredSource <- d %>% 
      filter(speedMeterSecond >= maxSpeed)
    
    # extract destination
    filteredDestination <- d %>%
      filter(index %in% as.numeric(filteredSource$index+1))
    
    # extract modes
    
    sourceMode<- filteredSource %>% 
      count(lon, lat) %>%
      slice(which.max(n))
    
    destMode<- filteredDestination %>% 
      count(lon, lat) %>%
      slice(which.max(n))
    
    if(sourceMode$n >= destMode$n & sourceMode$n >n){
      d <- filter(d, lon != sourceMode$lon & lat != sourceMode$lat)
      removed <- rbind(removed, sourceMode[,c("lon","lat")])
    } else if(destMode$n >= 10){
      d <- filter(d, lon != destMode$lon & lat != destMode$lat)
      removed <- rbind(removed, sourceMode[,c("lon","lat")])
    }
    
    dPrimeSource<- sourceMode$n
    dPrimeDest <- destMode$n
    
  }
  cat(paste0("Removed ", nrow(removed)-1, " unique locations"))
  return(d)
}
